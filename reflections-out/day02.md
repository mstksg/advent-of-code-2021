Day 2
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day02.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *2*

[reflections]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day01.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2021

*[Prompt][d02p]* / *[Code][d02g]* / *[Rendered][d02h]*

[d02p]: https://adventofcode.com/2021/day/2
[d02g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day02.hs
[d02h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day02.html

Day 2 has a satisfying "unified" solution for both parts that can be derived
from group theory!  The general group (or monoid) design pattern that I've gone
over [in many Advent of Code blog posts][day2-monoid-posts] is that we can
think of our "final action" as simply a "squishing" of individual smaller
actions.  The revelation is that our individual smaller actions are
"combinable" to yield something of the *same type*, so solving the puzzle is
generating all of the smaller actions repeatedly combining them to yield the
final action.

[day2-monoid-posts]: https://blog.jle.im/entries/series/+advent-of-code.html

In both of these parts, we can think of squishing a bunch of small actions
(`forward`, `up`, `down`) into a mega-action, which represents the final trip
as one big step.  So here is our general solver:

```haskell
-- | A type for x-y coordinates/2d vectors
data Point = P { pX :: Int, pY :: Int }

day02
    :: Monoid r
    => (Int -> r)            -- ^ construct a forward action
    -> (Int -> r)            -- ^ construct an up/down action
    -> (r -> Point -> Point) -- ^ how to apply an action to a point
    -> String
    -> Point                 -- ^ the final point
day02 mkForward mkUpDown applyAct =
      (`applyAct` P 0 0)             -- get the answer by applying from 0,0
    . foldMap (parseAsDir . words)   -- convert each line into the action and merge
    . lines                          -- split up lines
  where
    parseAsDir (dir:n:_) = case dir of
        "forward" -> mkForward amnt
        "down"    -> mkUpDown amnt
        "up"      -> mkUpDown (-amnt)
      where
        amnt = read n
```

And there we have it!  A solver for both parts 1 and 2.  Now we just need to
pick the Monoid :)

For part 1, the choice of monoid is simple: our final action is a translation
by a vector, so it makes sense that the intermediate actions would be vectors as
well -- composing actions means adding those vectors together.

```haskell
data Vector = V { dX :: Int, dY :: Int }

instance Semigroup Vector where
    V dx dy <> V dx' dy' = V (dx + dx') (dy + dy')
instance Monoid Vector where
    mempty = V 0 0

day02a :: String -> Int
day02a = day02
    (\dx -> V dx 0)   -- forward is just a horizontal vector
    (\dy -> V 0 dy)   -- up/down is a vertical vector
    (\(V dx dy) (P x0 y0) -> P (x0 + dx) (y0 + dy))
```

Part 2 is a little trickier because we have to keep track of dx, dy *and* aim.
So we can think of our action as manipulating a `Point` as well as an `Aim`,
and combining them together.

```haskell
newtype Aim = Aim Int

instance Semigroup Aim where
    Aim a <> Aim b = Aim (a + b)
instance Monoid Aim where
    mempty = Aim 0
```

So our "action" looks like:

```haskell
data Part2Action = P2A { p2Vector :: Vector, p2Aim :: Aim }
```

However, it's not exactly obvious how to turn this into a monoid.  How do we
combine two `Part2Action`s to create a new one, in a way that respects the
logic of part 2?  Simply adding them point-wise does not do the trick, because
we have to somehow also get the `Aim` to factor into the new y value.

Group theory to the rescue!  Using the [monoid-extras][] library, we can
can say that `Aim` encodes a "vector transformer".  Applying an aim means adding
the dy value by the aim value multiplied the dx component.

[monoid-extras]: https://hackage.haskell.org/package/monoid-extras-0.6.1

```haskell
instance Action Aim Vector where
    act (Aim a) = moveDownByAimFactor
      where
        moveDownByAimFactor (V dx dy) = V dx (dy + a * dx)
```

Because of this, we can now pair together `Vector` and `Aim` as a [semi-direct
product][]: If we pair up our monoid (`Vector`) with a "point transformer"
(`Aim`), then `Semi Vector Aim` is a monoid that contains both (like our
`Part2Action` above) but also provides a `Monoid` instance that "does the right
thing" (adds vector, adds aim, and also makes sure the aim action gets applied
correctly when adding vectors) thanks to group theory.

[semi-direct product]: https://hackage.haskell.org/package/monoid-extras-0.6.1/docs/Data-Monoid-SemiDirectProduct.html

```haskell
-- constructors/deconstructors that monoid-extras gives us
inject :: Vector -> Semi Vector Aim
embed  :: Aim    -> Semi Vector Aim
untag  :: Semi Vector Aim -> Vector

day02b :: String -> Int
day02b = day02
    (\dx -> inject $ V dx 0)   -- forward just contributs a vector motion
    (\a  -> embed  $ Aim a )   -- up/down just adjusts the aim
    (\sdp (P x0 y0) ->
        let V dx dy = untag sdp
        in  P (x0 + dx) (y0 + dy)
    )
```

And that's it, we're done, thanks to the power of group theory!  We identified
that our final monoid must somehow contain both components (`Vector`, and
`Aim`), but did not know how the two could come together to form a mega-monoid
of both.  However, because we saw that `Aim` also gets accumulated while also
acting as a "point transformer", we can describe how it transforms points (with
the `Action` instance) and so we can use `Semi` (semi-direct product) to encode
our action with a `Monoid` instance that does the right thing.

What was the point of this?  Well, we were able to unify both parts 1 and 2 to
be solved in the same overall method, just by picking a different monoid for
each part.  With only two parts, it might not seem that worth it to abstract,
but maybe if there were more we could experiment with what other neat monoids
we could express our solution as!  But, a major advantage we reap now is that,
because each action combines into other actions (associatively), we could do
all of this in parallel!  If our list of actions was very long, we could
distribute the work over multiple cores or computers and re-combine like a
map-reduce.  There's just something very satisfying about having the "final
action" be of the same type as our intermediate actions.  With that
revelation, we open the door to the entire field of monoid-based optimizations
and pre-made algorithms (like `Semi`)

(Thanks to `mniip` in libera irc's *#adventofcode* channel for helping me
express this in terms of a semi-direct product! My original attempt used a 4x4
matrix that ended up doing the same thing after some symbolic analysis.)

(Thanks too to \@lysxia on twitter for pointing out a nicer way of interpreting
the action in terms of how it acts on points!)


*[Back to all reflections for 2021][reflections]*

## Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 8.030 μs   (7.348 μs .. 8.777 μs)
                     0.955 R²   (0.936 R² .. 0.993 R²)
mean                 7.800 μs   (7.516 μs .. 8.666 μs)
std dev              1.719 μs   (936.6 ns .. 3.161 μs)
variance introduced by outliers: 97% (severely inflated)

* parsing and formatting times excluded

>> Day 02b
benchmarking...
time                 1.710 ms   (1.616 ms .. 1.830 ms)
                     0.964 R²   (0.929 R² .. 0.987 R²)
mean                 1.730 ms   (1.673 ms .. 1.792 ms)
std dev              215.9 μs   (168.1 μs .. 321.3 μs)
variance introduced by outliers: 79% (severely inflated)
```

