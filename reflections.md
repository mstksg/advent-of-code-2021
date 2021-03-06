Reflections
===========

<!--
This file generated by the build script at ./Build.hs from the files in
./reflections.  If you want to edit this, edit those instead!
-->

*[2016][]* / *[2017][]* / *[2018][]* / *[2019][]* / *[2020][]* / *2021*

[2016]: https://github.com/mstksg/advent-of-code-2016/blob/master/reflections.md
[2017]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md
[2018]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md
[2019]: https://github.com/mstksg/advent-of-code-2019/blob/master/reflections.md
[2020]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2021

Table of Contents
-----------------

* [Day 1](#day-1)
* [Day 2](#day-2)
* [Day 3](#day-3) *(no reflection yet)*
* [Day 4](#day-4) *(no reflection yet)*
* [Day 5](#day-5) *(no reflection yet)*
* [Day 6](#day-6) *(no reflection yet)*
* [Day 7](#day-7) *(no reflection yet)*
* [Day 8](#day-8) *(no reflection yet)*
* [Day 9](#day-9) *(no reflection yet)*
* [Day 10](#day-10) *(no reflection yet)*
* [Day 11](#day-11) *(no reflection yet)*
* [Day 15](#day-15) *(no reflection yet)*
* [Day 16](#day-16) *(no reflection yet)*

Day 1
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day01.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d01p]* / *[Code][d01g]* / *[Rendered][d01h]* / *[Standalone Reflection Page][d01r]*

[d01p]: https://adventofcode.com/2021/day/1
[d01g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day01.hs
[d01h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day01.html
[d01r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day01.md

As a simple data processing thing, this one shines pretty well in Haskell :)

Assuming we have a list, we can get the consecutive items with a combination of
`zipWith` and `drop`.  Then we can just count how many pairs of items match the
predicate (strictly increasing):

```haskell
countIncreasesPart1 :: [Int] -> Int
countIncreasesPart1 xs = length (filter (== True) (zipWith (<) xs (drop 1 xs)))
```

Yes, `filter (== True)` is the same as `filter id`, but it's a bit easier to
read this way :)

Remember that if `xs` is `[2,4,6,5]`, then `drop 1 xs` is `[4,6,5]`, and so
`zip xs (drop 1 xs)` is `[(2,4), (4,6), (6,5)]`  So `zipWith (<) xs (drop 1
xs)` is `[True, True, False]`.  So counting all of the `True` items yields the
right answer!

Part 2 is very similar, but we need to check if items *three* positions apart
are increasing.  That's because for each window, the sum of the window is
increasing if the new item gained is bigger than the item that was just lost.
So for an example like `[3,5,6,4,7,8]`, as we move from `[3,5,6]` to `[5,6,4]`,
we only need to check if `4` is greater than `3`.  So we only need to compare 4
and 3, 7 and 5, and then 8 and 6.

```haskell
countIncreasesPart2 :: [Int] -> Int
countIncreasesPart2 xs = length (filter (== True) (zipWith (<) xs (drop 3 xs)))
```

We just need to replace `drop 1 xs` with `drop 3 xs` to compare three-away
items.

Anyway the parsing in Haskell is straightforward, at least -- we can just do
`map read . lines`, to split our input into lines and then map `read :: String
-> Int` over each line.  Ta dah!  Fun start to the year :)


### Day 1 Benchmarks

```
>> Day 01a
benchmarking...
time                 26.81 ??s   (26.45 ??s .. 27.29 ??s)
                     0.996 R??   (0.991 R?? .. 1.000 R??)
mean                 26.75 ??s   (26.43 ??s .. 27.57 ??s)
std dev              1.478 ??s   (130.1 ns .. 2.625 ??s)
variance introduced by outliers: 62% (severely inflated)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 27.02 ??s   (25.16 ??s .. 28.74 ??s)
                     0.966 R??   (0.956 R?? .. 0.979 R??)
mean                 26.40 ??s   (25.02 ??s .. 27.78 ??s)
std dev              4.752 ??s   (3.640 ??s .. 6.699 ??s)
variance introduced by outliers: 95% (severely inflated)

* parsing and formatting times excluded
```



Day 2
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day02.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d02p]* / *[Code][d02g]* / *[Rendered][d02h]* / *[Standalone Reflection Page][d02r]*

[d02p]: https://adventofcode.com/2021/day/2
[d02g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day02.hs
[d02h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day02.html
[d02r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day02.md

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


### Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 8.030 ??s   (7.348 ??s .. 8.777 ??s)
                     0.955 R??   (0.936 R?? .. 0.993 R??)
mean                 7.800 ??s   (7.516 ??s .. 8.666 ??s)
std dev              1.719 ??s   (936.6 ns .. 3.161 ??s)
variance introduced by outliers: 97% (severely inflated)

* parsing and formatting times excluded

>> Day 02b
benchmarking...
time                 1.710 ms   (1.616 ms .. 1.830 ms)
                     0.964 R??   (0.929 R?? .. 0.987 R??)
mean                 1.730 ms   (1.673 ms .. 1.792 ms)
std dev              215.9 ??s   (168.1 ??s .. 321.3 ??s)
variance introduced by outliers: 79% (severely inflated)
```



Day 3
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day03.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d03p]* / *[Code][d03g]* / *[Rendered][d03h]* / *[Standalone Reflection Page][d03r]*

[d03p]: https://adventofcode.com/2021/day/3
[d03g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day03.hs
[d03h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day03.html
[d03r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day03.md

*Reflection not yet written -- please check back later!*

### Day 3 Benchmarks

```
>> Day 03a
benchmarking...
time                 1.556 ms   (1.538 ms .. 1.577 ms)
                     0.998 R??   (0.997 R?? .. 0.999 R??)
mean                 1.560 ms   (1.545 ms .. 1.574 ms)
std dev              58.01 ??s   (47.70 ??s .. 76.73 ??s)
variance introduced by outliers: 25% (moderately inflated)

* parsing and formatting times excluded

>> Day 03b
benchmarking...
time                 543.0 ??s   (527.5 ??s .. 568.7 ??s)
                     0.990 R??   (0.977 R?? .. 0.999 R??)
mean                 534.3 ??s   (527.0 ??s .. 551.6 ??s)
std dev              33.62 ??s   (14.55 ??s .. 58.08 ??s)
variance introduced by outliers: 55% (severely inflated)

* parsing and formatting times excluded
```



Day 4
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day04.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d04p]* / *[Code][d04g]* / *[Rendered][d04h]* / *[Standalone Reflection Page][d04r]*

[d04p]: https://adventofcode.com/2021/day/4
[d04g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day04.hs
[d04h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day04.html
[d04r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day04.md

*Reflection not yet written -- please check back later!*

### Day 4 Benchmarks

```
>> Day 04a
benchmarking...
time                 298.3 ??s   (296.0 ??s .. 301.3 ??s)
                     1.000 R??   (0.999 R?? .. 1.000 R??)
mean                 298.2 ??s   (297.3 ??s .. 300.3 ??s)
std dev              4.367 ??s   (2.639 ??s .. 7.182 ??s)

* parsing and formatting times excluded

>> Day 04b
benchmarking...
time                 784.5 ??s   (773.9 ??s .. 805.2 ??s)
                     0.997 R??   (0.994 R?? .. 0.999 R??)
mean                 752.0 ??s   (739.7 ??s .. 773.0 ??s)
std dev              53.03 ??s   (40.49 ??s .. 74.21 ??s)
variance introduced by outliers: 59% (severely inflated)

* parsing and formatting times excluded
```



Day 5
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day05.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d05p]* / *[Code][d05g]* / *[Rendered][d05h]* / *[Standalone Reflection Page][d05r]*

[d05p]: https://adventofcode.com/2021/day/5
[d05g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day05.hs
[d05h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day05.html
[d05r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day05.md

*Reflection not yet written -- please check back later!*

### Day 5 Benchmarks

```
>> Day 05a
benchmarking...
time                 5.197 ms   (5.107 ms .. 5.278 ms)
                     0.997 R??   (0.996 R?? .. 0.999 R??)
mean                 5.349 ms   (5.294 ms .. 5.445 ms)
std dev              217.7 ??s   (188.5 ??s .. 263.7 ??s)
variance introduced by outliers: 21% (moderately inflated)

* parsing and formatting times excluded

>> Day 05b
benchmarking...
time                 23.19 ms   (23.07 ms .. 23.31 ms)
                     1.000 R??   (1.000 R?? .. 1.000 R??)
mean                 23.36 ms   (23.25 ms .. 23.52 ms)
std dev              273.2 ??s   (200.5 ??s .. 368.6 ??s)

* parsing and formatting times excluded
```



Day 6
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day06.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d06p]* / *[Code][d06g]* / *[Rendered][d06h]* / *[Standalone Reflection Page][d06r]*

[d06p]: https://adventofcode.com/2021/day/6
[d06g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day06.hs
[d06h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day06.html
[d06r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day06.md

*Reflection not yet written -- please check back later!*

### Day 6 Benchmarks

```
>> Day 06a
benchmarking...
time                 3.421 ??s   (3.415 ??s .. 3.430 ??s)
                     1.000 R??   (1.000 R?? .. 1.000 R??)
mean                 3.422 ??s   (3.414 ??s .. 3.438 ??s)
std dev              36.50 ns   (17.25 ns .. 65.66 ns)

* parsing and formatting times excluded

>> Day 06b
benchmarking...
time                 3.454 ??s   (3.432 ??s .. 3.475 ??s)
                     1.000 R??   (0.999 R?? .. 1.000 R??)
mean                 3.457 ??s   (3.446 ??s .. 3.477 ??s)
std dev              54.27 ns   (25.71 ns .. 94.82 ns)
variance introduced by outliers: 14% (moderately inflated)

* parsing and formatting times excluded
```



Day 7
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day07.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d07p]* / *[Code][d07g]* / *[Rendered][d07h]* / *[Standalone Reflection Page][d07r]*

[d07p]: https://adventofcode.com/2021/day/7
[d07g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day07.hs
[d07h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day07.html
[d07r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day07.md

*Reflection not yet written -- please check back later!*

### Day 7 Benchmarks

```
>> Day 07a
benchmarking...
time                 410.4 ??s   (407.7 ??s .. 414.0 ??s)
                     1.000 R??   (0.999 R?? .. 1.000 R??)
mean                 409.6 ??s   (408.0 ??s .. 412.3 ??s)
std dev              7.499 ??s   (4.702 ??s .. 12.56 ??s)

* parsing and formatting times excluded

>> Day 07b
benchmarking...
time                 451.6 ??s   (447.5 ??s .. 456.9 ??s)
                     0.999 R??   (0.999 R?? .. 1.000 R??)
mean                 444.5 ??s   (442.3 ??s .. 447.2 ??s)
std dev              8.207 ??s   (6.712 ??s .. 11.79 ??s)

* parsing and formatting times excluded
```



Day 8
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day08.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d08p]* / *[Code][d08g]* / *[Rendered][d08h]* / *[Standalone Reflection Page][d08r]*

[d08p]: https://adventofcode.com/2021/day/8
[d08g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day08.hs
[d08h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day08.html
[d08r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day08.md

*Reflection not yet written -- please check back later!*

### Day 8 Benchmarks

```
>> Day 08a
benchmarking...
time                 28.48 ??s   (28.41 ??s .. 28.55 ??s)
                     1.000 R??   (1.000 R?? .. 1.000 R??)
mean                 28.51 ??s   (28.43 ??s .. 28.62 ??s)
std dev              306.4 ns   (233.2 ns .. 427.0 ns)

* parsing and formatting times excluded

>> Day 08b
benchmarking...
time                 495.5 ??s   (494.3 ??s .. 497.3 ??s)
                     1.000 R??   (1.000 R?? .. 1.000 R??)
mean                 498.1 ??s   (497.1 ??s .. 500.1 ??s)
std dev              5.367 ??s   (4.515 ??s .. 6.275 ??s)

* parsing and formatting times excluded
```



Day 9
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day09.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d09p]* / *[Code][d09g]* / *[Rendered][d09h]* / *[Standalone Reflection Page][d09r]*

[d09p]: https://adventofcode.com/2021/day/9
[d09g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day09.hs
[d09h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day09.html
[d09r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day09.md

*Reflection not yet written -- please check back later!*

### Day 9 Benchmarks

```
>> Day 09a
benchmarking...
time                 1.593 ms   (1.573 ms .. 1.614 ms)
                     0.999 R??   (0.999 R?? .. 1.000 R??)
mean                 1.570 ms   (1.561 ms .. 1.578 ms)
std dev              27.83 ??s   (21.69 ??s .. 34.24 ??s)

* parsing and formatting times excluded

>> Day 09b
benchmarking...
time                 7.241 ms   (7.209 ms .. 7.271 ms)
                     1.000 R??   (0.999 R?? .. 1.000 R??)
mean                 7.314 ms   (7.292 ms .. 7.354 ms)
std dev              82.86 ??s   (58.27 ??s .. 124.0 ??s)

* parsing and formatting times excluded
```



Day 10
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day10.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d10p]* / *[Code][d10g]* / *[Rendered][d10h]* / *[Standalone Reflection Page][d10r]*

[d10p]: https://adventofcode.com/2021/day/10
[d10g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day10.hs
[d10h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day10.html
[d10r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day10.md

*Reflection not yet written -- please check back later!*

### Day 10 Benchmarks

```
>> Day 10a
benchmarking...
time                 66.77 ??s   (60.77 ??s .. 70.87 ??s)
                     0.974 R??   (0.973 R?? .. 0.985 R??)
mean                 60.28 ??s   (58.91 ??s .. 62.63 ??s)
std dev              6.267 ??s   (4.142 ??s .. 8.292 ??s)
variance introduced by outliers: 84% (severely inflated)

* parsing and formatting times excluded

>> Day 10b
benchmarking...
time                 67.56 ??s   (67.48 ??s .. 67.65 ??s)
                     1.000 R??   (1.000 R?? .. 1.000 R??)
mean                 67.64 ??s   (67.51 ??s .. 68.10 ??s)
std dev              776.5 ns   (188.6 ns .. 1.601 ??s)

* parsing and formatting times excluded
```



Day 11
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day11.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d11p]* / *[Code][d11g]* / *[Rendered][d11h]* / *[Standalone Reflection Page][d11r]*

[d11p]: https://adventofcode.com/2021/day/11
[d11g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day11.hs
[d11h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day11.html
[d11r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day11.md

*Reflection not yet written -- please check back later!*

### Day 11 Benchmarks

```
>> Day 11a
benchmarking...
time                 7.176 ms   (7.149 ms .. 7.204 ms)
                     1.000 R??   (1.000 R?? .. 1.000 R??)
mean                 7.130 ms   (7.107 ms .. 7.161 ms)
std dev              70.69 ??s   (57.87 ??s .. 88.48 ??s)

* parsing and formatting times excluded

>> Day 11b
benchmarking...
time                 15.22 ms   (14.34 ms .. 16.29 ms)
                     0.988 R??   (0.981 R?? .. 0.998 R??)
mean                 14.97 ms   (14.69 ms .. 15.47 ms)
std dev              992.2 ??s   (555.8 ??s .. 1.562 ms)
variance introduced by outliers: 31% (moderately inflated)

* parsing and formatting times excluded
```



Day 15
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day15.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d15p]* / *[Code][d15g]* / *[Rendered][d15h]* / *[Standalone Reflection Page][d15r]*

[d15p]: https://adventofcode.com/2021/day/15
[d15g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day15.hs
[d15h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day15.html
[d15r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day15.md

*Reflection not yet written -- please check back later!*

### Day 15 Benchmarks

```
>> Day 15a
benchmarking...
time                 61.55 ms   (60.33 ms .. 62.69 ms)
                     0.999 R??   (0.997 R?? .. 1.000 R??)
mean                 60.96 ms   (60.51 ms .. 61.67 ms)
std dev              1.050 ms   (753.2 ??s .. 1.426 ms)

* parsing and formatting times excluded

>> Day 15b
benchmarking...
time                 2.338 s    (2.307 s .. 2.372 s)
                     1.000 R??   (1.000 R?? .. 1.000 R??)
mean                 2.331 s    (2.316 s .. 2.339 s)
std dev              14.04 ms   (1.157 ms .. 17.83 ms)
variance introduced by outliers: 19% (moderately inflated)

* parsing and formatting times excluded
```



Day 16
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day16.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d16p]* / *[Code][d16g]* / *[Rendered][d16h]* / *[Standalone Reflection Page][d16r]*

[d16p]: https://adventofcode.com/2021/day/16
[d16g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day16.hs
[d16h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day16.html
[d16r]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day16.md

*Reflection not yet written -- please check back later!*

### Day 16 Benchmarks

```
>> Day 16a
benchmarking...
time                 1.420 ms   (1.351 ms .. 1.535 ms)
                     0.977 R??   (0.963 R?? .. 0.995 R??)
mean                 1.452 ms   (1.363 ms .. 1.678 ms)
std dev              418.2 ??s   (84.22 ??s .. 896.7 ??s)
variance introduced by outliers: 97% (severely inflated)

* parsing and formatting times excluded

>> Day 16b
benchmarking...
time                 1.091 ms   (1.051 ms .. 1.145 ms)
                     0.990 R??   (0.986 R?? .. 0.996 R??)
mean                 1.085 ms   (1.061 ms .. 1.111 ms)
std dev              88.42 ??s   (51.04 ??s .. 118.5 ??s)
variance introduced by outliers: 64% (severely inflated)

* parsing and formatting times excluded
```

