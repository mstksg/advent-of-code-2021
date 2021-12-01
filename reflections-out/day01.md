Day 1
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day01.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *1*

[reflections]: https://github.com/mstksg/advent-of-code-2021/blob/master/reflections.md


[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2021

*[Prompt][d01p]* / *[Code][d01g]* / *[Rendered][d01h]*

[d01p]: https://adventofcode.com/2021/day/1
[d01g]: https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day01.hs
[d01h]: https://mstksg.github.io/advent-of-code-2021/src/AOC.Challenge.Day01.html

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


*[Back to all reflections for 2021][reflections]*

## Day 1 Benchmarks

```
>> Day 01a
benchmarking...
time                 26.81 μs   (26.45 μs .. 27.29 μs)
                     0.996 R²   (0.991 R² .. 1.000 R²)
mean                 26.75 μs   (26.43 μs .. 27.57 μs)
std dev              1.478 μs   (130.1 ns .. 2.625 μs)
variance introduced by outliers: 62% (severely inflated)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 27.02 μs   (25.16 μs .. 28.74 μs)
                     0.966 R²   (0.956 R² .. 0.979 R²)
mean                 26.40 μs   (25.02 μs .. 27.78 μs)
std dev              4.752 μs   (3.640 μs .. 6.699 μs)
variance introduced by outliers: 95% (severely inflated)

* parsing and formatting times excluded
```

