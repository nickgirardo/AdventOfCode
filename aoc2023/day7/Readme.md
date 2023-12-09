# Day 7

[Puzzle](https://adventofcode.com/2023/day/7)

## Thoughts

### Part one

This was a tiny bit tedious but I generally had fun with this one

One thing to note was that I wrote out some `Enum` typeclasses with this, but I felt that they could be used more powerfully. For instance, it seems like it should be possible to define `Eq` in terms of `Enum` (which would allow one to then derive `Ord` in terms of enum). Obviously one could write:

```
-- For some `Foo` which implements `Enum`
instance Eq Foo
  where
    (==) a b = fromEnum a == fromEnum b
```

as I've done for the related `Ord` typeclass in my program, but this seems like repetitive work that should be able to be saved. There may be some way to achieve this (I'm aware of `DerivingVia`, but have never used it), but not one that I'm aware of.

I did actually produce the wrong answer and struggle for a bit to find my error. I had failed to implement proper sorting behavior for the hands, as in the case where two hands had the same "Major Score" (e.g. both two pair) I was comparing their individual cards as `String`s. This meant that for the letter represented cards `A < J < K < T < Q` instead of `T < J < Q < K < A`. This is the purpose of the `Card` newtype with its `Enum` definition.

I peeked at what is changing for the part two, and it seems like it will be fun to implement as well, although a bit less clean probably
