# Day Five

[Puzzle](https://adventofcode.com/2023/day/5)

## Thoughts

### Part one

This is the first day for which I am writing the notes immediately upon doing part one and not recounting my memories.

Part one of this puzzle was quite simple, after parsing the data a simple fold over the maps was able to give me the values for each seed, and a simple `minimum` call did all the rest. The simplicity of this puzzle makes sense after seeing how the puzzle was complicated by the instructions for part two.

I sort of over-parsed the input for this puzzle (which I've done to a lesser extent in other puzzles). My natural inclination towards not throwing away any data left the signature for a `Map` looking like

```
type Map = (String, [MapEntry])
```

instead of just

```
type Map = [MapEntry]
```

The first tuple element, the string, is the name of the map (e.g. "seed-to-soil"). I was pretty sure this would continue to just serve as flavor text after part two was revealed, but I decided to keep it anyway. This ended up making my code a bit less elegant:

```
evalMap :: Int -> Map -> Int
evalMap n = evalMapEntries . snd
  where
    evalMapEntries ranges = maybe n getDestination (find isInRange ranges)
```

could have been

```
evalMap :: Int -> Map -> Int
evalMap n = maybe n getDestination . find isInRange
```

### Part two plan

I'm about to start part two, and I have a reasonable idea of a plan, but for now I'm not going to do anything except update the parser, since this does look tricky.
