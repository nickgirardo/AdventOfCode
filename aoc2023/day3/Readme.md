# Day Three

[Puzzle](https://adventofcode.com/2023/day/3)

## Thoughts

This one was a bit of a trainwreck!

In general, Haskell doesn't feel like it wants you do be doing a ton of random access on it's lists (they are linked lists after all). I decided to represent the elements of the field as datatype `Positioned a` which is just a tuple `(x, y, a)` where `x` and `y` are the position in the original field and `a` is the original value. I'm not really sure about this approach, it did make some things easier but very often felt quite unwieldy.

### `chunkRuns`

Of all the myriad helper functions I wrote to make dealing with these objects even somewhat ergonomic, the most convoluted by far is `chunkRuns`. `chunkRuns` was meant to group together digits which followed each other to make a full number, as such:

```
-- Original field example:
{-
323----
----453
-}

-- Digits as `Positioned`
[(0, 0, 3), (1, 0, 2), (2, 0, 3), (5, 1, 4), (6, 1, 5), (7, 1, 3)]

-- Ideal chunks
[
  [(0, 0, 3), (1, 0, 2), (2, 0, 3)],
  [(5, 1, 4), (6, 1, 5), (7, 1, 3)]
]
```

My initial attempt at this used `groupBy` and the predicate checked if the `x` value of one element was just after the next. Unfortunately, this isn't an equivalence relation so I ended up with chunks that looks like:

```
-- Initial chunking attempt
[
  [(0, 0, 3), (1, 0, 2)],
  [(2, 0, 3)],
  [(5, 1, 4), (6, 1, 5)],
  [(7, 1, 3)]
]
```

Note that the first two elements were correctly chunked, but as the third element is two off of the first instead of one off, it was not included.

The approach that I took which ended up working sucessfully is as follows:

- Calculate `starters`, the elements at the start of a chunk. This is fairly simple as it's just all the positioned elements for whom you can't find anything directly before them. Note that `allPos` below is after the field is filtered to only digit elements

```
{-
    starters allPos = filter (not . hasPreceding allPos) allPos
    hasPreceding ps (Positioned (x, y, _)) = hasAt (x - 1, y) ps
-}
```

- For each starter, add an element to the chunk if one exists at `(x + n, y)` where `x` and `y` are the x and y position of the starter and `n` is an iteration value starting at 0
- Repeat until there isn't a valid element at `(x + n, y)`

While this works, it feel very imperative to me

Another approach that I thought of after writing this program works as following

- Calculate `starters` as before
- Group each positioned element by its preceding `starter`

This should theoretically work and probably be a bit cleaner, but I really don't want to return to this problem to improve my solution lol. Furthermore, the fact that a function like this was both needed and difficult to write is probably indicitive of larger issues with my approach

### Other helpers

While everything did come together in the end, a lot of the helper functions did very similar but distinct things and I at times struggled to remember which helper did exactly what. Some examples of confusingly similar functions include

- `chunkRuns` and `chunkNeighbors`
- `neighbors`, `positionNeighbors`, and `chunkNeighbors`
- `getPosition`, `fromPositioned`, and `findPositionedAt`
- `findPositionedAt`, `isAt`, and `hasAt`

With `chunkRuns` pointing to more macro issues with my design, the confusing nature of these groups leads me to conclude that the design had issues on the micro scale as well.

### Bright spots

To it's credit, there were things which this design did make painless and easy.

The entire raison d'etre of the design was being able to filter the given field without "forgetting" where the elements were originally positioned. For example, I could just look at the numbers by filtering all positioned elements on `isDigit`. While `chunkRuns` was awkward and unwieldy, this made it somewhat reasonable. When I received the instructions for part two and realized that "\*" were important, filtering for them was similarly easy.

Looking things up in the list (which if I were concerned about performance for this at all would give me nightmares) was quite simple. One point of note I'd like to specifically highlight was the ease of finding the neighbors. A simple fn `neighbors` returns all position tuples associated with a `Positioned` element (an example of confusing mixes of (x,y) tuples and `Positioned a`). It's source is below:

```
-- | Returns all positions neighboring a given positioned element
neighbors :: Positioned a -> [(Int, Int)]
neighbors (Positioned (x, y, _)) =
    [ (x + 1, y)
    , (x -1, y)
    , (x, y + 1)
    , (x, y -1)
    , (x + 1, y + 1)
    , (x + 1, y -1)
    , (x -1, y + 1)
    , (x -1, y -1)
    ]
```

Ignoring that my formatter seems to enjoy rewriting `x - 1` as `x -1`... Note that neither this function nor the users of its output need to do any bounds checking. This is because looking for an element at (-1, -1) is totally fine and would just return `Nothing` instead of some weird bounds issue.

One last bright spot is that since my solution to part one had me build up a base which was strong (if difficult to wrap one's head around) part two was pretty much trivial
