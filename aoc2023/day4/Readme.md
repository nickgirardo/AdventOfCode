# Day Four

[Puzzle](https://adventofcode.com/2023/day/4)

## Thoughts

Once again I broke out `Parsec` for this and found it fairly straightforward

### Part one

I found this to be the simplest challenge since day one. In fact once the parser was written it was probably even simpler.

The parsing for this was simple enough that I considered not using parsec at all, just splitting on characters and using `words`. Perhaps if the previous day's puzzle didn't scare me away from doing janky ad-hoc stuff I would have :-)

This puzzle also felt like it followed a bit of a different structure to the past two days. In both of those days the structure was fairly similar, so after I put in the effort into solving part one, part two was only a minor complication. This puzzle instead feels like part one was just making sure you had the basics down before adding a fairly major complication in part two

### Part two

In part one the values of the cards were independent of all of the other cards in the list. In part two, the value of a card is based on the other cards which follow it. This is a significant complication as this becomes recursive. Fortunately the input size is small enough that even though I'm taking a bit of a caveman approach and just accepting the poor performance it's fine.

Even though I solved the problem, I don't think I can be ok with the fact that I'm recursively recalculating these values so many times. I've got a strategy for significantly reducing the work to be done in the recursive step, which I will update this to include after doing so...
