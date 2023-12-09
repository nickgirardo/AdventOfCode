# Day 9

[Puzzle](https://adventofcode.com/2023/day/9)

## Thoughts

Much like puzzle 6, this puzzle felt like much more of a mathematical test than a test of programming, which I don't mind. I do like that it was optional to solve the puzzle the way I did (finding a closed form solution), and I think just solving the puzzle as described would have been fine as well

It took a ton of fiddling around to find an equation for part one. I found the [triangular numbers](https://en.wikipedia.org/wiki/Triangular_number) and twisted my brain around how they could be extended into higher orders, when I noticed a pattern in their representation in terms of the binomial coefficient. The triangular number n could be represented as `n+1 choose 2`, the [tetrahedral numbers](https://en.wikipedia.org/wiki/Tetrahedral_number) could be represented as `n+2 choose 3`, the [pentatope numbers](https://en.wikipedia.org/wiki/Pentatope_number) `n+3 choose 4`. From here I was able to produce a solution

Part two was fairly similar. My original solution mostly worked, except that I would have had to evaluate `-1 choose n`, and my binomial coefficient was only defined for nats. This wasn't much of an issue though, as after fiddling around in wolfram alpha it seems like the `-1 choose n` when extended to complex numbers is defined when `n` is an integer. Specifically, `-1 choose n = 1` for 0, positive even n, and negative odd n and `-1 choose n = -1` otherwise. As I didn't care about negative `n` this was further simplifed to `-1 choose n = 1` for even n and `-1` for odd n. While I don't understand exactly how this function is extended, this actually made my part two solution simpler than part one!
