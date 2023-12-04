# Day 2

[Puzzle](https://adventofcode.com/2023/day/2)

## Thoughts

Unfortunately, most of my effort for today's puzzle was spent wrangling Nix rather than writing Haskell :(
This isn't all bad, as I did hope to use these challenges to improve my understanding of Nix (a long way to go unfortunately) but feeling like such a simple issue (just including `Parsec`) was holding me up did frustrate me.
I still haven't mastered the art of reading through Nix documentation or parsing it's error messages, but perhaps the most frustrating element was the solution. Even after getting my build running successfully I feel like I'm not sure why the fix was necessary. To make the package visible to my project I had to specifically add the package directly with a `callHackageDirect`. I don't understand why this was needed as the package I ended up specifing (`parsec_3_1_17_0`) seems to exist identically as I specified.

As for the actual problems, they were quite simple once I was set up with `Parsec`. Perhaps it was too powerful a tool to leave me with an interesting problem?

Writing the `Semigroup` and `Monoid` instances for `Pull` was fun though!

Oh, the layout for this is a bit janky. Yesterday's puzzle was simple enough that I didn't feel the need to split the project into modules, but moving forward I'm going to. It doesn't really make sense for the `Pull` instances and such to be in the `Parser` module, but whatever!!
