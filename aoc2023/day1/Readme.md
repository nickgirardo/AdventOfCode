# Day 1

[Puzzle](https://adventofcode.com/2023/day/1)

## Thoughts

### Part one

Fun easy challenge to get started with the month. I always appreciate an opportunity to use the `(&&&)` combinator lol

In my first pass after filtering on `isDigit`, I immediately converted the Chars to Ints, later using a simple helper to join the digits into a value.
I had wondered if it would be cleaner to keep them as chars, joining them into a string and then reading that string. I implemented this as well when I went to write this up (as `_processLine'`) but ended up thinking it wasn't as nice of a solution as the original

### Part two

I saw a lot of discourse about the second part of this problem being a bit tricky. I assume that this was due to parsing substrings like `"eightwo"` as 8 instead of an 8 followed by a 2 (as the "t" required for the "two" would have been consumed by the "eight").
I happened to see that exact case when I was glancing at my test cases so it wasn't an issue for me, but even if it weren't an issue I'm happy with my approach

In my approach I use the `tails` fn to check if each digit in the given string is the start of a number name or digit. This means that instead of the "eight" in "eightwo" being consumed and leaving only "wo" behind, I transform that into something like "8---2--" where the dashes represent chars that aren't the start of a digit.

I didn't find the function `isPrefixOf` in my brief search, so I implemented a `startsWith` which functioned exactly in the same way. I did replace this in a later commit when I came across this.

Lastly I implemented a helper fn `magicalLookup` (because I wasn't in the mood to make a useful name apparently) which functions similarly to the `lookup` function but takes a predicate instead of using equality checks. I wasn't able to find a standard function which worked identically but it's possible I've missed one
