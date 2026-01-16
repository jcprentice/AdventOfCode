library(tidyverse)
library(memoise)

message("\n# Day 11")

input <- readLines("2024/day_11-input.txt")
input

# If the stone is engraved with the number 0, it is replaced by a stone engraved
# with the number 1.
#
# If the stone is engraved with a number that has an even number of digits, it
# is replaced by two stones. The left half of the digits are engraved on the new
# left stone, and the right half of the digits are engraved on the new right
# stone. (The new numbers don't keep extra leading zeroes: 1000 would become
# stones 10 and 0.)
#
# If none of the other rules apply, the stone is replaced by a new stone; the
# old stone's number multiplied by 2024 is engraved on the new stone.

num_digits <- function(n) floor(log10(n)) + 1

cache <- collections::dict()

blink_count <- function(num, steps) {
    if (steps == 0) {
        return(1)
    }
    if (cache$has(c(num, steps))) {
        return(cache$get(c(num, steps), 1))
    }

    ans <- if (num == 0) {
        blink_count(1, steps - 1)
    } else {
        N <- num_digits(num)
        if (N %% 2 == 0) {
            left <- num %/% 10^(N / 2)
            right <- num %% 10^(N / 2)
            blink_count(left, steps - 1) + blink_count(right, steps - 1)
        } else {
            blink_count(num * 2024, steps - 1)
        }
    }
    cache$set(c(num, steps), ans)
    ans
}

nums <- input |> str_split_1(" ") |> as.numeric()

part1 <- map_dbl(nums, blink_count, 25) |> sum()
message("- Part1 = ", part1)

part2 <- map_dbl(nums, blink_count, 75) |> sum()
message("- Part2 = ", part2)
