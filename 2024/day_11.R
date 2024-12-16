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

blink <- function(s, blinks = 25) {
    # message(strrep("  ", blinks), s)
    if (blinks <= 0) {
        return(s)
    }

    str_split_1(s, " ") |>
        map_chr(~ {
        if (.x == "0") {
            "1"
        } else {
            N <- str_length(.x)
            if (N %% 2 == 0) {
                c(str_sub(.x, 1, N / 2),
                  str_sub(.x, N / 2 + 1, N)) |>
                    as.numeric() |>
                    paste(collapse = " ")
            } else {
                as.character(as.numeric(.x) * 2024)
            }
        }
    }) |>
        paste(collapse = " ") |>
        blink(blinks - 1)
}

part1 <- blink(input, 25) |>
    str_split_1(" ") |>
    length()


message("- Part1 = ", part1)


part1 <- blink(input, 75) |>
    str_split_1(" ") |>
    length()


part2 <- 0
message("- Part2 = ", part2)
