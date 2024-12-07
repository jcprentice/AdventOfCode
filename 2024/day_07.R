library(tidyverse)

message("\n# Day 07")

input <- readLines("2024/day_07-input.txt")
input

find_match <- function(goal, xs) {
    f <- function(rest, acc = 0) {
        if (length(rest) == 0) return(acc == goal)

        f(rest[-1], acc + rest[[1]]) ||
            f(rest[-1], acc * rest[[1]])
    }
    f(xs)
}

part1 <- map_dbl(input, \(x, i) {
    goal <- x |> str_split_i(": ", 1) |> as.numeric()
    xs <- x |> str_split_i(": ", 2) |> str_split_1(" ") |> as.numeric()

    find_match(goal, xs) * goal
}) |> sum()

message(" - Part 1 = ", part1)


# Turn 123 || 45 into 12345
ncat  <- function(x, y) x * 10^(floor(log10(y)) + 1) + y

find_match2 <- function(goal, xs) {
    f <- function(rest, acc = 0) {
        if (length(rest) == 0) return(acc == goal)

        f(rest[-1], acc + rest[[1]]) ||
            f(rest[-1], acc * rest[[1]]) ||
            f(rest[-1], ncat(acc, rest[[1]]))
    }
    f(xs)
}

part2 <- imap_dbl(input, \(x, i) {
    goal <- x |> str_split_i(": ", 1) |> as.numeric()
    xs <- x |> str_split_i(": ", 2) |> str_split_1(" ") |> as.numeric()

    # message("Input ", i, ": goal = ", goal)
    find_match2(goal, xs) * goal
}, .progress = TRUE) |> sum()

message(" - Part 2 = ", part2)
