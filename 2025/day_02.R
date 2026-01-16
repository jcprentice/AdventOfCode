library(tidyverse)
library(numbers)

message("# Day 02")

# input <- read_lines("2025/day_02-input-ex.txt") |>
input <- read_lines("2025/day_02-input.txt") |>
    str_split_1(",") |>
    str_split("-") |>
    map(as.numeric)

# srep <- function(s, n) sum(10^(seq(0, n - 1) * floor(1 + log10(s))) * s)
srep <- function(x, times) strrep(x, times) |> as.numeric()
digits <- function(x) floor(log10(x)) + 1
inrange <- function(x, rng) rng[[1]] <= x && x <= rng[[2]]

val1 <- map_dbl(input, \(x) {
    tens <- 10^floor(log10(x[[1]]) / 2 + 1)
    rng <- pmax(x %/% tens, 1)
    seq(rng[[1]], rng[[2]]) |>
        strrep(2) |>
        as.numeric() |>
        keep(inrange, x) |>
        sum()
}) |> sum()

message(" - answer = ", val1)

# What's the smallest (greatest) number that is >= x[1] (<= x[2]) when repeated
# n times?
smallest <- function(x, n) {
    xm <- min(x)
    d <- digits(xm)
    if (d %% n == 0) {
        x1 <- get_digits(xm, d / n)
        if (srep(x1, n) < xm) x1 + 1 else x1
    } else {
        10^(max(ceiling(d / n) - 1, 0))
    }
}

greatest <- function(x, n) {
    xm <- max(x)
    d <- digits(xm)
    if (d %% n == 0) {
        x1 <- get_digits(xm, d / n)
        if (srep(x1, n) > xm) x1 - 1 else x1
    } else {
        10^(max(ceiling(d / n) - 1, 0)) - 1
    }
}

# Test
map_lgl(input, \(x) {
    c(map_lgl(1:5, \(n) srep(smallest(x, n), n) >= x[[1]]),
      map_lgl(1:5, \(n) srep(greatest(x, n), n) <= x[[2]])) |> all()
}) |> all()


# longest repeated string = digits(x[[2]]) %/% 2 e.g. 11 is 5
val2 <- map_dbl(input, \(x) {
    ns <- seq(2, digits(x[[2]]))
    map(ns, \(n) {
        a <- smallest(x, n)
        b <- greatest(x, n)
        if (b < a) return(NULL)
        seq(a, b) |> srep(n) |> keep(inrange, x)
    }) |>
        unlist() |> unique() |> sum()
}) |> sum()

message(" - answer = ", val2)

# Correct example answer is 4174379265
# Correct answer is 14582313461
