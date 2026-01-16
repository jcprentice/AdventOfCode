library(tidyverse)
library(magrittr)

message("# Day 01")

# input <- read_lines("2025/day_01-input-ex.txt")
input <- read_lines("2025/day_01-input.txt") |>
    str_replace_all(c("L" = "-", "R" = "+")) |>
    as.integer()

val1 <- c(50L, input) |> cumsum() |> mod(100L) |> equals(0L) |> sum()

message(" - answer = ", val1)

pos <- c(50L, input) |> cumsum()
val2 <- ceiling(0.5 * sum(
    abs(diff(pos %/% 100L)) + abs(diff((pos - 1L) %/% 100L))
)) |> as.integer()

message(" - answer = ", val2)

