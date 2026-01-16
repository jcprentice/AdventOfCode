library(tidyverse)

message("\n# Day 12")

input <- readLines("2024/day_12-input_ex.txt")
input

N <- length(input)

mat <- input |>
    str_split("") |>
    unlist() |>
    # str_replace_all(setNames(as.character(1:26), LETTERS)) |>
    # as.integer() |>
    matrix(byrow = TRUE, nrow = N)

mat

part1 <- 0
message("- Part1 = ", part1)

part2 <- 0
message("- Part2 = ", part2)
