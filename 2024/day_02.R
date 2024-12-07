library(tidyverse)

message("\n# Day 02")

input <- readLines("day_02-input.txt") |>
    map(\(x) str_split_1(x, " ") |> as.integer())

is_safe <- function(x) {
    y <- diff(x)
    monotonic <- all(y > 0) || all(y < 0)
    abs_y <- abs(y)
    in_range <- all(1 <= abs_y & abs_y <= 3)
    monotonic && in_range
}

num_safe1 <- map_lgl(input, is_safe) |> sum()

message(" - ", num_safe1, " reports are safe.")


num_safe2 <- map_lgl(input, \(x) {
    map_lgl(seq_along(x), \(i) {
        is_safe(x[-i])
    }) |> any()
}) |> sum()

message(" - ", num_safe2, " reports are safe.")

