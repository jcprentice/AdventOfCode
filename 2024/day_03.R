library(tidyverse)

message("\n# Day 03")

input <- readLines("day_03-input.txt")

part1 <- input |>
    str_extract_all("mul\\(\\d+,\\d+\\)") |>
    unlist() |>
    map_int(\(x) {
        x |>
            str_extract_all("\\d+") |>
            unlist() |>
            as.integer() |>
            prod()
    }) |>
    sum()

message(" - Part 1 = ", part1)


process <- function(x, m = 1, acc = 0) {
    if (length(x) == 0) return(acc)
    x1 <- x[[1]]
    if (x1 %in% 0:1) {
        m <- x1
        x1 <- 0
    }
    Tailcall(process, x[-1], m, acc + m * x1)
}

part2 <- input |>
    str_extract_all("mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)") |>
    unlist() |>
    map_int(\(x) {
        if (x == "don't()") return(0)
        else if (x == "do()") return(1)
        x |>
            str_extract_all("\\d+") |>
            unlist() |>
            as.integer() |>
            prod()
    }) |>
    process()

message(" - Part 2 = ", part2)
