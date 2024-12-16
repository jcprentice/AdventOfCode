library(tidyverse)
library(matricks)

message("\n# Day 10")

input <- readLines("2024/day_10-input.txt")
input

N <- length(input)

mat <- input |>
    str_split("") |>
    unlist() |>
    as.integer() |>
    matrix(byrow = TRUE, nrow = N)

mod1 <- function(n, d = N) as.integer(((n - 1) %% d) + 1)
div1 <- function(n, d = N) as.integer(ceiling(n / d))
i2yx <- function(i, d = N) c(y = mod1(i, d), x = div1(i, d))
i2yx <- function(i, d = N) as.integer(c(y = ((n - 1) %% d) + 1,
                                        x = ceiling(n / d)))
yx2i <- function(yx, d = N) (yx[["x"]] - 1L) * N + yx[["y"]]

dirs <- list(c(-1L, +0L), c(+1L, +0L), c(+0L, -1L), c(+0L, +1L))

inbounds <- function(pos) all(1 <= pos) && all(pos <= N)

search_trail <- function(loc, height = 0) {
    if (height == 9L) {
        return(yx2i(loc))
    }
    # message(strrep("    ", height), "[", loc[[1]], ",", loc[[2]], "] (", at(mat, loc), ")")
    map(dirs, \(dir) {
        loc2 <- loc + dir
        if (inbounds(loc2) && at(mat, loc2) == height + 1L) {
            search_trail(loc2, height + 1L)
        } else {
            NULL
        }
    }) |> unlist()
}

trailheads <- which(mat[] == 0)

part1 <- trailheads |>
    map(i2yx) |>
    map(\(i) search_trail(i) |>
            unique() |>
            length()) |>
    unlist() |>
    sum()

message("- Part1 = ", part1)


part2 <- trailheads |>
    map(i2yx) |>
    map(\(i) search_trail(i) |>
            length()) |>
    unlist() |>
    sum()

message("- Part2 = ", part2)
