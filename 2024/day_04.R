library(tidyverse)
library(matricks)

message("\n# Day 04")

input <- readLines("2024/day_04-input.txt")
input

find_xmas <- function(mat) {
    n <- dim(mat)[[1]]
    xmas <- c("X", "M", "A", "S")
    f <- function(pos = 1, idx = c(1, 1), dir = c(1, 0)) {
        if (any(idx < 1) || any(idx > n)) return(FALSE)
        if (at(mat, idx) != xmas[[pos]]) return(FALSE)

        if (pos == 4) return(TRUE)
        else return(f(pos + 1, idx + dir, dir))
    }

    dirs <- expand.grid(v = -1:1, h = -1:1)
    idxs <- expand.grid(x = 1:n, y = 1:n)

    map_int(seq_len(nrow(dirs)), \(i) {
        map_lgl(seq_len(nrow(idxs)), \(j) {
            f(1, idx = as.numeric(idxs[j, ]), dir = as.numeric(dirs[i, ]))
        }) |> sum()
    }) |> sum()
}

part1 <- input |>
    str_split("") |>
    unlist() |>
    matrix(byrow = TRUE,
           nrow = length(input)) |>
    find_xmas()

message(" - Part 1 = ", part1)


find_x_mas <- function(mat) {
    n <- dim(mat)[[1]]

    f <- function(idx = c(2, 2)) {
        if (at(mat, idx) != "A") {
            return(FALSE)
        }

        diag1 <- c(at(mat, idx + c(-1, -1)), at(mat, idx + c(+1, +1)))
        diag2 <- c(at(mat, idx + c(+1, -1)), at(mat, idx + c(-1, +1)))
        is_xmas <- "M" %in% diag1 && "M" %in% diag2 && "S" %in% diag1 && "S" %in% diag2
        return(is_xmas)
    }

    idxs <- expand.grid(x = seq(2, n-1), y = seq(2, n-1))

    map_lgl(seq_len(nrow(idxs)), \(i) {
        f(idx = as.numeric(idxs[i, ]))
    }) |> sum()
}

part2 <- input |>
    str_split("") |>
    unlist() |>
    matrix(byrow = TRUE,
           nrow = length(input)) |>
    find_x_mas()
message(" - Part 2 = ", part2)

