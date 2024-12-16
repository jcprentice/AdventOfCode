library(tidyverse)
library(matricks)

message("\n# Day 08")

input <- readLines("2024/day_08-input.txt")
input

N <- length(input)

mat <- input |>
    str_split("") |>
    unlist() |>
    matrix(byrow = TRUE, nrow = N)

mod1 <- function(n, d = N) as.integer(((n - 1) %% d) + 1)
div1 <- function(n, d = N) as.integer(ceiling(n / d))
i2yx <- function(i, d = N) c(y = mod1(i, d), x = div1(i, d))
yx2i <- function(yx, d = N) (yx[["x"]] - 1L) * N + yx[["y"]]

get_pairs <- function(ids) {
    expand.grid(a = ids, b = ids) |>
        filter(a < b) |>
        apply(1, as.list) |>
        map(unlist) |>
        map(unname)
}

get_nodes <- function(pair) {
    A <- i2yx(pair[[1]], N)
    B <- i2yx(pair[[2]], N)
    delta <- B - A
    list(B + delta, A - delta)
}

antennae <- setdiff(unique(c(mat)), ".")

part1 <- antennae |>
    map_dfr(~ which(mat[] == .x) |>
                get_pairs() |>
                map(get_nodes) |>
                list_flatten()) |>
    unique() |>
    filter(1 <= x, x <= N, 1 <= y, y <= N) |>
    nrow()

message(" - Part 1 = ", part1)

gcd <- function(x, y) {
    x1 <- max(abs(x), abs(y))
    y1 <- min(abs(x), abs(y))
    r <- x1 %% y1
    if (r) gcd(y1, r) else y1
}

get_nodes2 <- function(pair) {
    A <- i2yx(pair[[1]], N)
    B <- i2yx(pair[[2]], N)

    delta <- unname(B - A)
    delta <- delta / gcd(delta[[1]], delta[[2]])

    xs <- c((rep(N + 1, 2) - A) / delta,
            (rep(0, 2) - A) / delta) |> trunc()

    map(seq(min(xs), max(xs)), ~ A + .x * delta)
}

part2 <- antennae |>
    map_dfr(~ which(mat[] == .x) |>
                get_pairs() |>
                map(get_nodes2) |>
                list_flatten()) |>
    unique() |>
    filter(1 <= x, x <= N, 1 <= y, y <= N) |>
    nrow()

message(" - Part 2 = ", part2)

