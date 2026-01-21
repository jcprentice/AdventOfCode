library(tidyverse)

message("# Day 05")

# input <- read_lines("2025/day_05-input-ex.txt")
input <- read_lines("2025/day_05-input.txt")

mid <- which(input == "")
ranges <- input[seq(1, mid - 1)] |> unique() |> str_split("-") |> map(as.numeric)
items <- input[seq(mid + 1, length(input))] |> as.numeric() |> sort()

inrange <- function(x, rng) rng[[1]] <= x && x <= rng[[2]]

val1 <- map_lgl(items, \(i) {
    map_lgl(ranges, \(rng) inrange(i, rng)) |> any()
}) |> sum()

message(" - answer = ", val1)

# First create an Nx2 matrix of ranges, then sort by the lower ends
rmat <- matrix(unlist(ranges), byrow = TRUE, ncol = 2)
mat <- rmat[order(rmat[, 1]), ]

reduce_range <- function(mat, i = 2) {
    if (i > dim(mat)[[1]]) {
        return(mat)
    }

    m1 <- mat[[i - 1, 2]]
    m2 <- mat[[i, 1]]

    if (i > 1 && m1 + 1 >= m2) {
        mat2 <- matrix(c(mat[-i, 1], mat[1 - i, 2]), ncol = 2)
        # Be careful here
        mat2[i - 1, 2] <- max(mat[c(i, i-1), 2])
        print(mat2)
        j <- i
    } else {
        mat2 <- mat
        j <- i + 1
    }

    Tailcall(reduce_range, mat2, j)
}
mat2 <- reduce_range(mat)

val2 <- sum(mat2[, 2] - mat2[, 1] + 1)

# 344486348901788
message(" - answer = ", val2)
