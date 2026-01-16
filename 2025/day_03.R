library(tidyverse)

message("# Day 03")

# input <- read_lines("2025/day_03-input-ex.txt")
input <- read_lines("2025/day_03-input.txt")

to_dec <- function(x) sum(x * 10^rev(seq_along(x) - 1))

get_sub <- function(x, n = 2) {
    x1 <- x |> str_split_1("") |> as.integer()

    lx1 <- length(x1)
    k <- 0
    map_int(1:n, \(i) {
        k <<- k + which.max(x1[seq(k + 1, lx1 - n + i)])
        x1[[k]]
    }) |> to_dec()
}

val1 <- map_dbl(input, get_sub, 2) |> sum()

# 17085
message(" - answer = ", val1)

val2 <- map_dbl(input, get_sub, 12) |> sum()

# 169408143086082
message(" - answer = ", sum(val2))
