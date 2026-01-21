library(tidyverse)

message("# Day 07")

# input <- read_lines("2025/day_07-input-ex.txt")
input <- read_lines("2025/day_07-input.txt")

rows <- length(input)
cols <- nchar(input[[1]])

mat <- input |> str_split("") |> unlist() |>
    matrix(rows, cols, byrow = TRUE)

paths <- matrix(0, rows, cols)
paths[1:2, which(mat[1, ] == "S")] <- 1

nsplits <- 0
for (i in seq(2, rows - 1)) {
    beams <- paths[i, ]
    next_row <- mat[i + 1, ]
    pass <- beams * (next_row == ".")
    hits <- (beams > 0) & (next_row != ".")

    nsplits <- nsplits + sum(hits)
    splits <- hits * beams
    bsplits <- lag(splits, 1, FALSE) + lead(splits, 1, FALSE)
    paths[i + 1, ] <- pass + bsplits
}
# apply(mat, 1, str_flatten) |> cat(sep = "\n")

val1 <- nsplits

message(" - answer = ", val1)

val2 <- sum(paths[cols, ])

message(" - answer = ", val2)

