library(tidyverse)

message("# Day 06")

# input <- read_lines("2025/day_06-input-ex.txt")
input <- read_lines("2025/day_06-input.txt")

mat <- input |> str_squish() |> str_split(" ") |> unlist() |>
    matrix(nrow = length(input), byrow = TRUE)

nr <- dim(mat)[[1]]
nc <- dim(mat)[[2]]

val1 <- map_dbl(1:nc, \(i) {
    op <- mat[nr, i]
    nos <- mat[1:(nr - 1), i] |> as.integer()
    if (op == "*") prod(nos) else sum(nos)
}) |> sum()


message(" - answer = ", val1)

# First pad all rows to ensure they are the same width, with a space at the end
width <- map_int(input, nchar) |> max()
input <- map(input, \(s) str_pad(s, width + 1, "right"))

# Next find the column boundaries, these start with a * or a +
pos <- input[[nr]] |> str_locate_all("[*+] *") |> _[[1]]

val2 <- map_dbl(1:nc, \(i) {
    x <- input |>
        map_chr(str_sub, pos[i, 1], pos[i, 2] - 1)
    op <- x[[nr]] |> str_squish()
    nos <- head(x, -1) |> str_split("") |> unlist() |>
        matrix(nrow = nr - 1, byrow = TRUE) |>
        apply(2, str_flatten) |> str_squish() |>
        as.integer()

    if (op == "*") prod(nos) else sum(nos)
}) |>
    sum()

message(" - answer = ", val2)

