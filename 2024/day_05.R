library(tidyverse)

message("\n# Day 05")

input <- "day_05-input.txt"

lines <- readLines(input)
line_no <- which(lines == "")
orders <- read_delim(input,
                     delim = "|",
                     col_names = FALSE,
                     col_types = "ii",
                     n_max = line_no - 1)
updates <- lines[seq(line_no + 1, length(lines))] |>
    str_split(",") |>
    map(as.integer)


# Part 1 ----

get_mid <- function(x) x[[(length(x) + 1L) %/% 2L]]

is_correct <- map_lgl(updates, \(x) {
    map_lgl(seq_len(nrow(orders)), \(i) {
        rule <- as.integer(orders[i, ])
        pos1 <- which(rule[[1]] == x)
        pos2 <- which(rule[[2]] == x)
        if (length(pos1) == 0 || length(pos2) == 0) {
            return(TRUE)
        }
        pos1 < pos2
    }) |> all()
},
.progress = TRUE)

part1 <- updates[is_correct] |> map_int(get_mid) |> sum()

message(" - Part 1 = ", part1)


# Part 2 ----

less_than <- function(a, b) {
    map_lgl(seq_len(nrow(orders)), \(i) {
        rule <- as.integer(orders[i, ])
        pos1 <- which(rule[[1]] == a)
        pos2 <- which(rule[[2]] == b)
        if (length(pos1) == 0 || length(pos2) == 0) {
            return(TRUE)
        }
        pos1 < pos2
    }) |> all()
}

fix_order <- function(x) {
    merge <- function(a, b) {
        if (length(a) == 0) b
        else if (length(b) == 0) a
        else if (less_than(a[1], b[1])) c(a[[1]], merge(a[-1], b))
        else c(b[[1]],  merge(a, b[-1]))
    }
    merge_sort <- function(x) {
        n <- length(x)
        if (n <= 1) {
            x
        } else {
            n2 <- n %/% 2
            merge(merge_sort(x[1:n2]), merge_sort(x[seq(n2 + 1, n)]))
        }
    }
    merge_sort(x)
}

part2 <- updates[!is_correct] |>
    map(fix_order, .progress = TRUE) |>
    map_int(get_mid) |>
    sum()

message(" - Part 2 = ", part2)
