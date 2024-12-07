library(tidyverse)
library(furrr)
library(matricks)

message("\n# Day 06")

input <- readLines("2024/day_06-input.txt")
input

N <- length(input)
mat <- input |>
    str_split("") |>
    unlist() |>
    matrix(byrow = TRUE, nrow = N)
mat

mod1 <- function(n, d) ((n - 1) %% d) + 1
div1 <- function(n, d) as.integer(ceiling(n / d))
i_to_xy <- function(i, N) c(mod1(i, N), div1(i, N))

xy0 <- i_to_xy(which(mat == "^"), N)
at(mat, xy0) <- "X"


direction <- list(c(-1, 0), c(0, 1), c(1, 0), c(0, -1))
dirs <- c("N", "E", "S", "W")
rotate <- function(n = 1) mod1(n + 1, 4)

pdir <- function(x) paste0("[", x[1], ",", x[2], "]")

find_path <- function(mat, pos = xy0, dir = 1) {
    test_pos <- pos + direction[[dir]]
    if (any(test_pos %in% c(0, N + 1))) {
        # message("Gone off the edge at ", pdir(pos), " heading ", dirs[[dir]])
        return(mat)
    }
    
    if (at(mat, test_pos) == "#") {
        # Something in front of us, turn right
        new_pos <- pos
        new_dir <- rotate(dir)
        # message("Obstruction at ", pdir(test_pos), " turning ", dirs[[dir]])
    } else {
        # Nothing in front of us, move forwards
        # message("Moving ", dirs[[dir]], " from ", pdir(pos), " to ", pdir(test_pos))
        new_pos <- test_pos
        new_dir <- dir
        at(mat, new_pos) <- "X"
    }
    Tailcall(find_path, mat, new_pos, new_dir)
}

mat_end <- find_path(mat)

part1 <- sum(mat_end == "X")

message(" - Part 1 = ", part1)



print_mat <- function(mat) {
    cat(paste0(apply(mat, 1, paste, collapse = ""), collapse = "\n"))
}


find_path2 <- function(mat, pos = xy0, dir = 1) {
    test_pos <- pos + direction[[dir]]
    
    # Left lab
    if (any(test_pos %in% c(0, N + 1))) return(FALSE)
    
    # Loop condition
    if (at(mat, test_pos) == dirs[[dir]]) return(TRUE)
    
    if (at(mat, test_pos) == "#") {
        # Something in front of us, turn right
        new_pos <- pos
        new_dir <- rotate(dir)
        # message("Obstruction at ", pdir(test_pos), " turning ", dirs[[dir]])
    } else {
        # Nothing in front of us, move forwards
        # message("Moving ", dirs[[dir]], " from ", pdir(pos), " to ", pdir(test_pos))
        new_pos <- test_pos
        new_dir <- dir
        at(mat, new_pos) <- dirs[[dir]]
    }
    Tailcall(find_path2, mat, new_pos, new_dir)
}



part2 <- map_lgl(seq_len(length(mat)), \(i) {
    block <- i_to_xy(i, N)
    
    if (at(mat_end, block) != "X") return(FALSE)
    
    at(mat, block) <- "#"
    find_path2(mat, xy0, 1)
},
.progress = TRUE) |> sum()

message(" - Part 2 = ", part2)
