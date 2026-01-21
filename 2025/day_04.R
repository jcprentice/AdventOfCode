library(tidyverse)
library(gsignal)

message("# Day 04")

# input <- read_lines("2025/day_04-input-ex.txt")
input <- read_lines("2025/day_04-input.txt")

N <- length(input)

x <- input |>
    str_replace_all(c("@" = "1", "\\." = "0")) |>
    str_split("") |>
    unlist() |>
    as.integer() |>
    matrix(nrow = N, byrow = TRUE)

window <- matrix(1, 3, 3); window[2, 2] <- 0

valid <- conv2(x, window, "same") < 4
val1 <- sum(valid * x)


message(" - answer = ", val1)

x1 <- x
out <- 0
while (TRUE) {
    valid <- (conv2(x1, window, "same") < 4) * x1
    new_out <- sum(valid)
    out <- out + new_out
    x1 <- x1 - valid
    if (new_out == 0) break
}

val2 <- out

message(" - answer = ", val2)
