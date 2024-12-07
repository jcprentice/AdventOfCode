library(tidyverse)


input <- readLines("day_01-input.txt") # "_s1", "_s2"

val1 <- map_int(input, \(x) {
    y <- x |>
        str_split_1("") |>
        str_subset("[a-z]", negate = TRUE)
    paste0(y[[1]], y[[length(y)]]) |>
        as.integer()
}) |>
    sum()

message("Sum of calibration values = ", val1)

# Be careful as twone counts as 21, so replace words with digits but keep
# letters at the start and end if they could be part of another digit
digits <- c("one" = "o1e", "two" = "t2o", "three" = "t3e",
            "four" = "4", "five" = "5e", "six" = "6",
            "seven" = "7n", "eight" = "e8t", "nine" = "n9e")

val2 <- map_int(input, \(x) {
    y <- x |>
        str_replace_all(digits) |>
        str_remove_all("[a-z]") |>
        str_split_1("")
    paste0(y[[1]], y[[length(y)]]) |>
        as.integer()
}) |>
    sum()

message("Sum of calibration values = ", val2)

