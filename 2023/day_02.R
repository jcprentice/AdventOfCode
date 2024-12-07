library(tidyverse)

input <- readLines("day_02-input.txt")

val1 <- imap_int(input, \(x, i) {
    y <- x |>
        str_split_i(": ", 2) |>
        str_replace_all(";", ",") |>
        str_split_1(", ") |>
        str_split(" ") |>
        map_lgl(\(y) {
            num <- y[[1]] |> as.integer()
            colour <- y[[2]]
            switch(colour,
                   "red" = {num <= 12},
                   "green" = {num <= 13},
                   "blue" = {num <= 14})
        }) |>
        all()
    y * i
}) |>
    sum()

message("Sum of IDs = ", val1)


val2 <- map_int(input, \(x, i) {
    x |>
        str_split_i(": ", 2) |>
        str_replace_all(";", ",") |>
        str_split_1(", ") |>
        str_split(" ") |>
        map(~ set_names(.x, c("num", "col"))) |>
        bind_rows() |>
        mutate(num = as.integer(num)) |>
        group_by(col) |>
        summarise(max = max(num), .groups = "drop") |>
        summarise(prod(max)) |>
        as.integer()
}) |>
    sum()

message("Prod of powers = ", val2)

