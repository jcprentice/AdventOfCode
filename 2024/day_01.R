library(tidyverse)

message("# Day 01")

input <- read_delim("day_01-input.txt",
                    delim = "   ",
                    col_names = FALSE,
                    col_types = "ii") |>
    set_names("a", "b")

val1 <- with(input, sum(abs(sort(a) - sort(b))))
message(" - Distance = ", val1)

val2 <- map_int(input$a, \(a) a * sum(a == input$b)) |> sum()
message(" - Similarity = ", val2)

