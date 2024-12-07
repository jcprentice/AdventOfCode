library(tidyverse)

input <- readLines("day_03-input_s.txt")

str_split(input, "") |>
    unlist() |>
    matrix(nrow = str_length(input[[1]]),
           byrow = TRUE)

