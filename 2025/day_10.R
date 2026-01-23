library(tidyverse)

message("# Day 10")

# input <- readLines("2025/day_10-input-ex.txt")
input <- readLines("2025/day_10-input.txt")

data <- map(input, \(x) {
    machine <- str_extract(x, "\\[.*\\]") |>
        str_remove_all("[\\[\\]]") |>
        str_split_1("") |>
        map_int(~ {(. == "#") * 1})


    wiring <- str_extract(x, "\\(.*\\)") |>
        str_remove_all("[()]") |>
        str_split_1(" ") |>
        str_split(",") |>
        map(as.integer) |>
        map(~ . + 1)

    joltage <- str_extract(x, "\\{.*\\}") |>
        str_remove_all("[{}]") |>
        str_split_1(",") |>
        as.integer()

    mget(c("machine", "wiring", "joltage"))
})

val1 <- map_int(data, \(x) {
    # x <- data[[1]]
    machine <- x$machine
    wiring <- x$wiring
    N <- length(machine)
    W <- length(wiring)

    buttons <- matrix(0, N, W)
    walk(seq(W), \(i) buttons[wiring[[i]], i] <<- 1)

    toggles <- t(expand.grid(rep(list(0:1), W)))

    out <- (buttons %*% toggles) %% 2

    valid <- apply(out, 2, \(x) all(x == machine))

    presses <- apply(toggles, 2, sum)[valid]

    min(presses)
}) |> sum()

# 494
message(" - answer = ", val1)

val2 <- val1 <- map_int(data, \(x) {
    # x <- data[[1]]
    joltage <- x$joltage
    wiring <- x$wiring
    N <- length(joltage)
    W <- length(wiring)

    buttons <- matrix(0, N, W)
    walk(seq(W), \(i) buttons[wiring[[i]], i] <<- 1)
    buttons

    valid <- apply(out, 2, \(x) all(x == joltage))

    presses <- apply(toggles, 2, sum)[valid]
}) |> sum()

message(" - answer = ", val2)

