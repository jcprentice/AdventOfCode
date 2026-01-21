library(tidyverse)
library(matricks)
library(progress)

message("# Day 09")

# input <- readLines("2025/day_09-input-ex.txt")
input <- readLines("2025/day_09-input.txt")

in1 <- input |> str_split(",") |> map(as.integer) |> unlist() |>
    matrix(ncol = 2, byrow = TRUE, dimnames = list(NULL, c("x", "y")))
N <- length(input)
dims <- apply(in1, 2, max)

get_area <- function(i, j) prod(abs(in1[i, ] - in1[j, ]) + 1)

idx <- c(0, 0)
max_area <- 0
pb <- progress_bar$new(total = (N-1)^2)
for (i in seq(N-1)) for (j in seq(2, N)) {
    pb$tick()
    area <- get_area(i, j)
    if (area > max_area) {
        max_area <- area
        idx <- c(i, j)
    }
}

val1 <- max_area
# idx <- c(60, 314)
# 4790063600
message(" - answer = ", val1)

tin <- as_tibble(in1)
{
    # idx <- sample(N, 2)
    r <- in1[idx, ]
    xmin <- min(r[, "x"])
    xmax <- max(r[, "x"]) + 1
    ymin <- min(r[, "y"])
    ymax <- max(r[, "y"]) + 1

    tin$col <- "red"
    tin$col[[1]] <- "start"
    tin$col[idx] <- "picked"

    # First let's plot the figure to see what we're working with
    ggplot(tin, aes(x = x, y = y, colour = col)) +
        geom_polygon(colour = "black",
                     fill = "green") +
        scale_colour_manual("Colour",
                            breaks = c("red", "start", "picked"),
                            values = c("red", "purple", "yellow")) +
        annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                 colour = "black", alpha = 0.5) +
        geom_point() +
        # Start Tile
        coord_equal()
}

is_inside <- function(i, j) {
    # i <- idx[[1]]; j <- idx[[2]]
    xs <- in1[c(i, j), "x"] |> sort()
    ys <- in1[c(i, j), "y"] |> sort()

    x1 <- xs[[1]]; x2 <- xs[[2]]
    y1 <- ys[[1]]; y2 <- ys[[2]]

    for (id in 1:N) {
        # check if a point is inside the rectangle
        # (it's okay to be on the boundary)
        ir <- x1 < in1[id, "x"] && in1[id, "x"] < x2 &&
            y1 < in1[id, "y"] && in1[id, "y"] < y2
        if (ir) return(TRUE)

        # check if a connecting line passes through the rectangle
        # it must be actually inside to count
        id1 <- if (id == N) 1 else i + 1

        hz <- in1[c(id, id1), "x"] |> sort()
        ih <- hz[[1]] < x1 && x2 < hz[[2]]
        if (ih) return(TRUE)

        vt <- in1[c(id, id1), "y"] |> sort()
        iv <- vt[[1]] < y1 && y2 < vt[[2]]
        if (iv) return(TRUE)
    }

    FALSE
}

idx <- c(0, 0)
max_area <- 0
pb <- progress_bar$new(total = N^2)
for (i in seq(N-1)) for (j in seq(2, N)) {
    pb$tick()
    area <- get_area(i, j)
    if (area > max_area && !is_inside(i, j)) {
        max_area <- area
        idx <- c(i, j)
    }
}
pb$terminate()

val2 <- max_area


message(" - answer = ", val2)

