library(tidyverse)
library(matricks)
library(progress)

message("# Day 09")

# input <- readLines("2025/day_09-input-ex.txt")
input <- readLines("2025/day_09-input.txt")

in1 <- input |> str_split(",") |> map(as.integer) |> unlist() |>
    matrix(ncol = 2, byrow = TRUE, dimnames = list(NULL, c("x", "y"))) |>
    as_tibble()
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

tin <- as_tibble(in1) |>
    mutate(col = "red")
{
    # idx <- sample(N, 2)
    r <- in1[idx, ]
    xmin <- min(r[, "x"])
    xmax <- max(r[, "x"]) + 1
    ymin <- min(r[, "y"])
    ymax <- max(r[, "y"]) + 1

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

library(sf)
poly <- rbind(in1, in1[1,] ) |> list() |>
    st_polygon() |> st_sfc() |> st_sf(geometry = _)

is_valid <- function(i, j) {
    # i <- idx[[1]]; j <- idx[[2]]
    px1 <- in1[c(i, j), "x"] |> min()
    px2 <- in1[c(i, j), "x"] |> max()
    py1 <- in1[c(i, j), "y"] |> min()
    py2 <- in1[c(i, j), "y"] |> max()
    # c(px1, px2, py1, py2)

    rect <- matrix(c(px1, py1, px2, py1, px2, py2, px1, py2, px1, py1),
                   ncol = 2, byrow = 2) |> list() |>
        st_polygon() |> st_sfc() |> st_sf(geometry = _)
    # print(rect)

    length(st_within(rect, poly)[[1]] > 0)
}

idx <- c(0, 0)
max_area <- 0
pb <- progress_bar$new(total = N^2)
for (i in seq(N - 1)) for (j in seq(2, N)) {
    pb$tick()
    area <- get_area(i, j)
    if (area > max_area && is_valid(i, j)) {
        max_area <- area
        idx <- c(i, j)
    }
}
pb$terminate()

val2 <- max_area

# 1516172795
message(" - answer = ", val2)

