library(tidyverse)

message("# Day 08")

input <- read_lines("2025/day_08-input-ex.txt")
input <- read_lines("2025/day_08-input.txt")

positions <- input |> str_split(",") |> unlist() |> as.integer() |>
    matrix(byrow = TRUE, ncol = 3)

N <- nrow(positions)

dmat <- matrix(Inf, N, N)

for (i in seq(N - 1)) for (j in seq(i + 1, N)) {
    dmat[i, j] <- sqrt(sum((positions[i, ] - positions[j, ])^2))
}

get_xy <- function(i) {
    c(((i - 1) %% N) + 1, ceiling(i / N))
}

ranks <- matrix(rank(dmat, ties = "min"), N, N)

# Now connect the boxes
boxes <- rep(0, N)
grp <- 0
mat <- matrix(0, N, N)
for (i in 1:10) {
    xy <- get_xy(which(ranks == i))
    bxy <- boxes[xy]
    zeros <- sum(bxy == 0)
    if (zeros == 2) {
       grp <- grp + 1
       boxes[xy] <- grp
    } else if (zeros == 1) {
        boxes[xy] <- max(bxy)
    } else {
        # already in a group, move everything into lowest group
        boxes[xy] <- min(bxy)
        boxes[boxes == max(bxy)] <- min(bxy)
    }
    print(c(i, xy))
    mat[i,] <- boxes
}
val1 <- prod(tail(sort(table(boxes)[-1]), 3))

# 123234
message(" - answer = ", val1)


boxes <- rep(0, N)
grp <- 0
i <- 0
while(TRUE) {
    i <- i + 1
    xy <- get_xy(which(ranks == i))
    bxy <- boxes[xy]
    zeros <- sum(bxy == 0)
    if (zeros == 2) {
       grp <- grp + 1
       boxes[xy] <- grp
    } else if (zeros == 1) {
        boxes[xy] <- max(bxy)
    } else {
        # already in a group, move everything into lowest group
        boxes[xy] <- min(bxy)
        boxes[boxes == max(bxy)] <- min(bxy)
    }
    if (sum(boxes == 0) == 0) {
        break
    }
    # mat[i,] <- boxes
}

val2 <- prod(positions[xy, 1])
# 9259958565
message(" - answer = ", val2)

