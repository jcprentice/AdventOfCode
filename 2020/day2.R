# Day 2 ----

library(data.table)

## Part 1 ----

x <- fread("input2.txt")
setnames(x, c("dir", "dist"))

sx <- x[, .(dist = sum(dist)), by = dir]
ans1 <- sx[, dist[dir == "forward"] * (dist[dir == "down"] - dist[dir == "up"])]
print(ans1)


## Part 2 ----

x2 <- copy(x)
x2[, aim := 0]
x2[dir == "down", aim := dist]
x2[dir == "up", aim := -dist]

x2[, c_aim := cumsum(aim)]
x2[, `:=`(horiz = 0, vert = 0)]
x2[dir == "forward", `:=`(horiz = c_aim * dist, vert = dist)]
ans2 <- x2[, sum(horiz) * sum(vert)]
print(ans2)

