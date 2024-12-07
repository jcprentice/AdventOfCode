# Day 1 ----

## Part 1 ----

x <- fread("input1.txt")$V1

ans1 <- sum(diff(x) > 0)
print(ans1)


## Part 2 ----

# Using diff with lag
ans1a <- sum(diff(x, lag = 3) > 0)
print(ans1a)

# Simple adding
N <- length(x)
x3 <- x[1:(N-2)] + x[2:(N-1)] + x[3:N]
ans1b <- sum(diff(x3) > 0)
print(ans1b)

# Using a filter
xf <- filter(x, rep(1, 3), sides=2)
ans1c <- sum(diff(xf) > 0, na.rm = TRUE)
print(ans1c)

# Using convolution and an FFT
ans1d <- 0
print(ans1d)


