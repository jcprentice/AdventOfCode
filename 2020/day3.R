# Day 3 ----

library(data.table)

## Part 1 ----

x <- fread("input3.txt", colClasses = "character")
setnames(x, "bits")

tic()
N <- x[1, nchar(bits)]
y1 <- y2 <- integer(N)

for (i in 1:N) {
    y1[i] <- x[, mean(as.integer(substr(bits, i, i))) > 0.5]
    y2[i] <- 1 - y1[i]
}

gamma_rate   <- strtoi(paste0(y1, collapse = ""), base = 2)
message("Gamma rate = ", gamma_rate)
epsilon_rate <- strtoi(paste0(y2, collapse = ""), base = 2)
message("Epsilon_rate = ", epsilon_rate)

power_consumption <- gamma_rate * epsilon_rate
message("Power consumption = ", power_consumption)


## Part 2 ----

x <- fread("input3.txt", colClasses = "character")
setnames(x, "bits")

N <- x[1, nchar(bits)]
y <- integer(N)

sub_x <- copy(x)

for (i in 1:N) {
	if (sub_x[, .N] == 1)
		break

	t <- sub_x[, table(substr(bits, i, i))]

	sub_x <- if (t["0"] > t["1"]) {
		sub_x[substr(bits, i, i) == 0]
	} else {
		sub_x[substr(bits, i, i) == 1]
	}
}
oxy_rating <- strtoi(sub_x[1, bits], base = 2)
message("Oxygen rating = ", oxy_rating)

sub_x <- copy(x)
for (i in 1:N) {
    if (sub_x[, .N] == 1)
		break

	t <- sub_x[, table(substr(bits, i, i))]

	sub_x <- if (t["0"] <= t["1"]) {
		sub_x[substr(bits, i, i) == 0]
	} else {
		sub_x[substr(bits, i, i) == 1]
	}
}
CO2_rating <- strtoi(sub_x[1, bits], base = 2)
message("CO2 Scrubber rating = ", CO2_rating)

LS_rating <- oxy_rating * CO2_rating
message("Life Support rating = ", LS_rating)