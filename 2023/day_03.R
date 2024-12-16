library(tidyverse)

input <- readLines("2023/day_03-input.txt")

N <- length(input)

mat <- input |>
    str_split("") |>
    unlist() |>
    matrix(byrow = TRUE, nrow = N)

nums <- input |> str_extract_all("\\d+") |> map(as.numeric)
positions <- input |> str_locate_all("\\d+")

part1 <- map_int(seq(N), \(i) {
    num_i <- nums[[i]]
    pos_i <- positions[[i]]

    valid <- map_lgl(seq_len(length(num_i)), \(j) {
        tp <- max(i - 1, 1)
        bm <- min(i + 1, N)
        lt <- max(pos_i[[j, 1]] - 1, 1)
        rt <- min(pos_i[[j, 2]] + 1,N)
        submat <- mat[tp:bm, lt:rt]
        str_detect(submat, "\\.|\\d", negate = TRUE) |> any()
    })
    # print(paste0(nums[valid], collapse = ", "))
    sum(num_i[valid])
}) |> sum()

message(" - Part 1 = ", part1)


part2 <- map_int(seq(N), \(i) {
    gears <- input[[i]] |> str_locate_all("\\*") |> pluck(1)

    map_int(seq_len(nrow(gears)), \(j) {
        tp <- max(i - 1, 1)
        bm <- min(i + 1, N)
        lt <- max(gears[[j, 1]] - 1, 1)
        rt <- min(gears[[j, 1]] + 1, N)

        nearby <- map(tp:bm, \(k) {
            map_lgl(seq_len(nrow(positions[[k]])), \(l) {
                any(lt:rt %in% seq(positions[[k]][[l, 1]], positions[[k]][[l, 2]]))
            })
        })

        if (sum(unlist(nearby)) == 2) {
            prod(c(nums[[tp]][nearby[[1]]],
                   nums[[i ]][nearby[[2]]],
                   nums[[bm]][nearby[[3]]]))
        } else {
            0
        }
    }) |> sum()
}) |> sum()

message(" - Part 2 = ", part2)
