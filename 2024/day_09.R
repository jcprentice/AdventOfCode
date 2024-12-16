library(tidyverse)

message("\n# Day 09")

input <- readLines("2024/day_09-input.txt")
input

fs <- input |>
    str_split_1("") |>
    as.integer() |>
    imap(~ rep(list(if (.y %% 2 == 0) "." else .y %/% 2L), .x)) |>
    unlist()

used <- sum(fs != ".")

compact <- function(fs) {
    space_idxs <- which(fs == ".")
    first_space_idx <- space_idxs[[1]]
    if (used < first_space_idx) {
        return(fs)
    }

    files <- which(fs != ".")
    last_file_idx <- files[[length(files)]]
    fs[[first_space_idx]] <- fs[[last_file_idx]]
    fs[[last_file_idx]] <- "."

    Tailcall(compact, fs)
}

cfs <- compact(fs)
part1 <- sum(as.integer(cfs[seq(used)]) * seq(0, used - 1))
message("- Part1 = ", part1)


fs <- input |>
    str_split_1("") |>
    as.integer() |>
    imap(~ rep(list(if (.y %% 2 == 0) -1L else .y %/% 2L), .x)) |>
    unlist()

compact2 <- function(fs, fid = max(fs)) {
    # message(paste(fs, collapse = "") |> str_replace_all("-1", "."))
    if (fid == 0L) {
        return(fs)
    }

    fstab <- with(rle(fs), tibble(file_id = values, size = lengths)) |>
        mutate(idx = row_number(),
               pos = cumsum(c(0L, size))[seq_len(length(size))])

    fid_row <- filter(fstab, file_id == fid) |> as.list()
    spaces <- filter(fstab, file_id == -1L, size >= fid_row$size, pos < fid_row$pos)

    if (nrow(spaces) > 0) {
        fs[spaces$pos[[1L]] + seq(fid_row$size)] <- fid
        fs[fid_row$pos + seq(fid_row$size)] <- -1L
    }
    Tailcall(compact2, fs, fid - 1L)
}

cfs <- compact2(fs)
part2 <- sum((cfs * seq(0L, length(cfs) - 1L))[cfs != -1L])
message("- Part2 = ", part2)


