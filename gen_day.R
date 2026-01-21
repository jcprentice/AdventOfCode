library(stringr)

gen_day <- function(d = 1, y = 2025) {
    fd <- str_pad(d, 2, pad = "0")

    f <- str_glue("{y}/day_{fd}.R")

    str_glue(
        "library(tidyverse)",
        "",
        "message(\"# Day {fd}\")",
        "",
        "# input <- readLines(\"{y}/day_{fd}-input-ex.txt\")",
        "input <- readLines(\"{y}/day_{fd}-input.txt\")",
        "",
        "val1 <- 0",
        "",
        "message(\" - answer = \", val1)",
        "",
        "val2 <- 0",
        "",
        "message(\" - answer = \", val2)",
        "\n",
        .sep = "\n",
    ) |>
        cat()
        writeLines(con = f)

    f |> str_replace("\\.R", "-input-ex.txt") |> file.create()
    f |> str_replace("\\.R", "-input.txt") |> file.create()
}
