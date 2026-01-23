library(stringr)

gen_day <- function(d = 1, y = 2025) {
    fd <- str_pad(d, 2, pad = "0")

    f <- str_glue("{y}/day_{fd}.R")

    s <- str_glue(
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
    )
    if (!file.exists(f)) writeLines(s, con = f)

    fex <- str_replace(f, ".R", "-input-ex.txt")
    if (!file.exists(fex)) file.create(fex)

    fin <- str_replace(f, ".R", "-input.txt")
    if (!file.exists(fin)) file.create(fin)

    message(str_glue("files generated for {y} / day {fd}"))
}
