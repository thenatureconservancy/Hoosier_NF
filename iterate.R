


library(tidyverse)

ltas <- unique(data$MAP_UNIT_SYMBOL)


# try to outputs
purrr::map(
  .x = ltas,  # vector of param values
  .f = ~rmarkdown::render(
    input = "demo_report.Rmd",  # RMarkdown filepath
    params = list(MAP_UNIT_SYMBOL = .x),  # iterated parameter value
    output_file = stringr::str_glue("reports/", .x, "-report.html")  # iterated output path
  )
)
