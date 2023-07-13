options(digits = 4, width = 84)
options(dplyr.print_min = 6, dplyr.print_max = 6)
options(cli.width = 85)
options(crayon.enabled = FALSE)

knitr::opts_chunk$set(
  comment = "#>",
  fig.width = 8, fig.height = 5,
  fig.show = "hold"
)

library(tidyverse)
library(plotly)
theme_set(theme_bw())

pre_proc_tbl <- function(file_name) {
  d <- read_table(str_glue("../data/{file_name}.txt"), 
                  col_types = cols(.default = "character", "X6" = col_skip()),
                  skip = 10)
  col_names <- c("time", "x", "y", "z", "tilt_fc", "tilt_tw")
  d <- set_names(d, col_names)
  
  d <- d |> 
    mutate_if(is.character, .funs = as.numeric) |> 
    mutate(time = as.numeric(time), 
           time = lubridate::milliseconds(time))  
  
  d <- d |> tidyr::drop_na()
}


plot_ad <- function(tbl) {
  tbl |> 
    pivot_longer(c("x", "y", "z")) |> 
    mutate(time = seq_along(time)/1000) |> 
    ggplot(aes(time, value)) +
    geom_point(size = 0.1) +
    geom_line(alpha = 0.3) +
    facet_wrap(~name) +
    labs(x = "time (s)", 
         y = "Acceleration (g)")
}
