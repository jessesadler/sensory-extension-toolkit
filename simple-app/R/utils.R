prior_tbl <- 
  tibble(certainty = c("Sure they're different",
                       "Not sure they're different",
                       "I really have no idea, but I hope they're different",
                       "I really have no idea, but I hope they're the same",
                       "Not sure they're the same",
                       "Sure they're the same"),
         proportion = c(1/3, 1/3, 1/3, 2/3, 2/3, 2/3),
         b_ratio = c(1/2, 1/2, 1/2, 2, 2, 2),
         a = c(6, 4, 2, 1, 2, 3),
         b = b_ratio * a)

prior_ab <- function(prior_tbl, level) {
  certainty_choice <- prior_tbl |> 
    dplyr::filter(certainty == level)
  out <- c(certainty_choice$a, certainty_choice$b)
  out
}

lims_fun <- function(N, X) {
  qbeta(p = c(0.05, 0.95), shape1 = 6 + X, shape2 = 6 + N - X)
}

report_txt <- function(N, X, confidence = 0.9) {
  lims <- lims_fun(N, X)
  str_glue(
    "Based on your prior certainty about how different your samples were, ", 
    "and the fact that {X}/{N} of your tasters correctly completed the ",
    "discrimination test, we estimate that in approximately ", 
    "{round(confidence * 100, 0)}% of cases, a minimum of ", 
    "{round(100*lims[1], 0)}% and a maximum of {round(100*lims[2], 0)}% ",
    "of consumers would notice the difference between these two sample wines."
  )
}

report_plt <- function(N, X, confidence = 0.9) {
  lims <- lims_fun(N, X)
  
  tibble(difference = c("noticed", "didn't notice"), 
         observed = c(X, N - X),
         `of 100 consumers,\nminimum noticing a difference` = round(c(lims[1], 1-lims[1]) * 100, 0),
         `of 100 consumers,\nmaximum noticing a difference` = round(c(lims[2], 1-lims[2]) * 100, 0)) |>
    pivot_longer(-difference) |>
    mutate(name = fct(name) |> fct_relevel("observed")) |>
    
    # and plot
    
    ggplot(aes(x = name, y = value, fill = difference)) + 
    geom_col(position = "dodge") + 
    facet_wrap(~name, scales = "free_x") + 
    
    # clean it up
    
    scale_fill_manual(values = c("darkgreen", "goldenrod")) + 
    labs(fill = NULL, x = NULL, y = "tasters",
         title = str_glue("Tetrad test results for {N} tasters"),
         subtitle = str_glue("with {X} correct responses and {round(100 * confidence, 0)}% confidence")) + 
    theme_linedraw() +
    theme(legend.position = "bottom") 
}