## Functions used to make outputs ##

library(shiny)
library(bslib)
library(ggplot2)
library(glue)

b_ratio <-  c(1/2, 1/2, 1/2, 2, 2, 2)
a <- c(6, 4, 2, 1, 2, 3)
prior_tbl <- 
  data.frame(certainty = c("Sure they're different",
                           "Not sure they're different",
                           "I really have no idea, but I hope they're different",
                           "I really have no idea, but I hope they're the same",
                           "Not sure they're the same",
                           "Sure they're the same"),
             proportion = c(1/3, 1/3, 1/3, 2/3, 2/3, 2/3),
             b_ratio = b_ratio,
             a = a,
             b = b_ratio * a)

prior_ab <- function(prior_tbl, level) {
  certainty_choice <- subset(prior_tbl, certainty == level)
  out <- c(certainty_choice$a, certainty_choice$b)
  out
}

lims_fun <- function(N, X) {
  qbeta(p = c(0.05, 0.95), shape1 = 6 + X, shape2 = 6 + N - X)
}

report_txt <- function(N, X, confidence = 0.9) {
  lims <- lims_fun(N, X)
  glue(
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
  
  data.frame(
    difference = c(rep("Noticed", 3), rep("Did not notice", 3)),
    name = rep(
      factor(c("observed", "max", "min"),
             levels = c("observed", "min", "max")),
      2),
    value = c(X, round(lims[2] * 100), round(lims[1] * 100),
              N - X, round((1 - lims[2]) * 100), round((1 - lims[1]) * 100)
    )
  ) |>
    
    # and plot
    
    ggplot(aes(x = name, y = value, fill = difference)) + 
    geom_col(position = "dodge") + 
    facet_wrap(~name, scales = "free_x",
               labeller = labeller(
                 name = c(observed = "Observed",
                          min = "of 100 consumers,\nminimum noticing a difference",
                          max = "of 100 consumers,\nmaximum noticing a difference")
               )) + 
    
    # clean it up
    
    scale_fill_manual(values = c("darkgreen", "goldenrod")) + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
    labs(fill = NULL, x = NULL, y = "tasters",
         title = glue("Tetrad test results for {N} tasters"),
         subtitle = glue("with {X} correct responses and {round(100 * confidence, 0)}% confidence")) + 
    theme_linedraw(base_size = 20) +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.ticks.length.x = unit(0, "cm")) 
}