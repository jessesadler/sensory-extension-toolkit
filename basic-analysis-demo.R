
# Setup -------------------------------------------------------------------


library(tidyverse)
library(tidybayes)
library(bayesrules)


# Determine prior beliefs -------------------------------------------------

# for the beta prior, E(pi) = a / (a + b):
# - 1 / 3 = a / (a + b) ==> a = b / 2
# - 2 / 3 = a / (a + b) ==> a = 2 * b

prior_plots <- 
  tibble(certainty = c("sure they're different",
                     "not sure they're different",
                     "I really have no idea, but I hope they're different",
                     "I really have no idea, but I hope they're the same",
                     "not sure they're the same",
                     "sure they're the same"),
       proportion = c(1/3, 1/3, 1/3, 2/3, 2/3, 2/3),
       b_ratio = c(1/2, 1/2, 1/2, 2, 2, 2),
       a = c(6, 4, 2, 1, 2, 3),
       b = b_ratio * a) |>
  mutate(prior_plot = map2(a, b, plot_beta),
         title = map2_chr(a, b, \(x, y) str_c("beta(", x, ", ", y, ")"))) |>
  mutate(prior_plot = map2(prior_plot, title, \(x, y) x + ggtitle(label = y))) |>
  mutate(prior_plot = map2(prior_plot, certainty, \(x, y) x + labs(subtitle = y)))

# Here's what those priors look like
do.call(gridExtra::grid.arrange, args = c(prior_plots$prior_plot, nrow = 2))


# Get the data ------------------------------------------------------------

# Say you have N people do a tetrad as described.  How many people (X) got the right
# answer?

N <- 20 # for the purpose of the demo
X <- 15 # for the purpose of the demo
confidence <- 0.9


# Update priors -----------------------------------------------------------

# You chose a particular belief about what you'd see.  You did the test and got
# X and N.  Here's what we'd conclude:

# for example, for row 1 of the prior_plots data frame above and the N and X
# given

prior_plots[1, ]

plot_beta_binomial(alpha = 6, beta = 3, y = X, n = N)
summarize_beta_binomial(alpha = 6, beta = 3, y = X, n = N)
plot_beta_ci(alpha = 6 + X, beta = 6 + N - X, ci_level = confidence) 

# I can write some functions to get the actual CI level boundaries (quantiles)
lims <- qbeta(p = c(0.05, 0.95), shape1 = 6 + X, shape2 = 6 + N - X)
plot_beta_ci(alpha = 6 + X, beta = 6 + N - X, ci_level = confidence) + 
  annotate(geom = "segment", x = lims[1], xend = lims[2], y = 0.5, yend = 0.5, color = "red")



# Reporting output --------------------------------------------------------

# Some example text

str_glue("Based on your prior certainty about how different your samples were, ", 
         "and the fact that {X}/{N} of your tasters correctly completed the ",
         "discrimination test, we estimate that in approximately ", 
         "{round(confidence * 100, 0)}% of cases, a minimum of ", 
         "{round(100*lims[1], 0)}% and a maximum of {round(100*lims[2], 0)}% ",
         "of consumers would notice the difference between these two sample wines.")

# An example plot

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


















