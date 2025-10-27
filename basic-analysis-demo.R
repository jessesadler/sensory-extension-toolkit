
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


# Update priors -----------------------------------------------------------

# You chose a particular belief about what you'd see.  You did the test and got
# X and N.  Here's what we'd conclude:

# for example, for row 1 of the prior_plots data frame above and the N and X
# given

prior_plots[1, ]

plot_beta_binomial(alpha = 6, beta = 3, y = X, n = N)
summarize_beta_binomial(alpha = 6, beta = 3, y = X, n = N)
plot_beta_ci(alpha = 6 + X, beta = 6 + N - X, ci_level = 0.9) + 
  annotate(geom = "segment", xmin = )

# I can write some functions to get the actual CI level boundaries (quantiles)
lims <- qbeta(p = c(0.05, 0.95), shape1 = 6 + X, shape2 = 6 + N - X)
plot_beta_ci(alpha = 6 + X, beta = 6 + N - X, ci_level = 0.9) + 
  annotate(geom = "segment", x = lims[1], xend = lims[2], y = 0.5, yend = 0.5, color = "red")






















