#' Difference Between SAM and Sample Median
#'
#' @description
#' A function that will return the difference S. Median - SAM
#'
#' @param n int The sample size
#' @param distribution character The distribution name to be used
#'
#' @returns The difference in value of the SAM and S. Median
#'
#' @details
#' The function generates a random sample of size `n` from the
#' `distribution` distribution. From this sample, we then calculate
#' the values of the Sample Arithmetic Mean and the Sample Median. The
#' function returns the difference S. Median - SAM.
#'
getDifference <- function(n, distribution){
  ## Generate Random Sample
  if (distribution == "uniform") {
    randSample <- sample(x = 1:10, size = n, replace = TRUE)
  } else if (distribution == "gaussian") {
    randSample <- rnorm(n = n, mean = 0, sd = 1)
  } else if (distribution == "poisson") {
    randSample <- rpois(n = n, lambda = 0.75)
  } else if (distribution == "exponential") {
    randSample <- rexp(n = n, rate = 0.5)
  } else {
    ### Optional
    ### Any other value for distribution causes a stop
    stop("Distribution value not recognized")
  }
  
  ## Calculate values of SAM and Sample Median
  sam <- mean(randSample)
  smedian <- median(randSample)
  
  ## Calculate the difference
  diff <- smedian - sam
  
  return(diff)
}

#-------------------------------------------#
# Monte Carlo Simulation ----
## Carry out the simulations for the 12 combinations of sample sizes
## and distributions.

# Load packages -----
library(tidyverse)

# Run distributions ----
## Discrete Uniform ----
uniform.10 <- replicate(
  n = 10000,
  expr = getDifference(n = 10, distribution = "uniform")
)
uniform.50 <- replicate(
  n = 10000,
  expr = getDifference(n = 50, distribution = "uniform")
)
uniform.100 <- replicate(
  n = 10000,
  expr = getDifference(n = 100, distribution = "uniform")
)
## Gaussian ----
gaussian.10 <- replicate(
  n = 10000,
  expr = getDifference(n = 10, distribution = "gaussian")
)
gaussian.50 <- replicate(
  n = 10000,
  expr = getDifference(n = 50, distribution = "gaussian")
)
gaussian.100 <- replicate(
  n = 10000,
  expr = getDifference(n = 100, distribution = "gaussian")
)
## Poisson ----
poisson.10 <- replicate(
  n = 10000,
  expr = getDifference(n = 10, distribution = "poisson")
)
poisson.50 <- replicate(
  n = 10000,
  expr = getDifference(n = 50, distribution = "poisson")
)
poisson.100 <- replicate(
  n = 10000,
  expr = getDifference(n = 100, distribution = "poisson")
)
## Exponential ----
exponential.10 <- replicate(
  n = 10000,
  expr = getDifference(n = 10, distribution = "exponential")
)
exponential.50 <- replicate(
  n = 10000,
  expr = getDifference(n = 50, distribution = "exponential")
)
exponential.100 <- replicate(
  n = 10000,
  expr = getDifference(n = 100, distribution = "exponential")
)

# Make Data Frame ----
simResults <- bind_cols(
  uniform.10, uniform.50, uniform.100,
  gaussian.10, gaussian.50, gaussian.100,
  poisson.10, poisson.50, poisson.100,
  exponential.10, exponential.50, exponential.100
)

# Wrangle the data ----
## Apply Meaningful names
## Pivot the columns to one for values and one for names
## Separate the names to the simulation parameters
## Set sample size to numeric data type
simResults <- simResults %>%
  rename(
    uniform.10 = ...1,
    uniform.50 = ...2,
    uniform.100 = ...3,
    gaussian.10 = ...4,
    gaussian.50 = ...5,
    gaussian.100 = ...6,
    poisson.10 = ...7,
    poisson.50 = ...8,
    poisson.100 = ...9,
    exponential.10 = ...10,
    exponential.50 = ...11,
    exponential.100 = ...12
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "simSettings",
    values_to = "diffs"
  ) %>%
  separate_wider_delim(
    cols = "simSettings",
    delim = ".",
    names = c("distribution", "sampleSize")
  ) %>%
  mutate(
    sampleSize = parse_number(sampleSize)
  )

View(simResults)

# Make Plot ----
## The following code demonstrates some initial attempts at creating 
## data visualizations for this Monte Carlo simulation.
# ## Initial Plot ----
# ggplot(
#   data = simResults,
#   mapping = aes(x = distribution, y = diffs)
# ) +
#   geom_boxplot()
# 
# ## Improve Plot by Faceting
# ggplot(
#   data = simResults,
#   mapping = aes(x = distribution, y = diffs)
# ) +
#   geom_boxplot() +
#   facet_wrap(facets = vars(sampleSize))
# 
# ## Try Histogram
# ggplot(
#   data = simResults,
#   mapping = aes(x = diffs)
# ) +
#   geom_histogram(
#     binwidth = 0.2
#   ) +
#   facet_grid(
#     rows = vars(distribution),
#     cols = vars(sampleSize),
#     scales = "free_y"
#   )

## Polish Visualization ----
## I anticipate students will gravitate towards the three geometries of
## histograms, boxplots, and density plots.
## Improve labels and scaling
## Add a reference line for a difference of 0
### Histograms ----
ggplot(
  data = simResults,
  mapping = aes(x = diffs)
) +
  geom_histogram(
    fill = "black",
    color = "white",
    closed = "left",
    boundary = 0,
    binwidth = 0.2
  ) +
  geom_vline(
    xintercept = 0,
    color = "limegreen"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1))
  ) +
  facet_grid(
    rows = vars(distribution),
    cols = vars(sampleSize),
    scales = "free_y"
  ) +
  labs(
    x = "Difference, S. Median - SAM",
    y = "Frequency",
    title = "Histogram of Monte Carlo Simulation Results"
  ) +
  theme_bw()

### Box Plots ----
ggplot(
  data = simResults,
  mapping = aes(y = diffs)
) +
  geom_boxplot() +
  geom_hline(
    yintercept = 0,
    color = "limegreen"
  ) +
  facet_grid(
    rows = vars(distribution),
    cols = vars(sampleSize),
    scales = "free"
  ) +
  labs(
    y = "Difference, S. Median - SAM",
    title = "Box Plots of Monte Carlo Simulation Results"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

### Density plots ----
ggplot(
  data = simResults,
  mapping = aes(x = diffs)
) +
  geom_density() +
  geom_vline(
    xintercept = 0,
    color = "limegreen"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1))
  ) +
  facet_grid(
    rows = vars(distribution),
    cols = vars(sampleSize),
    scales = "free"
  ) +
  labs(
    x = "Difference, S. Median - SAM",
    y = "Density",
    title = "Density Plots of Monte Carlo Simulation Results"
  ) +
  theme_bw()
