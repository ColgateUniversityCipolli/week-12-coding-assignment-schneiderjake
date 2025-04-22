library(tidyverse)
library(VGAM)
library(boot)
library(xtable)



##############################################################################
#step 1
##############################################################################


c20 <- qt(.95, df = 19)
print(c20)

c30 <- qt(.95, df=29)
print(c30)

a <- 0
b <- 4
n.sims <- 10000

#function
sim.experiment <- function() {
  #30 samples
  observations <- rlaplace(30, location=a, scale=b)
  
  #t stat month 20
  mean.20 <- mean(observations[1:20])
  sd.20 <- sd(observations[1:20])
  t.20 <- (mean.20 - 0) / (sd.20 / sqrt(20))
  
  #rejection check at month 20
  if (t.20 > c20) {
    return(1)  #reject H0 (early stop)
  }
  
  #t stat month 30
  mean.30 <- mean(observations)
  sd.30 <- sd(observations)
  t.30 <- (mean.30 - 0) / (sd.30 / sqrt(30))
  
  #reject check at month 30
  if (t.30 > c30) {
    return(1)  #reject H0
  } else {
    return(0)  #fail to reject H0
  }
}

#run the simulation
results <- replicate(n.sims, sim.experiment())

#calc Type I error rate
type_I_error <- mean(results)
print(paste("Type I error rate:", round(type_I_error, 4)))



##############################################################################
#step 2
##############################################################################

n <- 15
n.sims <- 10000
alpha <- 0.05

# Define test cases
distributions <- tibble(
  dist = c("Beta(10,2)", "Beta(2,10)", "Beta(10,10)"),
  shape1 = c(10, 2, 10),
  shape2 = c(2, 10, 10),
  true_mean = c(10/12, 2/12, 0.5)
)

# Run simulations
simulate_errors <- function(shape1, shape2, mu) {
  rerun(n.sims, {
    x <- rbeta(n, shape1, shape2)
    tibble(
      left  = t.test(x, mu = mu, alternative = "less")$p.value < alpha,
      right = t.test(x, mu = mu, alternative = "greater")$p.value < alpha,
      two   = t.test(x, mu = mu, alternative = "two.sided")$p.value < alpha
    )
  }) |>
    bind_rows() |>
    summarise(
      `Left Tailed` = mean(left),
      `Right Tailed` = mean(right),
      `Two Tailed` = mean(two)
    )
}

# Map simulation to each row of the distributions table
results <- distributions |>
  rowwise() |>
  mutate(sim = list(simulate_errors(shape1, shape2, true_mean))) |>
  unnest(sim)|> 
  select(Distribution = dist, `Left Tailed`, `Right Tailed`, `Two Tailed`) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

