library(tidyverse)
library(rstan)
library(boot)

relu <- Vectorize(function(x) max(0,x))
log1p_exp <- Vectorize(function(x) log(1+exp(x)))

tibble(x=c(-1,1)) %>% ggplot(aes(x)) + stat_function(fun = relu) + stat_function(fun = log1p_exp)

get_nn_lik <- function(df) {
attach(df)
nn_lik <- function(theta1, theta2, c, w) {
  h1 <- log1p_exp(cos(theta1)*x1 + sin(theta1)*x2 - c[1])
  h2 <- log1p_exp(cos(theta2)*x1 + sin(theta2)*x2 - c[2])
  yhat <- w[1]*h1 + w[2]*h2
  lik <- sum((ynoise-yhat)^2)
  
  return(lik)
}
}

l <- get_nn_lik(synth)

l(0,0,c(1,2),c(1,2))

nn <- function(x1,x2) {
  h1 <- log1p_exp((x1+x2)/sqrt(2))
  h2 <- log1p_exp((x1+x2)/sqrt(2)-5)
  y <- h1-2*h2
}

nn_grid <- tibble(x1 = seq(0,7,by=0.01), x2 = seq(0,7,by=0.01)) %>%
  expand(x1,x2) %>%
  mutate(Wx1 = (x1+x2)/sqrt(2), Wx2 = (x1+x2)/sqrt(2)-1) %>%
  mutate(h1_relue = relu(Wx1), h2_relue = relu(Wx2)) %>%
  mutate(h1_logexp = log1p_exp(Wx1), h2_logexp = log1p_exp(Wx2)) %>%
  mutate(f = nn(x1,x2))

nn_grid %>% ggplot(aes(x1,x2)) + geom_raster(aes(fill = f))

N <- 20
synth <- tibble(x1 = runif(N,0,7), x2 = runif(N,0,7)) %>%
  mutate(h1 = log1p_exp((x1+x2)/sqrt(2)), h2 = log1p_exp((x1+x2)/sqrt(2)-5)) %>%
  mutate(y = h1-2*h2) %>%
  mutate(ynoise = y + rnorm(N, 0, 0.1))

synth %>% ggplot(aes(x1,x2, color = y)) + geom_point()

dat <- list(N = N, X = cbind(synth$x1, synth$x2), y = synth$ynoise)
fit <- stan("bayesian_nn.stan", data = dat, chains = 1, iter = 2000, init = list(list(W_raw = matrix(c(1/sqrt(2),1/sqrt(2),1/sqrt(2),1/sqrt(2)),2), w = c(1,-2))))
fit <- stan("bayesian_nn_angle.stan", data = dat, chains = 8, iter = 2000)

s <- extract(fit)
posterior_draw <- function(s,d) {
  
  theta1 <- atan2(s$W[d,2,1], s$W[d,1,1])/pi
  theta2 <- atan2(s$W[d,2,2], s$W[d,1,2])/pi
  
  list(theta1 = theta1, theta2 = theta2, c = s$c[d,], w = s$w[d,])
}

posterior_draw(s,sample(1:1000,1))

