map_estimator <- function ()
{
  y <- rnorm(100, 0, sqrt(5))
  map_estimate = (sum(y^2)+1)/100
  return (map_estimate)
}  
prior <- function(ssquare)
{
  #constant in the prior found from the fact that integral of prior must be 1
  return (1/7.519885*(ssquare^(-7/2)*exp(-1/2/ssquare)))
}
posterior_proportional <- function(ssquare, data, n)
{
  return (ssquare^(-1/2*(n+7))*exp(-(sum(data^2)+1)/2/ssquare))
}
posterior <- function(x, data, n)
{
  return (posterior_proportional(x, data, n)/integrate(posterior_proportional, lower = 0, upper = Inf, data, n))  
}
bayes_estimate <- function()
{
  n <- 100
  y <- rnorm(n, 0, sqrt(5))
  estimator_function <- function(x)
  {
    return (x*posterior(x, y, n))
  }
  return (integrate(estimator_function, 0, Inf))
}