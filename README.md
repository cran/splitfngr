
<!-- README.md is generated from README.Rmd. Please edit that file -->
splitfngr
=========

[![Travis-CI Build Status](https://travis-ci.org/CollinErickson/splitfngr.svg?branch=master)](https://travis-ci.org/CollinErickson/splitfngr)

Sometimes multiple values are returned by a single function. This makes sense when their combined evaluation is faster than separate evaluations, such as function and corresponding gradient values which require solving the same matrix system. If the user wants to have separate functions for the function and gradient, such as when passing to an optimization routine.

Functions provided in this package allow this to be done in a general situation.

Below is an example. The R function `optim` with method BFGS requires a function for the function value and gradient value separately. In this trivial example the gradient value is not recalculated when it is called.

``` r
# A function that returns
quad_share <- function(x){list(sum(x^4), 4*x^3)}
# Run BFGS using function with gradient
splitfngr::optim_share(par=c(3, -5), quad_share, method="BFGS")
#> $par
#> [1] -0.0003599207  0.0006973696
#> 
#> $value
#> [1] 2.532927e-13
#> 
#> $counts
#> function gradient 
#>       42       36 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> NULL
```

See the documentation for more examples and details on usage.
