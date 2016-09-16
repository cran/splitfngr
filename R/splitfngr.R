#' Calculate function and gradient together but access separately.
#' Reduces computation since they share data in calculation.
#' Doesn't have to be function and gradient, can be any two
#' values calculated together but accessed separately.
#' Useful in optimization when function evaluation is expensive
#' since the chain rule means many parts of function and gradient
#' are the same.
#'
#' @param fn_gr A function that returns a list of two values.
#' Both are calculated when fn is called, but only the first
#' is returned. The second is returned when gr is called
#' but nothing is recalculated.
#'
#' @return An environment with two functions, fn and gr.
#' @export
#'
#' @examples
#' quad_share <- function(x){list(sum(x^4), 4*x^3)}
#' share <- grad_share(quad_share)
#' share$fn(1)
#' share$gr(1)
#' share$gr(2)
#' share$fn(2)
#' share$gr(2)
grad_share <- function(fn_gr) {
  env <- new.env()
  env$x_last <- NULL
  env$fn <- function(x) {
    if (is.null(env$x_last) || env$x_last != x) {
      out <- fn_gr(x)
      env$x_last <- x
      env$fn_val <- out[[1]]
      env$gr_val <- out[[2]]
    }
    env$fn_val
  }
  env$gr <- function(x = NULL) {
    if (is.null(env$x_last) || env$x_last != x) {
      out <- fn_gr(x)
      env$x_last <- x
      env$fn_val <- out[[1]]
      env$gr_val <- out[[2]]
    }
    env$gr_val
  }
  env
}


#' Access a list of values separately but calculate them together.
#' This function generalizes grad_share for any number of functions.
#'
#' @param func Function that returns a list of values
#' @param evalForNewX Should the function reevaluate for any new x?
#' Recommended.
#' @param check_all Should it check that the accessed values were
#' calculated at the current input? Ignored if evalForNewX is true.
#' Will give a warning but still return the stored value.
#' @param recalculate_indices Indices for which the values should
#' be recalculated. Ignored if evalForNewX is true. Use this if
#' you don't want to pass x to dependent functions, or if you know
#' other indices won't need to be recalculated.
#'
#'
#' @return An environment where the function values are calculated.
#' @export
#'
#' @examples
#' tfunc <- function(x) {list(x+1, x+2, x+3, x+4, x+5)}
#' f <- fngr(tfunc)
#' f(1)(0)
#' f(3)(0)
#' f(3)(1)
#' f(1)(23.4)
#' f(4)()
#'
#' # Use same function but only recalculate when first value is called
#' g <- fngr(tfunc, evalForNewX = FALSE, recalculate_indices = c(1))
#' g1 <- g(1)
#' g3 <- g(3)
#' g1(1)
#' g3(1)
#' g3(11) # This won't be give expected value
#' g1(11) # This updates all values
#' g3(11) # This is right
fngr <- function(func, evalForNewX=TRUE,
                 recalculate_indices = c(),
                 check_all=FALSE
                 ) {
  if (evalForNewX == F & length(recalculate_indices) == 0) {
    stop("Values will never be calculated")
  }
  env <- new.env()
  env$x_last <- NULL
  env$f <- function(i, evalForNewX_=evalForNewX,
                    recalculate = any(i==recalculate_indices),
                    check=check_all
                    ) {
    function(x=NULL, check_now=check, recalculate_now=recalculate,
             evalForNewX_now=evalForNewX_
             ) {
      if (recalculate_now ||
          (evalForNewX_now &&
           ((is.null(env$x_last) && !is.null(x)) ||
            (!is.null(env$x_last) && !is.null(x) && x != env$x_last)
            ))
        ) {
        out <- func(x)
        env$x_last <- x
        env$out <- out
        out[[1]]
      } else {
        # Can check if evaluated at same value, but will only slow it down
        if (check_now) {
          if (!is.null(x) &&
              !any(is.nan(x)) &&
              (is.null(env$x_last) || x != env$x_last)) {
            warning("gr called at different x than fn")
          }
        }
      }
      env$out[[i]]
    }
  }
  env$f
}


#' Convert a function from multiple function arguments to a
#' single function
#'
#' @param func The function that takes in two function arguments
#' @param arg_fn The function (first) argument name of func
#' @param arg_gr The gradient (second) argument name of func
#'
#' @return A new function that evaluates the two arguments together
#' @export
#'
#' @examples
#' quad_share <- function(x){list(sum(x^4), 4*x^3)}
#' lbfgs_share <- make_share(lbfgs::lbfgs, 'call_eval', 'call_grad')
#' make_share(lbfgs::lbfgs, 'call_eval', 'call_grad')(quad_share, vars=c(5,-4))
make_share <- function(func, arg_fn, arg_gr) {
  function(fngr, ...) {
    env <- grad_share(fngr)
    args_list <- list(env$fn, env$gr, ...)
    names(args_list)[1] <- arg_fn
    names(args_list)[2] <- arg_gr
    do.call(what=func, args=args_list)
  }
}
