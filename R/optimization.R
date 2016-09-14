#' Use R's optim function but pass in a single function
#' that returns both the function and gradient
#' together in a list. Useful when the function and
#' gradient are expensive to calculate and can be
#' calculated faster together than separate.
#'
#' @param par Initial values for the parameters to be optimized over.
#'        Will be passed to optim as par argument.
#' @param fngr A function that returns a list of two elements:
#'        the function value and the gradient value.
#' @param ... Arguments passed to optim, such as method,
#'        lower, upper, control, and hessian.
#'
#' @return Results from running optim
#' @export
#' @importFrom stats optim
#'
#' @examples
#' quad_share <- function(x){list(sum(x^4), 4*x^3)}
#' optim_share(par=c(3, -5), quad_share, method="BFGS")
optim_share <- function(par, fngr, ...) {
  env <- grad_share(fngr)
  optim(par=par, fn=env$fn, gr=env$gr, ...)
}


#' Use lbfgs packages's lbfgs function but pass in a single function
#' that returns both the function and gradient
#' together in a list. Useful when the function and
#' gradient are expensive to calculate and can be
#' calculated faster together than separate.
#'
#' @param fngr A function that returns a list of two elements:
#'        the function value and the gradient value.
#' @param vars Initial values for the parameters to be optimized over.
#'        Will be passed to optim as par argument.
#' @param ... Other arguments passed to lbfgs
#'
#' @return Result from running lbfgs on the given function
#' @export
#'
#' @examples
#' quad_share <- function(x){list(sum(x^4), 4*x^3)}
#' lbfgs_share(vars=c(3, -5), fngr=quad_share)
lbfgs_share <- function(fngr, vars, ...) {
  env <- grad_share(fngr)
  lbfgs::lbfgs(call_eval=env$fn, call_grad=env$gr, vars=vars, ...)
}
