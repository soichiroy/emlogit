


##
## ANOVA logit
##

#' Logistic regression with ANOVA style parametrization.
#' @aliases anova_logit
#' @param option A list of options. \code{pvec} should be provided.
#' @example examples/anova_logit-example.R
#' @export
#' @rdname anova_logit
anova_logit <- function(...) {
    UseMethod("anova_logit") 
}

#' @param formula An object of class "formula": 
#'  symbolic description of the model to be fitted in the form
#'  \code{y ~ x} for the binary logit or 
#'  \code{y | n ~ x} for the binomial regression where \code{y} is 
#'  \code{n} is the trial in binomial case.
#' @param data Data frame.
#' @importFrom purrr map
#' @export
#' @rdname anova_logit
anova_logit.default <- function(formula, data, option = list()) {
  
  ## -------------------------------------------
  ## set the default value of the option parameters
  option <- al_set_option(option)
  
  ## prepare data 
  dat_input <- al_data(formula, data, option$adaptive_weight)
  option$pvec <- dat_input$pvec
  option$wj   <- dat_input$wj
  
  ## fit 
  fit <- anova_logit.matrix(
    y = dat_input$y,
    X = dat_input$X,
    trials = dat_input$trials, 
    option = option 
  )
  
  ## name coefficients 
  names(fit$coef) <- c("intercept", unlist(map(dat_input$nj, ~names(.x))))

  return(fit)
}

#' @param y A vector of binary responses. In the binomial response, this corresponds to "success".
#' @param X A matrix of overparametrized design matrix.
#' @param trials A vector of trial counts. In the binary outcome this is left as \code{NULL}.
#'  The lengh of this vector should match the length of \code{y}.
#' @rdname anova_logit
#' @keywords internal
anova_logit.matrix <- function(y, X, trials = NULL, option = list()) {


  ## EM ----------------------------------------
  params <- al_em_run(y, X, trials, option)

  ## fitted value ------------------------------
  betas  <- params$coefficient
  params$fitted <- 1 / (1 + exp(-as.vector(X %*% betas)))

  ## return estimates --------------------------
  attr(params, "option") <- option
  class(params)          <- c("anova_logit")
  return(params)
}
