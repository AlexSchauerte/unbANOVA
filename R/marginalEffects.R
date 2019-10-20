#' Calculate the effects of different ANOVA types
#'
#' This function calculates the effects (differences in marginal means) of x according to the different ANOVA types
#' @param means matrix of means, the columns denote the covariates and the rows the different x levels
#' @param freq matrix of frequencies, the columns denote the covariates and the rows the different x levels
#' @param k.levels numeric vector with the number of levels for all covariates (K1, K2, ...)
#' @param type numeric or character vector denoting the ANOVA types to be calculated (allowed values are any of the following: "I", 1, "II", 2, "III", 3, "ATE")
#' @param fixed numeric vector with the levels the corresponding covariates should be fixed to (NA for no fixing)
#' @keywords anova
#' @export
marginalEffects <- function(means, freq, k.levels = NULL, type = c("I", "II", "III", "ATE"), fixed = NULL){
  # checks
  if(!(is.matrix(means))) stop("'means' is not a matrix.")
  if(!(is.matrix(freq))) stop("'freq' is not a matrix.")
  if(!(is.numeric(means))) stop("'means' is not numeric.")
  if(!(is.numeric(freq))) stop("'freq' is not numeric.")
  if(nrow(means) != nrow(freq) | ncol(means) != ncol(freq)) stop(paste0("'means' (", nrow(means), "x", ncol(means), ") and 'freq' (", nrow(freq), "x", ncol(freq),") do not hATE the same dimensions."))
  if(is.null(k.levels) & (2 %in% type | "II" %in% type)) {
    warning("'k.levels' not given, but it is recommended when calculating type 2, because the results depend on this. Will assume that only one K (values: ", paste0(1:ncol(means), collapse = " "),") is present.")
    k.levels <- ncol(means)
  }
  if(!(is.numeric(k.levels)) & (2 %in% type | "II" %in% type)) stop("'k.levels' is not numeric.")
  if((prod(k.levels) != ncol(means)) & (2 %in% type | "II" %in% type)) stop("Number of columns is not equal to the product of 'k.levels'.")
  if(!(any(type %in% c("I", 1, "II", 2, "III", 3, "ATE")))) stop("No value in 'type' correspends to 'I', 1, 'II', 2, 'III', 3 or 'ATE'.")
  if(!(all(type %in% c("I", 1, "II", 2, "III", 3, "ATE")))){
    warning(paste0(paste0(type[!(type %in% c("I", 1, "II", 2, "III", 3, "ATE"))], collapse = ", "), " can't be understood as a value in 'type'. Will ignore these."))
    type <- type[type %in% c("I", 1, "II", 2, "III", 3, "ATE")]
  }
  if(!(all(is.na(fixed)))){
    means <- matrix(means[,findConditionalColumns(fixed, k.levels)], nrow = nrow(me))
    freq <- matrix(freq[,findConditionalColumns(fixed, k.levels)], nrow = nrow(me))
    k.levels <- k.levels[-which(!(is.na(fixed)))]
  }

  #cat(paste0("k.levels = ", paste0(k.levels, collapse = ""), " [marginalEffects]\n"))

  effectCalc <- function(mMeans, x.levels = nrow(means)){
    unlist(sapply(1:(x.levels-1), function(x){sapply((x+1):x.levels, function(y){setNames(mMeans[y] - mMeans[x], paste0(x," -> ", y))})}))
  }

  return(purrr::compact(list(
    anova1 = if(1 %in% type | "I" %in% type) effectCalc(marginalMeans(means, freq, k.levels = k.levels, type = 1)[[1]]),
    anova2 = if(2 %in% type | "II" %in% type) effectCalc(marginalMeans(means, freq, k.levels = k.levels, type = 2)[[1]]),
    anova3 = if(3 %in% type | "III" %in% type) effectCalc(marginalMeans(means, freq, k.levels = k.levels, type = 3)[[1]]),
    ATE    = if("ATE" %in% type) effectCalc(marginalMeans(means, freq, k.levels = k.levels, type = "ATE")[[1]])
  )))
}
