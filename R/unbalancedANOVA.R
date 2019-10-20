#' Show differences between all ANOVA types
#'
#' This function calculates the marginal means, effects (differences in marginal means) and sum of squares of x according to the different ANOVA types. Additionaly it calculates different attributes (Is data balanced? Is data proportional? ...) and function call information (What was the means- and freq-matrix? ...)
#' @param means matrix of means, the columns denote the covariates and the rows the different x levels
#' @param freq matrix of frequencies, the columns denote the covariates and the rows the different x levels
#' @param k.levels numeric vector with the number of levels for all covariates (K1, K2, ...)
#' @param type numeric or character vector denoting the ANOVA types to be calculated (allowed values are any of the following: "I", 1, "II", 2, "III", 3, "ATE")
#' @param fixed numeric vector with the levels the corresponding covariates should be fixed to (NA for no fixing)
#' @keywords anova
#' @export
unbalancedANOVA <- function(means, freq, k.levels = NULL, type = c("I", "II", "III", "ATE"), fixed = c(NA,NA,NA)){
  # checks
  if(!(is.matrix(means))) stop("'means' is not a matrix.")
  if(!(is.matrix(freq))) stop("'freq' is not a matrix.")
  if(!(is.numeric(means))) stop("'means' is not numeric.")
  if(!(is.numeric(freq))) stop("'freq' is not numeric.")
  if(nrow(means) != nrow(freq) | ncol(means) != ncol(freq)) stop(paste0("'means' (", nrow(means), "x", ncol(means), ") and 'freq' (", nrow(freq), "x", ncol(freq),") do not have the same dimensions."))
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
    means <- matrix(means[,findConditionalColumns(fixed, k.levels)], nrow = nrow(means))
    freq <- matrix(freq[,findConditionalColumns(fixed, k.levels)], nrow = nrow(means))
    k.levels <- k.levels[-which(!(is.na(fixed)))]
    
    if(length(k.levels) == 0) k.levels <- 1
    #cat(paste0("k.levels = ", paste0(k.levels, collapse = ""), " [unbalancedANOVA]\n"))
  }

  marginalMeans <- marginalMeans(means = means, freq = freq, k.levels = k.levels, type = type)
  effects <- marginalEffects(means = means, freq = freq, k.levels = k.levels, type = type)
  ss <- marginalSS(means = means, freq = freq, k.levels = k.levels, type = type)

  return(structure(.Data = purrr::compact(list(
          anova1 = if(1 %in% type | "I" %in% type) list(marginalMeans = marginalMeans$anova1, effects = effects$anova1, SS = ss$anova1),
          anova2 = if(2 %in% type | "II" %in% type) list(marginalMeans = marginalMeans$anova2, effects = effects$anova2, SS = ss$anova2),
          anova3 = if(3 %in% type | "III" %in% type) list(marginalMeans = marginalMeans$anova3, effects = effects$anova3, SS = ss$anova3),
          ATE    = if("ATE" %in% type) list(marginalMeans = marginalMeans$ATE, effects = effects$ATE, SS = ss$ATE),
          attr   = list(isBalanced = isBalanced(freq), isProportional = isProportional(freq), isInteractionfree = isInteractionFree(means, freq, k.levels), interactionfreeMeans = calcInteractionfreeDataset(means, freq, k.levels)),
          call   = list(means = means, frequencies = freq, x.levels = nrow(means), k.levels = k.levels, type = type, fixed = fixed)
         )),
         class = "unbANOVA"))
}
