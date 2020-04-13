#' Calculate the marginal means of different ANOVA types
#'
#' This function calculates the marginal means of x according to the different ANOVA types
#' @param means matrix of means, the columns denote the covariates and the rows the different x levels
#' @param freq matrix of frequencies, the columns denote the covariates and the rows the different x levels
#' @param k.levels numeric vector with the number of levels for all covariates (K1, K2, ...)
#' @param type numeric or character vector denoting the ANOVA types to be calculated (allowed values are any of the following: "I", 1, "II", 2, "III", 3, "ATE")
#' @param fixed numeric vector with the levels the corresponding covariates should be fixed to (NA for no fixing)
#' @keywords anova
#' @export
marginalMeans <- function(means, freq, k.levels = NULL, type = c("I", "II", "III", "ATE"), fixed = NA){
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
    means <- matrix(means[,unbANOVA::findConditionalColumns(fixed, k.levels)], nrow = nrow(me))
    freq <- matrix(freq[,unbANOVA::findConditionalColumns(fixed, k.levels)], nrow = nrow(me))
    k.levels <- k.levels[-which(!(is.na(fixed)))]
    
    if(length(k.levels) == 0) k.levels <- 1
  }

  #cat(paste0("k.levels = ", paste0(k.levels, collapse = ""), " [marginalMeans]\n"))
  
  # Anova II
  if(2 %in% type | "II" %in% type){
    means_if <- unbANOVA::calcInteractionfreeDataset(means, freq, k.levels)
  }

  return(purrr::compact(list(
    anova1 = if(1 %in% type | "I" %in% type) setNames(if(length(k.levels) == 1 & k.levels[1] == 1){as.numeric(means)}else{colSums(sapply(1:nrow(means), function(x){means[x,]*(freq[x,]/sum(freq[x,]))}))}, paste0("X = ", 1:nrow(means))),
    anova2 = if(2 %in% type | "II" %in% type) setNames(if(length(k.levels) == 1 & k.levels[1] == 1){as.numeric(means)}else{rowMeans(means_if)}, paste0("X = ", 1:nrow(means))),
    anova3 = if(3 %in% type | "III" %in% type) setNames(if(length(k.levels) == 1 & k.levels[1] == 1){as.numeric(means)}else{rowMeans(means)}, paste0("X = ", 1:nrow(means))),
    ATE    = if("ATE" %in% type) setNames(if(length(k.levels) == 1 & k.levels[1] == 1){as.numeric(means)}else{colSums(sapply(1:nrow(means), function(x){means[x,] * (colSums(freq)/sum(freq))}))}, paste0("X = ", 1:nrow(means)))
  )))
}
