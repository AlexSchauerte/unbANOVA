#' Find the columns based on the fixed levels of the covariates
#'
#' This function finds the columns .
#' @param fixed numeric vector with the levels the corresponding covariates should be fixed to (NA for no fixing)
#' @param k.levels numeric vector with the number of levels for all covariates (K1, K2, ...)
#' @keywords anova
#' @export
findConditionalColumns <- function(fixed, k.levels){
  which(apply(rev(expand.grid(sapply(rev(k.levels),  seq))), 1, function(i){
    all(sapply(1:length(fixed), function(x){
      if(is.na(fixed[x])) return(T)
      if(i[x] == fixed[x]) return(T)
      return(F)
    }))
  }))
}
