#' Check for interaction-free data
#'
#' This function tests if data is interaction-free (x-effects, K-effects, but no x*K-effects).
#' @param freq matrix of frequencies
#' @keywords anova
#' @export
isInteractionFree <- function(means, freq, k.levels){
  all(means == calcInteractionfreeDataset(means, freq, k.levels))
}
