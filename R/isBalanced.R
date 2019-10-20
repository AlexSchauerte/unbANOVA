#' Check for balanced data
#'
#' This function tests if frequency matrix is balanced (all frequencies are equal).
#' @param freq matrix of frequencies
#' @keywords anova
#' @export
#' @examples
#' isBalanced(matrix(c(33,27,43,38), 2, 2, T))
#' isBalanced(matrix(c(30,30,30,30), 2, 2, T))
isBalanced <- function(freq){
  length(unique(as.numeric(freq))) == 1
}
