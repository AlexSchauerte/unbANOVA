#' Check for proportional data
#'
#' This function tests if frequency matrix is proportional (all frequencies are proportional within the x-levels).
#' @param freq matrix of frequencies
#' @keywords anova
#' @export
#' @examples
#' isProportional(matrix(c(33,27,43,38), 2, 2, T))
#' isProportional(matrix(c(5,10,15,30), 2, 2, T))
isProportional <- function(freq){
  if(ncol(freq) == 1) return(NA) 
  
  all(unlist(sapply(1:(ncol(freq) - 1), function(i){
    apply(sapply((i + 1):ncol(freq), function(j){freq[,i]/freq[,j]}), 2, function(x){length(unique(x)) == 1})
  })))
}
