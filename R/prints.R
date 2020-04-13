#' @export
print.unbANOVA <- function(g){
  print(g[1:4])
  invisible(g)
}

#' @export
summary.unbANOVA <- function(g){
  ans <- list()
  ans$`Marginal Means` <- cbind(`ANOVA I` = g$anova1$marginalMeans, `ANOVA II` = g$anova2$marginalMeans, `ANOVA III` = g$anova3$marginalMeans, `ATE` = g$ATE$marginalMeans)
  ans$`Sum of Squares` <- matrix(c(g$anova1$SS, g$anova2$SS, g$anova3$SS, g$ATE$SS), 1, dimnames = list("     ", c("ANOVA I", "ANOVA II", "ANOVA III", "ATE")))
  ans$`Is data balanced?` <- g$attr$isBalanced
  ans$`Is data proportional?` <- g$attr$isProportional
  ans$`Is data free of interaction?` <- g$attr$isInteractionfree
  class(ans) <- "summary.unbANOVA"
  ans
}

#' @export
effects.unbANOVA <- function(g, reference.group = NULL){
  if(is.null(reference.group) || reference.group == 0){
    return(cbind(`ANOVA I` = g$anova1$effects, `ANOVA II` = g$anova2$effects, `ANOVA III` = g$anova3$effects, `ATE` = g$ATE$effects))
  } else if(reference.group %in% 1:length(g$anova1$marginalMeans)) {
    return(cbind(`ANOVA I` = setNames(g$anova1$marginalMeans[-reference.group] - g$anova1$marginalMeans[reference.group], paste0((1:length(g$anova1$marginalMeans))[-reference.group], " - ", reference.group)),
                 `ANOVA II` = setNames(g$anova2$marginalMeans[-reference.group] - g$anova2$marginalMeans[reference.group], paste0((1:length(g$anova2$marginalMeans))[-reference.group], " - ", reference.group)),
                 `ANOVA III` = setNames(g$anova3$marginalMeans[-reference.group] - g$anova3$marginalMeans[reference.group], paste0((1:length(g$anova3$marginalMeans))[-reference.group], " - ", reference.group)),
                 `ATE`     = setNames(g$ATE$marginalMeans[-reference.group] - g$ATE$marginalMeans[reference.group], paste0((1:length(g$ATE$marginalMeans))[-reference.group], " - ", reference.group))))
  } else {
    stop("'reference.group' not valid level of X.")
  }
}

#' @export
print.summary.unbANOVA <- function(g){
  cat("Marginal Means:\n\n")
  print(round(g$`Marginal Means`, 2))

  cat("\n\nSum of Squares:\n\n")
  print(round(g$`Sum of Squares`, 2))

  cat("\n\n")
  if(g$`Is data balanced?`){cat("Data is balanced.\t")}else{cat("Data is not balanced.\t")}
  if(g$`Is data proportional?`){cat("Data is proportional.\n")}else{cat("Data is not proportional.\n")}
  if(g$`Is data free of interaction?`){cat("Data is free of treatment-covariate-interaction (x*K).\n")}else{cat("Data has treatment-covariate-interaction (x*K).\n")}
  invisible(g)
}
