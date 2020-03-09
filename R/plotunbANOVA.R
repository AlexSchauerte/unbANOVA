#' Plot the results of unbalancedANOVA.
#'
#' This function plots the results in nice graphs.
#' @param results object of type 'unbANOVA', result of \link{unbalancedANOVA}
#' @param type numeric or character denoting result of which ANOVA type to be ploted (allowed values are any of the following: "I", 1, "II", 2, "III", 3, "ATE")
#' @param XorK "X" or "K", denoting which variable should be on the x-axis of the plot
#' @param showWeights boolean, should Weights be added to means?
#' @param showMM boolean, should marginal means be plotted?
#' @param showEffects boolean, should effects be plotted?
#' @param showIFmeans boolean, should interactionfree means be plotted for type 2?
#' @keywords anova
#' @export
plotunbANOVA <- function(results, type, XorK = "K", showWeights = TRUE, showMM = TRUE, showEffects = FALSE, showIFmeans = FALSE){
  if(class(results) != "unbANOVA") stop("'results' is not of class 'unbANOVA'. Please use result of the 'unbalancedANOVA'-function.")
  if(length(type) != 1) stop("Please give exactly one type.")
  if(!(type %in% c("I", 1, "II", 2, "III", 3, "ATE"))) stop("Type not recognized, please use one of the following: 'I', 1, 'II', 2, 'III', 3, 'ATE'")
  if(is.numeric(type)) type <- switch(type, "I", "II", "III")
  if(!(type %in% results$call$type)) stop("Type was not calculated in these results.")
  if(XorK != "X" & XorK != "K") stop("'XorK' should be either 'K' or 'X'.")
  
  data <- list(graph = cbind(
                        setNames(rev(expand.grid(sapply(rev(c(results$call$x.levels, results$call$k.levels)), seq, simplify = F))), c("x", paste0("K", 1:length(results$call$k.levels)))),
                        list(freq = as.numeric(t(results$call$freq)), means = as.numeric(t(results$call$means)), ifmeans = as.numeric(t(results$attr$interactionfreeMeans)), mm = rep(switch(type, "I" = results$anova1$marginalMeans, "II" = results$anova2$marginalMeans, "III" = results$anova3$marginalMeans, "ATE" = results$ATE$marginalMeans), each = prod(results$call$k.levels)))
                       ),
               names = do.call(paste0, rlist::list.flatten(rlist::list.expand(sapply(1:length(results$call$k.levels), function(i){paste0("K",i," = ", rep(1:results$call$k.levels[i], each = prod(results$call$k.levels[-c(1:i)])))}, simplify = FALSE), list(c(", "))))[1:(length(results$call$k.levels) * 2- 1)])
  )
  data$graph$x <- as.factor(data$graph$x)
  data$graph$K <- as.factor(gsub(", ", "\n", rep(data$names, results$call$x.levels)))
  data$graph$K_mean <- rep(sapply(1:results$call$x.levels, function(i){sum(1:prod(results$call$k.levels) * data$graph$freq[data$graph$x == i]/sum(data$graph$freq[data$graph$x == i]))}), each = prod(results$call$k.levels))
  
  colors <- hcl(h = seq(15, 375, length = results$call$x.levels + 1), l = 55, c = 100)[1:results$call$x.levels]
  unval <- 1:results$call$x.levels * prod(results$call$k.levels)
  
  plots <- list(
    points = if(showWeights){ggplot2::geom_point(size = 30*(data$graph$freq/sum(data$graph$freq)) + 5, alpha = if(type == "II"){0.35}else{1})} else {ggplot2::geom_point(size = 7, alpha = if(type == "II"){0.35}else{1})},
    lines = ggplot2::geom_line(ggplot2::aes(group = x)),
    mm = if(showMM){list(ggplot2::annotate("point", y = data$graph$mm[unval], x = data$graph$K_mean[unval], shape = 15, size = 4, color = colors), ggplot2::annotate("text", y = data$graph$mm[unval] - (max(data$graph$means) - min(data$graph$means))/30, x = data$graph$K_mean[unval], size = 4, color = colors, label = round(data$graph$mm[unval], 2)))},
    effects = if(showEffects){list(ggplot2::geom_hline(mapping = ggplot2::aes(yintercept=mm[unval][1]), linetype="longdash", color = colors[1]),lapply(2:results$call$x.levels, function(i){ggplot2::geom_segment(mapping = ggplot2::aes(y=mm[unval][1], yend=mm[unval[i]], x = K_mean[unval[i]], xend = K_mean[unval[i]]), color = colors[i])}))},
    shadowPoints = ggplot2::geom_point(mapping = ggplot2::aes(y=means), alpha = 0.35, size = 30*(rep(colSums(results$call$freq), times = results$call$x.levels)/sum(data$graph$freq)) + 5)
  )
  
  switch(XorK,
         "X" = switch(type,
                      "I"   = ggplot2::ggplot(data = data$graph, mapping = ggplot2::aes(x=x)) +
                               ggplot2::geom_line(mapping = ggplot2::aes(y=means, color = rep(data$names, results$call$x.levels))) +
                               ggplot2::geom_point(mapping = ggplot2::aes(y=means, color = rep(data$names, results$call$x.levels)), size = if(showWeights){30*(data$graph$freq/sum(data$graph$freq)) + 5}else{5}) +
                               ggplot2::geom_point(mapping = ggplot2::aes(y=anova1, color = "Anova I", shape = 15), size = 4) +
                               ggplot2::scale_shape_identity() +
                               ggplot2::geom_text(mapping = ggplot2::aes(y=anova1, color = "Anova I"), label = rep(round(results$anova1$marginalMeans, 2), each = prod(results$call$k.levels)), nudge_y = -(max(data$graph$means) - min(data$graph$means))/30, size = 4) +
                               ggplot2::scale_x_discrete(name = "", limits = 1:results$call$x.levels, labels = paste0("x = ", 1:results$call$x.levels)) +
                               ggplot2::labs(color = ggplot2::element_blank()),
                      "II"  = ggplot2::ggplot(data = data$graph, mapping = ggplot2::aes(x=x)) +
                               ggplot2::geom_line(mapping = ggplot2::aes(y=means, color = rep(data$names, results$call$x.levels)), alpha = .5) +
                               ggplot2::geom_point(mapping = ggplot2::aes(y=means, color = rep(data$names, results$call$x.levels)), alpha = .5, size = 30*(data$graph$freq/sum(data$graph$freq)) + 5) +
                               ggplot2::geom_point(mapping = ggplot2::aes(y=anova2, color = "Anova II", shape = 15), size = 4) +
                               ggplot2::geom_line(mapping = ggplot2::aes(y=ifmeans, color = rep(data$names, results$call$x.levels))) +
                               ggplot2::geom_point(mapping = ggplot2::aes(y=ifmeans, color = rep(data$names, results$call$x.levels)), size = 2) +
                               ggplot2::scale_shape_identity() +
                               ggplot2::geom_text(mapping = ggplot2::aes(y=anova2, color = "Anova II"), label = rep(round(results$anova2$marginalMeans, 2), each = prod(results$call$k.levels)), nudge_y = -(max(data$graph$means) - min(data$graph$means))/30, size = 4) +
                               ggplot2::scale_x_discrete(name = "", limits = 1:results$call$x.levels, labels = paste0("x = ", 1:results$call$x.levels)) +
                               ggplot2::labs(color = ggplot2::element_blank()),
                      "III" = ggplot2::ggplot(data = data$graph, mapping = ggplot2::aes(x=x)) +
                               ggplot2::geom_line(mapping = ggplot2::aes(y=means, color = rep(data$names, results$call$x.levels))) +
                               ggplot2::geom_point(mapping = ggplot2::aes(y=means, color = rep(data$names, results$call$x.levels)), size = 30*(data$graph$freq/sum(data$graph$freq)) + 5) +
                               ggplot2::geom_point(mapping = ggplot2::aes(y=anova3, color = "Anova III", shape = 15), size = 4) +
                               ggplot2::scale_shape_identity() +
                               ggplot2::geom_text(mapping = ggplot2::aes(y=anova3, color = "Anova III"), label = rep(round(results$anova3$marginalMeans, 2), each = prod(results$call$k.levels)), nudge_y = -(max(data$graph$means) - min(data$graph$means))/30, size = 4) +
                               ggplot2::scale_x_discrete(name = "", limits = 1:results$call$x.levels, labels = paste0("x = ", 1:results$call$x.levels)) +
                               ggplot2::labs(color = ggplot2::element_blank()),
                      "ATE" = ggplot2::ggplot(data = data$graph, mapping = ggplot2::aes(x=x)) +
                               ggplot2::geom_line(mapping = ggplot2::aes(y=means, color = rep(data$names, results$call$x.levels))) +
                               ggplot2::geom_point(mapping = ggplot2::aes(y=means, color = rep(data$names, results$call$x.levels)), alpha = 0.5, size = 30*(rep(colSums(results$call$freq), times = results$call$x.levels)/sum(data$graph$freq)) + 5) +
                               ggplot2::geom_point(mapping = ggplot2::aes(y=means, color = rep(data$names, results$call$x.levels)), size = 30*(data$graph$freq/sum(data$graph$freq)) + 5) +
                               ggplot2::geom_point(mapping = ggplot2::aes(y=mm, color = "ATE", shape = 15), size = 4) +
                               ggplot2::scale_shape_identity() +
                               ggplot2::geom_text(mapping = ggplot2::aes(y=mm, color = "ATE"), label = rep(round(results$ATE$marginalMeans, 2), each = prod(results$call$k.levels)), nudge_y = -(max(data$graph$means) - min(data$graph$means))/30, size = 4) +
                               ggplot2::scale_x_discrete(name = "", limits = 1:results$call$x.levels, labels = paste0("x = ", 1:results$call$x.levels)) +
                               ggplot2::labs(color = ggplot2::element_blank())
               ),
         "K" = switch(type,
                      "I"   = ggplot2::ggplot(data = data$graph, mapping = ggplot2::aes(y = means, x=K, colour = x)) +
                               {plots$lines} +
                               {plots$points} +
                               {plots$effects} +
                               {plots$mm} +
                               ggplot2::scale_x_discrete(name = "", labels = data$graph$K) +
                               ggplot2::scale_colour_discrete(name = "", labels = paste0("X = ", 1:results$call$x.levels)),
                      "II"  = ggplot2::ggplot(data = data$graph, mapping = ggplot2::aes(y = means, x=K, colour = x)) +
                               {plots$lines} +
                               {plots$points} +
                               {plots$effects} +
                               {plots$mm} +
                               ggplot2::geom_line(mapping = ggplot2::aes(y=ifmeans, group = x)) +
                               ggplot2::geom_point(mapping = ggplot2::aes(y=ifmeans), size = 2) +
                               ggplot2::scale_x_discrete(name = "", labels = data$graph$K) +
                               ggplot2::scale_colour_discrete(name = "", labels = paste0("X = ", 1:results$call$x.levels)),
                      "III" = ggplot2::ggplot(data = data$graph, mapping = ggplot2::aes(y = means, x=K, colour = x)) +
                               {plots$lines} +
                               {plots$points} +
                               {plots$effects} +
                               {plots$mm} +
                               ggplot2::scale_x_discrete(name = "", labels = data$graph$K) +
                               ggplot2::scale_colour_discrete(name = "", labels = paste0("X = ", 1:results$call$x.levels)),
                      "ATE" = ggplot2::ggplot(data = data$graph, mapping = ggplot2::aes(y = means, x=K, colour = x)) +
                               {plots$lines} +
                               {plots$points} +
                               {plots$effects} +
                               {plots$mm} +
                               {plots$shadowPoints} +
                               ggplot2::scale_x_discrete(name = "", labels = data$graph$K) +
                               ggplot2::scale_colour_discrete(name = "", labels = paste0("X = ", 1:results$call$x.levels)),
               )
    
  )
}
# 
# means <- matrix(c(120, 110, 60, 70, 150, 80, 100, 100, 100, 100, 100, 100, 80, 90, 140, 130, 50, 120), nrow = 3, ncol = 6, byrow = T)
# freq <- matrix(c(20, 17, 3, 5, 15, 20, 7, 26, 7, 5, 28, 7, 3, 17, 20, 15, 10, 15), nrow = 3, ncol = 6, byrow = T)
# k.levels <- c(2,3)
# #
# # means <- matrix(c(1,2,3,4,5,6,7,8), nrow = 2, ncol = 4, byrow = T)
# # freq <- matrix(c(12,8,13,7,6,14,18,2), nrow = 2, ncol = 4, byrow = T)
# # k.levels <- c(2,2)
# #
# # means <- matrix(c(4, 5, 3, 7), nrow = 2, ncol = 2, byrow = T)
# # freq <- matrix(c(10, 40, 20, 30), nrow = 2, ncol = 2, byrow = T)
# # k.levels <- 2
# 
# results <- unbANOVA::unbalancedANOVA(means, freq, k.levels)
# 
# plotunbANOVA(results, type = 1)
# plotunbANOVA(results, type = 1, showWeights = FALSE, showMM = TRUE, showEffects = TRUE)
# 
# plotunbANOVA(results, type = 2)
# plotunbANOVA(results, type = 2, showWeights = FALSE, showMM = TRUE, showEffects = TRUE)
# 
# plotunbANOVA(results, type = 3)
# plotunbANOVA(results, type = 3, showWeights = FALSE, showMM = TRUE, showEffects = TRUE)
# 
# plotunbANOVA(results, type = "ATE")
# plotunbANOVA(results, type = "ATE", showWeights = FALSE, showMM = TRUE, showEffects = TRUE)
