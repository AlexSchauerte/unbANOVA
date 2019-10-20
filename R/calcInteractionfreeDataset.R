#' Calculate the means of an interaction-free dataset
#'
#' This function calculates an interaction-free dataset (x-effects, K-effects, but no x*K-effects).
#' @param means matrix of means, the columns denote the covariates and the rows the different x levels
#' @param freq matrix of frequencies, the columns denote the covariates and the rows the different x levels
#' @param k.levels numeric vector with the number of levels for all covariates (K1, K2, ...)
#' @keywords anova
#' @export
calcInteractionfreeDataset <- function(means, freq, k.levels){
  # checks
  if(!(is.matrix(means))) stop("'means' is not a matrix.")
  if(!(is.matrix(freq))) stop("'freq' is not a matrix.")
  if(!(is.numeric(means))) stop("'means' is not numeric.")
  if(!(is.numeric(freq))) stop("'freq' is not numeric.")
  if(nrow(means) != nrow(freq) | ncol(means) != ncol(freq)) stop(paste0("'means' (", nrow(means), "x", ncol(means), ") and 'freq' (", nrow(freq), "x", ncol(freq),") do not hATE the same dimensions."))
  if(ncol(means) == 1) return(means)
  if(is.null(k.levels)) {
    warning("'k.levels' not given, but it is recommended, because the results depend on this. Will assume that only one K (values: ", paste0(1:ncol(means), collapse = " "),") is present.")
    k.levels <- ncol(means)
  }

  # cat(paste0("k.levels = ", paste0(k.levels, collapse = ""), " [calcInteractionfreeDataset]\n"))
  # cat(paste0("length(k.levels) = ", length(k.levels), " [calcInteractionfreeDataset]\n"))

  #bereite Daten vor
  data <- list(work = cbind(setNames(rev(expand.grid(sapply(c(rev(k.levels), nrow(means)), seq, simplify = F))), c("x", paste0("K", 1:length(k.levels)))), means = as.numeric(t(means)), freq = as.numeric(t(freq))))
  corr_cov <- paste0("K", 1:length(k.levels))

  data$extend <- data$work[rep(row.names(data$work), data$work$freq), c("x",corr_cov,"means")]
  data$extend$x <- factor(data$extend$x)
  data$extend[corr_cov] <- lapply(data$extend[corr_cov], factor)

  X <- model.matrix(formula(paste0("means ~ x", if(length(k.levels) > 0) paste0("+", mapply(unlist(sapply(1:length(k.levels), function(i){combn(length(k.levels), i, simplify = F)}), recursive = F), FUN = function(x){paste0("K", x, collapse=":")}), collapse = ""))), data$extend)

  #bilde Referenzgruppenmodellparameter für ein Modell ohne Interaktion
  v <- c(0, solve(t(X) %*% X) %*% t(X) %*% data$extend$means)
  lev <- k.levels - 1

  m <- list(matrix(F, nrow(means), ncol(means)))

  #intercept everywhere
  m <- rlist::list.append(m, matrix(T, nrow(means), ncol(means)))

  #add all x main effects
  m <- rlist::list.append(m, lapply(2:nrow(means), function(x){m[[1]][x,] <- T
                                                        return(m[[1]])}))
  #add all K main effects
  m <- rlist::list.flatten(rlist::list.append(m, lapply(1:length(k.levels), function(i){
                           lapply((2:k.levels[i]) - 1, function(j){
                                temp <- (1:prod(k.levels[1:length(k.levels) > i])) + j * prod(k.levels[1:length(k.levels) > i])
                                bool <- rep(F, temp[1]-1)
                                bool[temp] <- T
                                if((length(bool)%%k.levels[length(k.levels)]) != 0){
                                    bool <- c(bool, rep(F, k.levels[length(k.levels)]-length(bool)))
                                }
                                m[[1]][,rep(bool, length.out = prod(k.levels))] <- T
                                return(m[[1]])
                           })
                      })))

  #add all K interactions (without any X-K-interaction)
  if(length(k.levels) > 1){
    m <- rlist::list.flatten(rlist::list.append(m, unlist(lapply(
      lapply(
        unlist(lapply(2:length(lev), function(b){
          mapply(FUN = function(y){
            expand.grid(sapply(y, function(x){
              1 + nrow(means) + sum(lev[1:length(lev) < x]) + 1:lev[x]
            }, simplify = F))
          }, combn(1:length(lev), b, simplify = F), SIMPLIFY = F)
        }), recursive = F), function(j){
          data.frame(t(j))
        }
      ),
      function(k){
        lapply(k, function(i){Reduce("&", m[i])})
      }), recursive = F)), use.names = F)
  }

  #put all those parameters in there and just add them together
  return(Reduce("+", lapply(1:length(m), function(i){m[[i]] * v[i]})))
}
