#' Calculate the sum of squares of different ANOVA types
#'
#' This function calculates the sum of squares of x according to the different ANOVA types
#' @param means matrix of means, the columns denote the covariates and the rows the different x levels
#' @param freq matrix of frequencies, the columns denote the covariates and the rows the different x levels
#' @param k.levels numeric vector with the number of levels for all covariates (K1, K2, ...)
#' @param type numeric or character vector denoting the ANOVA types to be calculated (allowed values are any of the following: "I", 1, "II", 2, "III", 3, "ATE")
#' @param fixed numeric vector with the levels the corresponding covariates should be fixed to (NA for no fixing)
#' @keywords anova
#' @export
marginalSS <- function(means, freq, k.levels = NULL, type = c("I", "II", "III", "ATE"), fixed = NULL){
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

  sshyp <- function(A, means, freqs){
    return((t(A %*% means) %*% solve(A %*% solve(freqs) %*% t(A)) %*% (A %*% means))[1])
  }

  # cat(paste0("k.levels = ", paste0(k.levels, collapse = ""), " [marginalSS]\n"))
  # Anova II
  if(2 %in% type | "II" %in% type){
    data <- list(work = cbind(setNames(rev(expand.grid(sapply(c(rev(k.levels), nrow(means)), seq, simplify = F))), c("x", paste0("K", 1:length(k.levels)))), means = as.numeric(t(means)), freq = as.numeric(t(freq))))
    corr_cov <- paste0("K", 1:length(k.levels))

    data$extend <- data$work[rep(row.names(data$work), data$work$freq), c("x",corr_cov,"means")]
    data$extend$x <- factor(data$extend$x)
    data$extend[corr_cov] <- lapply(data$extend[corr_cov], factor)

    form_cov <- if(length(k.levels) == 1 & k.levels[1] == 1) {""} else {if(length(k.levels) > 0) paste0("+", mapply(unlist(sapply(1:length(k.levels), function(i){combn(length(k.levels), i, simplify = F)}), recursive = F), FUN = function(x){paste0("K", x, collapse=":")}), collapse = "")}
    X <- model.matrix(formula(paste0("means ~ x", form_cov)), data$extend)
  }


  return(purrr::compact(list(
    anova1 = if(1 %in% type | "I" %in% type) sshyp(A = matrixcalc::hadamard.prod(matrix(t(freq/rowSums(freq)), nrow = nrow(means) - 1, ncol = length(means), byrow = T),
                                                                     matrix(sapply(1:(nrow(means) - 1), function(i){
                                                                       return(c(rep(1, times = ncol(means)),
                                                                                rep(0, times = (i-1) * ncol(means)),
                                                                                rep(-1, times = ncol(means)),
                                                                                rep(0, times = (nrow(means) - (i+1)) * ncol(means))))
                                                                     }), nrow = nrow(means) - 1, ncol = length(means), byrow = T)),
                                                   means = as.numeric(t(means)),
                                                   freqs = diag(as.numeric(t(freq)))),
    anova2 = if(2 %in% type | "II" %in% type) sshyp(A = matrix(sapply(2:(nrow(means)), function(x){ # testet ob die Parameter der verschiedenen
                                                      erg <- rep(0, times = ncol(X)) # Indikatorvariablen für die x Level ungleich null sind
                                                      erg[x] <- 1
                                                      return(erg)
                                                    }), nrow = nrow(means) - 1, byrow = T),
                                                    means = solve(t(X) %*% X) %*% t(X) %*% data$extend$means, #schätzt interaktionsfreie Referenzgruppenparameter
                                                    freqs = t(X) %*% X),
    anova3 = if(3 %in% type | "III" %in% type) sshyp(A = matrix(sapply(1:(nrow(means) - 1), function(i){
                                                       return(c(rep(1 / length(means), times = ncol(means)),
                                                               rep(0, times = (i-1) * ncol(means)),
                                                               rep(- 1 / length(means), times = ncol(means)),
                                                               rep(0, times = (nrow(means) - (i+1)) * ncol(means))))
                                                     }), nrow = nrow(means) - 1, ncol = length(means), byrow = T),
                                                     means = as.numeric(t(means)),
                                                     freqs = diag(as.numeric(t(freq)))),
    ATE    = if("ATE" %in% type) sshyp(A = matrixcalc::hadamard.prod(matrix(colSums(freq)/sum(freq), nrow = nrow(means) - 1, ncol = length(means), byrow = T),
                                                         matrix(sapply(1:(nrow(means) - 1), function(i){
                                                           return(c(rep(1, times = ncol(means)),
                                                                    rep(0, times = (i-1) * ncol(means)),
                                                                    rep(-1, times = ncol(means)),
                                                                    rep(0, times = (nrow(means) - (i+1)) * ncol(means))))
                                                         }), nrow = nrow(means) - 1, ncol = length(means), byrow = T)),
                                       means = as.numeric(t(means)),
                                       freqs = diag(as.numeric(t(freq))))
  )))
}
