#' @name RED-package
#' @docType package
#'
#' @aliases RED
#'
#' @title Robust Estimator of grade Difference (RED)
#' @author Daniel E. Ehrlich (package); Daniels, J., McKean, J., Willermet, C., Edgar, H. (method)
#'
#'
#' @description Package of functions for calculating and visualizing Robust Estimator of grade Difference (RED). RED is a nonparametric method that avoids data compression and allows missing data. Please send any comments, or questions to robustestimator@gmail.com
#'
#'
#'
#' @import rgl
#' @import stats
#' @import utils
#' @import graphics
#'
#'
#' @references (2018) Daniels, J., McKean, J., Willermet, C., Edgar, H., Robust Estimator of Grade Differences: a new statistical solution to an old categorical data problem, Chapter 4, Cambridge University Press.
#'
#' @references (2016) Willermet, C., Daniels, J., Edgar, H., Seeing RED: A new statistical solution to an old categorical data problem. https://www.researchgate.net/publication/301777157_Seeing_RED_A_new_statistical_solution_to_an_old_categorical_data_problem

#' RED-package
NULL

#'Forensic sample of dental non-metric trait scores
#'
#' @name forensic
#' @docType data
#'
#' @keywords datasets
#'
#'
#' @usage dat <- forensic

"forensic"

#' Standardize data by calculating z-scores.
#'
#' Rescale a vector based on the mean, sd of the data.
#'
#' @param dat A numeric vector to be scaled
#'
#' @return output Returns a vector of z-scores
#'
#'
#'
#' @export

RED_zscore <- function(dat){
  dat <- as.matrix(dat)
  t.dat <- dat

  for (i in 1:ncol(dat)){
    for (j in 1:nrow(dat)){
      t.dat[j,i] <- (dat[j,i] - mean(dat[,i], na.rm = T))/sd(dat[,i], na.rm = T)

    }
  }
  return(t.dat)
}

