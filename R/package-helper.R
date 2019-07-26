#' @name RED-package
#' @docType package
#'
#' @title Robust Estimator of grade Difference (RED)
#' @author Daniel E. Ehrlich (package); Daniels, J., McKean, J., Willermet, C., Edgar, H. (method)
#' @description Package of functions for calculating and visualizing Robust Estimator of grade Difference (RED). RED is a nonparametric method that avoids data compression and allows missing data.
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
#' @usage data(forensic)
#'
#' @examples
#' data(forensic)
#'
#'
#' dimnames(forensic)
#'
#' grp <- forensic [,1]
#' dat <- forensic [,2:12]
#'
#' forensic
NULL


