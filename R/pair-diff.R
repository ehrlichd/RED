#' Calculate pairwise difference between two vectors
#'
#' Calculate pairwise differenec between any two vectors. Calculating on the same vector twice will produce a symmetrical matrix with 0s on the diagonal
#'
#' @param v1 A numeric vector
#' @param v2 A numeric vector
#'
#' @return Returns symmetrical pairwise difference between vectors
#'
#' @keywords
#'
#' @export
#'
#' @examples
#' a <- 1:10
#' b <- 5:15
#'
#' pair.diff(a,b)


pair.diff <- function(v1, v2){
  mat = matrix(NA, nrow = length(v1), ncol = length(v2))
  for (i in 1:length(v1)){
    for (j in 1:length(v2)){
      mat[i,j] <- v1[i]-v2[j]
    }
  }
  return(mat)
}
