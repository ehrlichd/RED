#' Calculate pairwise difference between two vectors
#'
#' Calculate pairwise differenec (0 on diagonals) between any two vectors.
#'
#' @param v1 A numeric vector
#' @param v2 A numeric vector
#'
#' @return Returns symmetrical pairwise difference between vectors
#'
#' @keywords
#'
#' @export


pair.diff <- function(v1, v2){
  mat = matrix(NA, nrow = length(v1), ncol = length(v2))
  for (i in 1:length(v1)){
    for (j in 1:length(v2)){
      mat[i,j] <- v1[i]-v2[j]
    }
  }
  return(mat)
}
