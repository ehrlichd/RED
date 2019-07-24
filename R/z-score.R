#' Standardize data by calculating z-scores.
#'
#' Rescale a vector based on the mean, sd of the data.
#'
#' @param dat A numeric vector to be scaled
#'
#' @return output Returns a vector of z-scores
#'
#' @keywords
#'
#' @export

z.score <- function(dat){
  dat <- as.matrix(dat)
  t.dat <- dat

  for (i in 1:ncol(dat)){
    for (j in 1:nrow(dat)){
      t.dat[j,i] <- (dat[j,i] - mean(dat[,i], na.rm = T))/sd(dat[,i], na.rm = T)

    }
  }
  return(t.dat)
}
