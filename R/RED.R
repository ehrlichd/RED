#' Calculate RED distance matrix
#'
#' Calculate RED (Robust Estimator of grade Difference) on a set of ordinal or continuous data. Data are first standardized by mean and sd before pairwise differences are calculated within and between groups.
#'
#' @param dat A data matrix containing observations (rows) of variables(columns)
#' @param grp A grouping vector to compare the observations
#'
#' @return output Returns a list containing the distance matrix, table of group sample sizes, and standardized z-scores.
#'
#' @keywords
#'
#' @export

RED <- function(dat, grp){

  dat <- as.matrix(dat)
  grp <- as.factor(grp)
  if (!is.numeric(dat)){stop("dat must be numeric")}
  if (!is.factor(grp)){stop("grp must be a factor")}


  ##First step: standardize grades, Z-scores

  dat <- z.score(dat)

  gl = length(levels(grp)) #get number of groups
  var = ncol(dat) #get number of variables

  t.mat = array(dim = c(gl, gl, var))
  g.mat = array(dim = c(gl, gl))

  for (i in 1:var){
    for(j in 1:gl){
      for (k in 1:gl){

        t.mat[j,k,i] <- mean(
          pair.diff(
            dat[as.integer(grp)==j,i],
            dat[as.integer(grp)==k,i]), na.rm = T)

      }
    }
  }

  for (l in 1:gl){
    for (n in 1:gl){
      g.mat[n,l] <- abs(mean(t.mat[l,n,], na.rm = T))
    }
  }


  final<- abs(g.mat)
  colnames(final)<-levels(grp)
  final <- as.dist(final)
  out <- list("RED.dist" = final, "n.tab" = table(grp), "z.grades" = dat)

  return(out)

}
