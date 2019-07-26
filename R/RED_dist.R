#' Calculate RED distance matrix
#'
#' Calculate RED (Robust Estimator of grade Difference) on a set of ordinal or continuous data. Data are first standardized by mean and sd before pairwise differences are calculated within and between groups.
#'
#' @param dat A data matrix containing observations (rows) of variables(columns)
#' @param grp A grouping vector to compare the observations
#' @param dis.only Logical value indicating whether only the distance matrix is or a list containing the distance matrix, table of group sample sizes, and standardized z-scores.
#'
#' @return Depending on dis.only, RED() returns either a distance matrix only (dis.only = TRUE) or a list containing group sample size and standardized z-scores in addition to the distance matrix
#'
#'
#'
#' @export
#'
#' @examples
#'
#'
#' #with Forensic dataset
#'
#' data(forensic) #load sample data
#'
#' str(forensic) #check the structure of the data
#' grades <- forensic[,2:12] #asign the grades to an object
#' groups <- forensic[,1] #assign the grouping factor to another
#'
#' red1 <- RED_dist(grades, groups, dis.only = TRUE)
#'
#' RED_plot(red1, type = "tree")
#'
#'
#'
#' #Create sample data
#' dat1 <- matrix(rnorm(100, mean = 7.5, sd = 1), nrow = 100, ncol = 10)
#' dat2 <- matrix(rnorm(100, mean = 2.5, sd = 2), nrow = 100, ncol = 10)
#' dat3 <- matrix(rnorm(100, mean = 4, sd = 1), nrow = 100, ncol = 10)
#'
#' dat <- rbind(dat1, dat2, dat3)
#' grp <- rep(c("A","B","C"), each = 100)
#'
#'#Calculate distance matrix
#' dis <- RED_dist(dat, grp)
#'
#'#Visualize
#' RED_plot(dis, type = "2D")

RED_dist <- function(dat, grp, dis.only=TRUE){

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

  if (dis.only ==TRUE){
    out <- final
    } else {out <- list("RED.dist" = final, "n.tab" = table(grp), "z.grades" = dat)}

  return(out)

}