#'Plot dist objcect as dendrogram
#'
#'Plot the result of RED() (or any dist object) in a variety of ways. For scatter plots, Multidimensional scaling (cmdscale()) is used to visualize the distance matrix, for dendrograms, Ward's clustering is used hclust(method = "ward.D2").
#'
#' @param d A distance object
#' @param type The type of plot to make: 2D Scatterplot, 3D Scatterplot, Dendrogram
#' @param labels Logical value indicating whether data point should be labeled
#' @param ... Additional arguments to pass to plot/plot3D
#' @export
#' @return Nothing will happen
#'
#' @examples
#' #with Forensic dataset
#'
#' data(forensic) #load sample data
#'
#' str(forensic) #check the structure of the data
#' grades <- forensic[,2:12] #asign the grades to an object
#' groups <- forensic[,1] #assign the grouping factor to another
#'
#' red1 <- RED(grades, groups, dis.only = TRUE)
#'
#' plot_RED(red1, type = "tree")
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
#' dis <- RED(dat, grp)
#'
#'#Visualize
#' plot_RED(dis, type = "2D")
#'
#'
#'


plot_RED <- function(d, type = c("2D","3D", "tree"), labels = T, ...){
  if (class(d) != "dist"){stop("d must be a dist object")}
  if (length(type) != 1){stop("'type' must be one of:c( '2D', '3D', 'tree')")}


  if(type == "3D"){


    d1 <- cmdscale(d, k =3)

    plot3d(d1, main = paste("MDS Cluster of ", deparse(substitute(d))), xlab ="Dim 1", ylab ="Dim 2", zlab ="Dim 3", ...)
    if (labels == TRUE){
      text3d(d1, texts = labels(d), adj = 1)
    }
  }

  else if (type == "2D"){
    d1 <- cmdscale(d, k = 2)

    plot(d1, main = paste("MDS Cluster of ", deparse(substitute(d)), sep =""), xlab = "Dim 1", ylab = "Dim 2", ...)

    if (labels == TRUE){
      text(d1, labels = labels(d), adj = 1)
    }
  }

  else if (type == "tree"){

    if (labels == TRUE){

      plot(as.dendrogram(hclust(d, method = "ward.D2")), main = paste("Cluster Dendrogram of ", deparse(substitute(d)), sep =""))

    } else {
      plot(as.dendrogram(hclust(d, method = "ward.D2")), leaflab = "none", main = paste("Cluster Dendrogram of ", deparse(substitute(d)), sep =""), ...)
    }
  }
  else {stop("'type' must be one of:c( '2D', '3D', 'tree')")}
}
