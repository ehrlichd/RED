#'Plot dist objcect as dendrogram
#'
#'Plot the result of \code{\link{RED_dist}} (or any dist object) in a variety of ways. For scatter plots, Multidimensional scaling (cmdscale()) is used to visualize the distance matrix, for dendrograms, Ward's clustering is used hclust(method = "ward.D2").
#'
#' @param d A distance object
#' @param type The type of plot to make: 2D Scatterplot (type = "2D"), 3D  Scatterplot (type = "3D"; note: requires \code{\link{rgl}} package),  or Dendrogram (type = "tree")
#' @param labels Logical value indicating whether data point should be labeled
#' @param ... Additional arguments to pass to plot/plot3D
#' @export
#'
#'
#' @seealso \code{\link{RED_dist}}
#' @seealso \code{\link{rgl}}
#'
RED_plot <- function(d, type = c("2D","3D", "tree"), labels = T, ...){
  if (class(d) != "dist"){stop("d must be a dist object")}
  if (length(type) != 1){stop("'type' must be one of:c( '2D', '3D', 'tree')")}


  if(type == "3D"){
    if (require(rgl)){

      d1 <- cmdscale(d, k =3)

      plot3d(d1, main = paste("MDS Cluster of ", deparse(substitute(d))), xlab ="Dim 1", ylab ="Dim 2", zlab ="Dim 3", ...)

      if (labels == TRUE){
        text3d(d1, texts = labels(d), adj = 1)
      }
    } else {stop("To use 3D plots, first use install.packages(\"rgl\") ")
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

    } else {##Testing leaf labels


      plot(as.dendrogram(hclust(d, method = "ward.D2")), leaflab = "none", main = paste("Cluster Dendrogram of ", deparse(substitute(d)), sep =""), ...)
    }
  }

  else {stop(" \"type\" must be one of: c( \"2D\", \"3D\", \"tree\") ")}
  }
