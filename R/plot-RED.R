#'Plot dist objcect as dendrogram
#'
#'Plot the result of RED() (or any dist object) as a dendrogram. Increased functionality to come
#'
#'@param d A distance object
#'@param type The type of plot to make: 2D Scatterplot, 3D Scatterplot, Dendrogram
#'@param labels Logical value indicating whether data point should be labeled
#'
#'
#'@return Nothing will happen


plot_RED <- function(d, type = c("2D","3D", "tree"), labels = T){
  if (class(d) != "dist"){stop("d must be a dist object")}
  if(type == "3D"){
    require(rgl)

    d1 <- cmdscale(d, k =3)

    plot3d(d1, main = paste("MDS Cluster of ", deparse(substitute(d))), xlab ="Dim 1", ylab ="Dim 2", zlab ="Dim 3")
    if (labels == TRUE){
      text3d(d1, texts = labels(d), adj = 1)
    }
  } else if (type == "2D"){
    d1 <- cmdscale(d, k = 2)

    plot(d1, main = paste("MDS Cluster of ", deparse(substitute(d)), sep =""), xlab = "Dim 1", ylab = "Dim 2")

    if (labels == TRUE){
      text(d1, labels = labels(d), adj = 1)
    }
  } else {
    plot(hclust(d, method = "ward.D2"), xlab = "", sub ="", main = paste("Cluster Dendrogram of ", deparse(substitute(d)), sep =""))
  }
}
