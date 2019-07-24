###RED Function WIP####

##Code written by John Daniels: Willermet, Daniels, Edgar 2016 AAPA
##Modified by DEE 2019

#This is an example R-code for the forensic data
#'set used in the Robust Estimate of Grade Differences (RED)  A more generic function,
#'that can be used on any R dataframe is being written at this time.#


##Read in test data set from forensic

dat = (read.csv("forensic.csv"))

##reclass 99 as NA
dat[dat==99] = NA 

##Realize data.frame is appropriate structure
grp = dat[,1]
dat = as.matrix(dat[,2:12])

##check strcture
str(dat)


z.score <- function(dat){
  dat <- as.matrix(dat)
  t.dat <- dat
  
  for (i in 1:ncol(dat)){
    for (j in 1:nrow(dat)){
      t.dat[j,i] <- (dat[j,i] - mean(dat[,i], na.rm = T))/(sd(dat[,i], na.rm = T))
      
    }
  }
  return(t.dat)
}


pair.diff <- function(v1, v2){
  mat = matrix(NA, nrow = length(v1), ncol = length(v2))
  for (i in 1:length(v1)){
    for (j in 1:length(v2)){
      mat[i,j] <- v1[i]-v2[j]
    }
  }
  return(mat)
}



######
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

red = RED(dat, grp)


plot(hclust(red2$RED.dist, method = "ward.D2"))
##matches results of original code


####Default Plotting#####

###2D###

plot.tree <- function(dist, grp = NULL, horiz = F, ...){
  
}

plot.mds <-function(dist, grp = NULL, dim = c(2,3),...){
  
  col = rainbow(length(levels(as.factor(grp))), s=.4)
  
  if (dim == 3){
    d = cmdscale(distm, k=3)
    plot3d(d, type = "n")
    for (i in length(dist))
    points3d(d)
  }
  
}

clus<-hclust(red2$RED.dist, method="ward.D2")

mds.2 = cmdscale(red2$RED.dist, k=2) 
mds.3 = cmdscale(red2$RED.dist, k=3)
plot(mds.2, pch = 16)
text(mds.2, labels = levels(grp))

plot(princomp(z.score(dat))$scores)

r = range(mds.3)
plot3d(mds.3)
text3d(mds.3, texts = levels(grp))
dimnames(mds.2)
colors = c(NY = "red", CA = "blue", TN = "green", OH = "cyan", SF = "magenta")
ord = order.dendrogram(dend)
grp[[2]]

colors = c("red", "blue","green")

colors = rainbow(10)

t = as.matrix(cbind(levels(grp), colors))
labelCol <- function(x) {
  
  if (is.leaf(x)) {
    
    ## fetch label - works for coloring by an aspect of the labels
    label <- attr(x, "label")
    #code <- substr(label, 1, 2)
    
    ##fetch order?
    #ord <- attr(x, "order")
    
    #Use following line to reset labels to abbriavation (grouping)
    # attr(x, "label") <- code
    attr(x, "nodePar") <- list(lab.col = t[label,])
    
    }
  return(x)
}

d = dendrapply(dend, labelCol)
dend = as.dendrogram(clus)
plot(d)
dend[[2]]
attr(dend, "label")

attributes(dend[[2]][[2]][[2]])

labels(dend)
attributes(dend)
attr(dend, "nodePar") <- list(lab.col = rep(c("blue", "red"), 2))
plot(dend)
class(clus)
is.leaf()

plot(as.dendrogram(clus), 
     type = "rectangle",
     horiz = F, 
     nodePar = list (pch = c(1,16), col = "red", label = "blue"), 
     edgePar = list(col = c("cyan", "blue"))
     )

plot(cluster)
plot(clus2)
table(cutree(cluster,length(levels(grp))),levels(grp))




