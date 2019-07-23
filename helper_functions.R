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
  
  for (i in 1:ncol(dat)){
    for (j in 1:nrow(dat)){
      dat[j,i] <- ((as.numeric(dat[j,i]) - mean(as.numeric(dat[,i]), na.rm = T))/sd(as.numeric(dat[,i]), na.rm = T))
      
    }
  }
  return(dat)
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
  
  #####Now the function doesn't run at all#####
  dat <- as.matrix(dat)
  grp <- as.factor(grp)
  if (!is.numeric(dat)){stop("dat must be numeric")}
  if (!is.factor(grp)){stop("grp must be a factor")}
  #if (is.null(dimnames(dat)[[2]])){dimnames(dat)[[2]] = paste("var", 1:ncol(dat), sep = ".")} ###try to add in better variable naming in the future
  
  
  ##First step: Z-scores
  
  dat <- z.score(dat)
  
  #Calc largest group size
  maxN <- max(table(grp))
  
  ##Array data as (individuals x groups x variables)
  ##Setting array extents to largest group size so array can be looped over. Values are ultimately averaged so NAs can be dropped
  
  #####reshaping the data to this particular array might not be neccessary#####
 
  m <- array(-100, c(maxN, length(levels(grp)), ncol(dat)))
  
  dimnames(m) <- list(1:maxN, "grp" = levels(grp), "var"  = colnames(dat))
 
  for (g in 1:length(levels(grp))){
    for (i in 1:ncol(dat)){
      if (table(grp)[g] == maxN){
        pad <- NULL} else{
          pad <- rep(NA, maxN - table(grp)[g])
        }
      
      
      
      m[1:maxN,g,i] <- append(dat[as.integer(grp)==g,i], pad)
      
     
    }
  }
  

  #####Calculate intergroup distance#####
  
  ##### vvv Section to improve vvv #####
  ###loops iterate over data to ultimately summarize group differences.
  ##For each trait, for each combination of groups (g1, g1; g1, g2', g1, g3; etc), compare all the individuals

  
  store<-array(NA,c(maxN,maxN)) ##individual(n) x individual(l) comparison (can be within or between groups(j,k))
  reset<-array(NA,c(maxN,maxN)) 
  hl<-array(NA,c(length(levels(grp)),length(levels(grp)),ncol(dat))) ##group (j) x group (k) x trait (i) 
  diss<-matrix(NA,nrow=length(levels(grp)),ncol=length(levels(grp))) ##group x group 
  
  for (i in 1:ncol(dat)){ #for each variable
    for (j in 1:length(levels(grp))){ #for each group (rows)
      for (k in 1:length(levels(grp))){ #for each group (column)
        for (l in 1:maxN){ #for ALL individuals (column)
          
          
          for (n in 1:maxN){ #for ALL individuals (rows)
            store[n,l]<- m[l,j,i]- m[n,k,i] #each cell of store represents individual-individual difference. 0s on diagonal. NAs ok.
            if ((n==maxN) & (l==maxN)) {hl[j,k,i]<-mean(store,na.rm=TRUE)} #when the store matrix is full (rows(n) == maxN & col(l) == maxN) calculate the average of the whole matrix and save it to 1 cell in hl
            if ((n==maxN) & (l==maxN)) {store<-reset} #reset, for next loop (increment groups)
          }
        }
      }
    }
  }
  

  for (i in 1:length(levels(grp))){
    for (j in 1:length(levels(grp))){
      
      diss[j,i]<-abs(mean(hl[j,i,])) ##averaging across traits
    }
    }
  
  
  final<- abs(diss)
  colnames(final)<-levels(grp)
  View(final)
  final = as.dist(final)
  out = list("RED.dist" = final, "n.tab" = table(grp))
  
  return(out)
  
}

red1 = RED(dat, grp)


##### vvv DEE working on alternative calculation steps vvv #####

RED.2 <- function(dat, grp){
  
  dat <- as.matrix(dat)
  grp <- as.factor(grp)
  if (!is.numeric(dat)){stop("dat must be numeric")}
  if (!is.factor(grp)){stop("grp must be a factor")}
  #if (is.null(dimnames(dat)[[2]])){dimnames(dat)[[2]] = paste("var", 1:ncol(dat), sep = ".")} ###try to add in better variable naming in the future
  
  
  ##First step: standardize grades, Z-scores
  
  dat <- z.score(dat)
  
  gl = length(levels(grp)) #get number of groups

  t.mat = array(dim = c(gl, gl, ncol(dat))) #this is analogous to hl
  g.mat = array(dim = c(gl, gl)) #this is analogous to diss
  
  for (i in 1:ncol(dat)){
    for(j in 1:gl){
      for (k in 1:gl){
        
        t.mat[j,k,i] <- mean(pair.diff(dat[as.integer(grp)==j,i], dat[as.integer(grp)==k,i]), na.rm = T)
        
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
  final = as.dist(final)
  out = list("RED.dist" = final, "n.tab" = table(grp))
  
  return(out)
  
}

red2 = RED.2(dat[,2:12], dat$Ancestry)
plot(hclust(red2$RED.dist, method = "ward.D2"))

plot(hclust(red2$RED.dist, method = "ward.D"))


t = pair.diff(t1,t2)
View(t)
t.mat[1:table(grp)[1],1] <- dat[as.integer(grp)==1,1][1] - dat[as.integer(grp)==1,1]

View(t.mat)

##### ^^^^^ alt workflow ^^^^ #####

####Default Plotting#####
clus<-hclust(red1$RED.dist, method="ward.D2")

class(clus)
plot(as.dendrogram(clus), 
     type = "rectangle",
     horiz = F, 
     nodePar = list (pch = c(1,16), col = "red"), 
     edgePar = list(col = c("cyan", "blue"))
     )

plot(cluster)
plot(clus2)
table(cutree(cluster,length(levels(grp))),levels(grp))




