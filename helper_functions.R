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




######
RED = function(dat, grp){
  
  #####Now the function doesn't run at all#####
  dat = as.matrix(dat)
  grp = as.factor(grp)
  if (!is.numeric(dat)){stop("dat must be numeric")}
  if (!is.factor(grp)){stop("grp must be a factor")}
  #if (is.null(dimnames(dat)[[2]])){dimnames(dat)[[2]] = paste("var", 1:ncol(dat), sep = ".")} ###try to add in better variable naming in the future
  
  
  ##First step: Z-scores
  
  dat <- z.score(dat)
  
  #Calc largest group size
  maxN = max(table(grp))
  
  ##Array data as (individuals x groups x variables)
  ##Setting array extents to largest group size so array can be looped over. Values are ultimately averaged so NAs can be dropped
 
  m <- array(-100, c(maxN, length(levels(grp)), ncol(dat)))
  
  dimnames(m) = list(1:maxN, "grp" = levels(grp), "var"  = colnames(dat))
 
  for (g in 1:length(levels(grp))){
    for (i in 1:ncol(dat)){
      if (table(grp)[g]==maxN){
        pad <- NULL} else{
          pad <- rep(NA, maxN - table(grp)[g])
        }
      
      ##### vvvvv Something in here was the problem before vvvvv #####
      
      m[,g,i] <- append(dat[as.integer(grp)==g,i], pad)
      
      ##### ^^^^^something here ^^^^^ ######
    }
  }
  

  #####Calculate intergroup distance#####
  ##Comparing mean z.scores of groups
  rm(g,i)
  
  store<-array(NA,c(maxN,maxN))
  reset<-array(NA,c(maxN,maxN))
  hl<-array(NA,c(length(levels(grp)),length(levels(grp)),ncol(dat)))
  diss<-matrix(NA,nrow=length(levels(grp)),ncol=length(levels(grp)))
  
  for (i in 1:ncol(dat)){ #for each variable
    for (j in 1:length(levels(grp))){ #for each group 
      for (k in 1:length(levels(grp))){ #for each group?
        for (l in 1:maxN){ #for ALL individuals (including NAs)
          for (n in 1:maxN){ #again, for all?
            store[n,l]<- m[l,j,i]- m[n,k,i]
            if ((n==maxN) & (l==maxN)) {hl[j,k,i]<-mean(store,na.rm=TRUE)}
            if ((n==maxN) & (l==maxN)) {store<-reset}
          }
        }
      }
    }
  }
  
  rm(i,j)
  for (i in 1:length(levels(grp))){
    for (j in 1:length(levels(grp))){
      
      diss[j,i]<-abs(mean(hl[j,i,]))
    }
    }
  
  
  final<-abs(diss)
  colnames(final)<-levels(grp)
  
  out = list("RED.dist" = final, "n.tab" = table(grp))
  
  return(out)
  
}



red1 = RED(dat, grp)


####Default Plotting#####
cluster<-hclust(as.dist(final), method="ward.D2")

plot(cluster)
table(cutree(cluster,length(levels(grp))),levels(grp))




