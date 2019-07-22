###RED Function WIP####

##Code written by John Daniels: Willermet, Daniels, Edgar 2016 AAPA
##Modified by DEE 2019

#This is an example R-code for the forensic data
#'set used in the Robust Estimate of Grade Differences (RED)  A more generic function,
#'that can be used on any R dataframe is being written at this time.#


##Read in test data set from forensic
dat = as.matrix(read.csv("forensic.csv"))

##reclass 99 as NA
dat[dat==99] = NA 

##Realize data.frame is appropriate structure
dat = as.data.frame(dat)

##check strcture
str(dat)

##Good!

####First step is "Standardization loop"
##This can paramatized

fun1 <- function(dat){
  dat <- as.matrix(dat)

  for (i in 1:ncol(dat)){
    for (j in 1:nrow(dat)){
      t <- ((as.numeric(dat[j,i]) - mean(as.numeric(dat[,i]), na.rm = T))/var(as.numeric(dat[,i]), na.rm = T))
      if (sign(t) < 0){
        dat[j,i] <- (abs(t)^.5)*-1
      } else {
        dat[j,i] <- t^.5
      }
      }
  }
  return(dat)
}

t.dat = fun1(dat[,2:12])
t1 = ((as.numeric(t[1,1]) - mean(as.numeric(t[,1]), na.rm = T))/var(as.numeric(t[,1]), na.rm = T))
sign(t1) == -1
t = -5
sign(t)==-1
range(as.numeric(dat[,2]), na.rm = T)
t = matrix(NA, dim(dat))
rm(i,j)

View(t.dat)
View(t)
t = dat[,2:12]
t = as.matrix(t)

for (i in 1:ncol(t)){
  for (j in 1:nrow(t)){
    t[j,i] <- ((as.numeric(t[j,i]) - mean(as.numeric(t[,i]), na.rm = T))/var(as.numeric(t[,i]),na.rm = T))**.5
  }
}

(as.numeric(t[1,1]) - mean(as.numeric(t[,1]), na.rm = T))/var(as.numeric(t[,1]), na.rm = T)**.5

(as.numeric(t[1,1]) - mean(as.numeric(t[,1]), na.rm = T))/var(as.numeric(t[,1]), na.rm = T)^.5

#####Old Code--Don't Need#####
#' attach(forensic2)
#' teeth<-(forensic2) -- creating a copy dataset
#' group<-10
#' var<-11
#' teeth2<-teeth
#' standardization loop#
#' for (i in 2:ncol(teeth2)) {
#' for (j in 1:nrow(teeth2)){
#'  teeth2[j,i]<- (teeth[j,i]-mean(teeth[,i],na.rm=TRUE))/(var(teeth[,i],na.rm=TRUE))**.5
#'  }}


new<-(split(teeth2, factor(teeth2[,1])))
g1<-as.data.frame(new$'TN AA')
g2<-as.data.frame(new$'CA AA')
g3<-as.data.frame(new$'NY AA')
g4<-as.data.frame(new$'OH EA')
g5<-as.data.frame(new$'TN EA')
g6<-as.data.frame(new$'NY EA')
g7<-as.data.frame(new$'NM HA')
g8<-as.data.frame(new$'SF HA')
g9<-as.data.frame(new$'CA HA')
g10<-as.data.frame(new$'NY HA')

max<-max(nrow(g1),nrow(g2),nrow(g3),nrow(g4),nrow(g5),nrow(g6),nrow(g7),
         nrow(g8),nrow(g9),nrow(g10))
m<-array(-100,c(max,group,var))

for (i in 2:ncol(teeth2)){
  
  pad<-rep(NA,(max-length(g1[,i])))
  g12<-c(g1[,i],pad)
  
  pad<-rep(NA,(max-length(g2[,i])))
  g22<-c(g2[,i],pad)
  
  pad<-rep(NA,(max-length(g3[,i])))
  g32<-c(g3[,i],pad)
  
  pad<-rep(NA,(max-length(g4[,i])))
  g42<-c(g4[,i],pad)
  
  pad<-rep(NA,(max-length(g5[,i])))
  g52<-c(g5[,i],pad)
  
  pad<-rep(NA,(max-length(g6[,i])))
  g62<-c(g6[,i],pad)
  
  pad<-rep(NA,(max-length(g7[,i])))
  g72<-c(g7[,i],pad)
  
  pad<-rep(NA,(max-length(g8[,i])))
  g82<-c(g8[,i],pad)
  
  pad<-rep(NA,(max-length(g9[,i])))
  g92<-c(g9[,i],pad)
  
  pad<-rep(NA,(max-length(g10[,i])))
  g102<-c(g10[,i],pad)
  
  
  
  m[,,i-1]<-cbind(g12,g22,g32,g42,g52,g62,g72,g82,g92,g102)
  
}

store<-array(NA,c(max,max))
reset<-array(NA,c(max,max))
hl<-array(NA,c(group,group,var))
diss<-matrix(NA,nrow=group,ncol=group)

for (i in 1:var){
  for (j in 1:group){
    for (k in 1:group){
      for (l in 1:max){
        for (n in 1:max){
          store[n,l]<- m[l,j,i]- m[n,k,i]
          if ((n==max) & (l==max)) {hl[j,k,i]<-mean(store,na.rm=TRUE)}
          if ((n==max) & (l==max)) {store<-reset}
        }}}}}

for (i in 1:group){
  for (j in 1:group){
    
    diss[j,i]<-abs(mean(hl[j,i,]))
  }}


final<-abs(diss)
colnames(final)<-c("TN AA","CA AA","NY AA","OH EA","TN EA",
                   "NY EA","NM HA","SF HA","CA HA","NY AA")
cluster<-agnes(final,diss="TRUE", method="ward")
plot(cluster)
cluster
table(cutree(cluster,group),teeth2[,1])


