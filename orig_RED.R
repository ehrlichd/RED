###Original RED Code####

#This is an example R-code for the forensic dataset used in the Robust Estimate of Grade Differences (RED)  A more generic function, that can be used on any R dataframe is being written at this time.

attach(forensic2)
teeth<-(dat) ###DEE changed initial data source
group<-10
var<-11
teeth2<-teeth
#standardization loop#
for (i in 2:ncol(teeth2)) {
  for (j in 1:nrow(teeth2)){
    teeth2[j,i]<- (teeth[j,i]-mean(teeth[,i],na.rm=TRUE))/(var(teeth[,i],na.rm=TRUE))**.5
  }}


new<-(split(teeth2, factor(teeth2[,1])))
g1<-as.data.frame(new$'TNAA') ###DEE removed spaces in data frame name
g2<-as.data.frame(new$'CAAA')
g3<-as.data.frame(new$'NYAA')
g4<-as.data.frame(new$'OHEA')
g5<-as.data.frame(new$'TNEA')
g6<-as.data.frame(new$'NYEA')
g7<-as.data.frame(new$'NMHA')
g8<-as.data.frame(new$'SFHA')
g9<-as.data.frame(new$'CAHA')
g10<-as.data.frame(new$'NYHA')

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

cluster<-cluster::agnes(final,diss="TRUE", method="ward") ##DEE add cluster::
plot(cluster)
cluster
table(cutree(cluster,group),teeth2[,1])


