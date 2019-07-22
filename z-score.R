z.score <- function(dat){
  dat <- as.matrix(dat)
  
  for (i in 1:ncol(dat)){
    for (j in 1:nrow(dat)){
      dat[j,i] <- ((as.numeric(dat[j,i]) - mean(as.numeric(dat[,i]), na.rm = T))/sd(as.numeric(dat[,i]), na.rm = T))
      
    }
  }
  return(dat)
}