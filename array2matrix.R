array2matrix<-function(Array){
  #rewrites and array from geomorph into a matrix to facilitate analyses
  
  Nind<-dim(Array)[3]
  Nvars<-dim(Array)[1]*dim(Array)[2]
  
  orgMat<-data.frame(matrix(0,Nind,Nvars+1))
  orgMat[,1]<-dimnames(Array)[[3]]
  
  for (a in 1:dim(Array)[3]){
    col=1
    for(b in 1:dim(Array)[1]){
      for (c in 1:dim(Array)[2]){
        col=col+1
        orgMat[a,col]<-Array[b,c,a]
      }
    }
      
  }
  return(orgMat)
}