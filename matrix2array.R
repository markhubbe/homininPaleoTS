matrix2array<-function(Data, dim =3){
  #transforms a matrix data into an array for geomorph analyses
  
  vars=(ncol(Data)-1)/dim
  
  inds=nrow(Data)
  
  DataArray<-array(0, c(vars,dim,inds))
  
  if(dim==2){
    colnames = c("X","Y")
  }else if (dim ==3){
    colnames = c("X","Y","Z")
  }
  
  dimnames(DataArray)[[2]]<-colnames
  
  #name the rows
  Varnames<-rep(0,vars)
  
  for(a in 1:vars){
    Varnames[a]<-substr(colnames(Data)[(a-1)*3+2],1,nchar(colnames(Data)[(a-1)*3+2])-2)
  }
  
  dimnames(DataArray)[[1]]<-Varnames
  
  #name the inds
  
  indnames<-rep(NA,inds)
  
  for (a in 1:inds){
    indnames[a]<-as.character(Data[a,1])
  }
  
  
  dimnames(DataArray)[[3]]<-indnames
  
  # get the data
  for (a in 1:inds){
    for (b in 1:vars){
      DataArray[b,,a]<-unlist(Data[a,((b-1)*3+2):(b*3+1)])
    }
  }
  
  return(DataArray)
  
}