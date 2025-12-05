lm2dist<-function(data,x,y){

  #Quick function to extract linear distances from landmark data
  
  #data is a dataframe with ID in first column and data as XYZ for each landamark in folowing columns
  #x and y are vectors with landmarks to connect.first x will connect to first y, and so forth  
  #Both must be the same length
  
  if(length(x)!=length(y)){
    stop("ERROR: x and y must be the same length")
  }
  
  x<-unlist(x)
  y<-unlist(y)
  data<-data.frame(data)
  
  nlandmarks<- (ncol(data)-1)/3
  ninds<-nrow(data)
  
  distances<-data.frame(matrix(NA,ninds,(length(x)+1)))
  
  distances[,1]<-data[,1]
  colnames(distances)[1]<-colnames(data)[1]
  
  varnames<-rep(NA,nlandmarks)
  for(a in 1:nlandmarks){
    varnames[a]<-substr(colnames(data)[(a-1)*3+2],1,nchar(colnames(data)[(a-1)*3+2])-2)
  }
  #print(varnames)
  
  for(a in 1:length(x)){
    #print(a)
    for(b in 1:nrow(data)){
      
      tmp_vec<-c(data[b,(x[a]-1)*3+2:4],data[b,(y[a]-1)*3+2:4])
      
      if(any(is.na(tmp_vec))==FALSE){
        distances[b,a+1]<-((data[b,(x[a]-1)*3+2]-data[b,(y[a]-1)*3+2])^2+
                             (data[b,(x[a]-1)*3+3]-data[b,(y[a]-1)*3+3])^2+
                             (data[b,(x[a]-1)*3+4]-data[b,(y[a]-1)*3+4])^2)^0.5
        
        
      }else{
        
        distances[b,a+1]<-NA
        
      }
      
      
    }
    colnames(distances)[a+1]<-paste0(varnames[x[a]],"_",varnames[y[a]])                    
  }
  
  return(distances)
  
}