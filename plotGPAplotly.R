plotGPA<-function(Data,dim=3,frame = NULL,plot=TRUE){

  #this function will plot the procrustes landmarks into a plot with lines connecting points to centroids, to find outliers
  #Data needs to be a data.frame with name of specimens on first column, and landmarks labelled as x1,y1,z1, etc.
  #Frame is a matrix with 2 columns, with all pairs of points to connect in the data.frame.
    
  require(geomorph)
  require(plotly)
  
  Data<-data.frame(Data)
  
  #first, bring the data into an array to use gpa function from geomorph
  
  vars=(ncol(Data)-1)/3
  
  inds=nrow(Data)
  
  DataArray<-array(0, c(vars,dim,inds))
  
  Colnames<-rep(0,dim)
  
  #name the columns
  for(a in 1:dim){
    Colnames[a]<-substr(colnames(Data)[a+1],1,1)
  }
  
  dimnames(DataArray)[[2]]<-Colnames
  
  #name the rows
  Varnames<-rep(0,vars)
  
  for(a in 1:vars){
    Varnames[a]<-substr(colnames(Data)[(a-1)*3+2],2,nchar(colnames(Data)[(a-1)*3+2]))
  }
  
  dimnames(DataArray)[[1]]<-Varnames
  
  #name the inds
  
  dimnames(DataArray)[[3]]<-Data[,1]
  
  # get the data
  for (a in 1:inds){
    for (b in 1:vars){
      DataArray[b,,a]<-as.numeric(Data[a,((b-1)*3+2):(b*3+1)])
    }
  }
  
  
  #2. Do the GPA
  
  gpaData<-gpagen(DataArray)
  
  
  #start the plot
  if(plot==TRUE){ 
    
    #organize the data in a data.frame
    ninds<-dim(gpaData$coords)[3]
    nlandmarks<-dim(gpaData$coords)[1]
    plotdata<-data.frame(matrix(NA,(ninds*nlandmarks),4))
    centroiddata<-data.frame(matrix(0,nlandmarks,4))
    colnames(plotdata)<-c("Ind","X","Y","Z")
    colnames(centroiddata)<-c("Ind","X","Y","Z")
   
    for(a in 1:ninds){
      plotdata[1:nlandmarks+(a-1)*nlandmarks,1]<-rep(dimnames(gpaData$coords)[3][[1]][a],nlandmarks)
      
      plotdata[1:nlandmarks+(a-1)*nlandmarks,2:4]<-gpaData$coords[,,a]
      
      centroiddata[,2:4]<-centroiddata[,2:4]+gpaData$coords[,,a]
    }
    
    centroiddata[,2:4]<-centroiddata[,2:4]/ninds
    centroiddata[,1]<-rep("centroid",nlandmarks)

    plotdata<-rbind(plotdata, centroiddata)
    
    
          
    plotdata[,1]<-factor(plotdata[,1],levels=c(unique(Data[,1]),"centroid"))
    
    
    
    minval<-min(plotdata[,2:4])
    maxval<-max(plotdata[,2:4])
    
    fig <- plot_ly(plotdata, x = ~X, y = ~Y, z = ~Z, color = ~Ind, colors = c(rainbow(ninds),"gray50"),size=2)
 
    fig <- fig %>% add_markers()
    
    fig <- fig %>% layout(scene = list(xaxis = list(range = c(minval,maxval)),
                                       yaxis = list(range = c(minval,maxval)),
                                       zaxis = list(range = c(minval,maxval))))
    
  }  
  
  if(plot==TRUE){
    return(list(gpaData=gpaData,figure=fig))
  }
  else{
    return(gpaData)
  }
}