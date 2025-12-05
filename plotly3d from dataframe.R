
plot3d_df<-function(df,group=NULL,plot_labels=TRUE,frame = NULL, color="auto",plot.centroid=TRUE){
  
  #quick function to plot 3D data in plotly from a dataframe
  
  #df is a data frame with the name of individuals in the first column, X,Y,Z
  #of each variable following.
  #if plot_labels=TRUE: get the first part of the variable_name and plot them with landmarks
  #Frame is a matrix with 2 columns, with all pairs of points to connect in a dataframe.
  #group = variable with groups. If present, will colorcode the individuals by group
  #color = if 'auto', colors are picked from rainbow. Else, if can be a vector of color
  #of either the length of rows in df (if group=NA) or the number of groups, if grouping variable is present
  
  
  require(plotly)
  require(dplyr)
  
  df <- data.frame(df)
  
  if(is.null(group)==FALSE){
    group <- data.frame(group)
  }
  
  if(is.null(frame)==FALSE){
    frame <- data.frame(frame)
  }
  
  nlandmarks <- (ncol(df)-1)/3
  ninds <- nrow(df)
  
  if(is.null(group)==TRUE){
    plotdata <- data.frame(matrix(NA,(ninds*nlandmarks),4))
    colnames(plotdata) <- c("Ind","X","Y","Z")
    centroiddata <- data.frame(matrix(0,nlandmarks,4))
    
    colnames(centroiddata) <- c("Ind","X","Y","Z")
    
  }else{
    
    plotdata <- data.frame(matrix(NA,(ninds*nlandmarks),5))
    colnames(plotdata) <- c("Ind","X","Y","Z","Group")
    centroiddata <- data.frame(matrix(0,nlandmarks,5))
    
    colnames(centroiddata) <- c("Ind","X","Y","Z","Group")
  }
  
  samples_n <- rep(ninds,nlandmarks)
  
  for(a in 1:ninds){
    plotdata[(1:nlandmarks)+(a-1)*nlandmarks,1] <- rep(as.character(df[a,1]),nlandmarks)
    if(is.null(group)==FALSE){
      plotdata[(1:nlandmarks)+(a-1)*nlandmarks,5] <- rep(as.character(group[a,1]),nlandmarks)
    }
    
    for(b in 1:nlandmarks){
      plotdata[(a-1)*nlandmarks+b,2:4] <- df[a,((b-1)*3)+2:4]
      if(is.na(plotdata[(a-1)*nlandmarks+b,2])==FALSE){
        centroiddata[b,2:4] <- centroiddata[b,2:4]+df[a,((b-1)*3)+2:4]
      }
      else{
        samples_n[b] <- samples_n[b]-1
      }
    }
    
  }
  for(a in 1:nlandmarks){
    centroiddata[a,2:4] <- centroiddata[a,2:4]/samples_n[a]
  }
  
  centroiddata[,1] <- rep("centroid",nlandmarks)
  if(is.null(group)==FALSE){
    centroiddata[,5] <- rep("centroid",nlandmarks)
  }
  
  if(plot.centroid==TRUE){
    plotdata <- rbind(plotdata, centroiddata)
    plotdata[,1] <- factor(plotdata[,1],levels=c(unique(df[,1]),"centroid"))
  }else{
    plotdata[,1] <- factor(plotdata[,1],levels=unique(df[,1]))
  }
  
  minval <- min(plotdata[,2:4])
  maxval <- max(plotdata[,2:4])
  
  #this is all to create labels
  if(plot_labels==TRUE){
    varnames <- rep(NA,nlandmarks)
    for(a in 1:nlandmarks){
      varnames[a] <- substr(
        colnames(df)[(a-1)*3+2],
        1,
        nchar(colnames(df)[(a-1)*3+2])-2)
    }
  }
  
  df[,1]<-factor(df[,1])
  
  #plotdata[,1]<-factor(plotdata[,1],levels=c(levels(df[,1]),"centroid"))
  if(is.null(group)==FALSE){
    group[,1]<-factor(group[,1])
    ngroups<-nlevels(group[,1])
    if(plot.centroid==TRUE){
      plotdata[,5]<-factor(plotdata[,5],levels=c(levels(group[,1]),"centroid"))  
    }else{
      plotdata[,5]<-factor(plotdata[,5],levels=levels(group[,1]))
    }
    
  }                     
  
  if(color[1]=="auto"){
    if(is.null(group)==TRUE){
      if(plot.centroid==TRUE){
        colors = c(rainbow(ninds),"gray50")  
      }else{
        colors = rainbow(ninds)
      }
      
    }else{
      if(plot.centroid==TRUE){
        colors = c(rainbow(ngroups),"gray50")
      }else{
        colors = rainbow(ngroups)
      }
      
    }
  }else{
    if(plot.centroid==TRUE){
      colors = c(color,"gray50")
    }else{
      colors = color
    }
    
  }
  
  if(is.null(frame)==TRUE){
    if(is.null(group)==TRUE){
      fig <- plot_ly(data=plotdata, x = ~X, y = ~Y, z = ~Z, color = ~Ind, colors = colors,size=2)
    }else{
      fig <- plot_ly(data=plotdata, x = ~X, y = ~Y, z = ~Z, color = ~Group, colors = colors,size=2)  
    }
    
    fig <- fig %>% add_markers()
    
  }
  else{
    if(is.null(group)==TRUE){
      linedata<-data.frame(matrix(NA,(nrow(frame)*ninds)*2,5))
      colnames(linedata)<-c("lines","xl","yl","zl","Ind")
    }else{
      linedata<-data.frame(matrix(NA,(nrow(frame)*ninds)*2,6))
      colnames(linedata)<-c("lines","xl","yl","zl","Ind","Group")
    }
    
    for(a in 1:ninds){
      linedata[(1:(nrow(frame)*2))+(a-1)*2*nrow(frame),5]<-rep(as.character(df[a,1]),nrow(frame)*2)
      if(is.null(group)==FALSE){
        linedata[(1:(nrow(frame)*2))+(a-1)*2*nrow(frame),6]<-rep(as.character(group[a,1]),nrow(frame)*2)
      }
      for(b in 1:nrow(frame)){
        
        linedata[((1:2)+2*(b-1))+(a-1)*nrow(frame)*2,1]<-b
        linedata[(1+2*(b-1))+(a-1)*nrow(frame)*2,2:4]<-df[a,(frame[b,1]-1)*3+2:4]
        linedata[(2+2*(b-1))+(a-1)*nrow(frame)*2,2:4]<-df[a,(frame[b,2]-1)*3+2:4]
      }
    }
    
    if(is.null(group)==TRUE){
      centroidlines<-data.frame(matrix(NA,nrow(frame)*2,5))
      colnames(centroidlines)<-c("lines","xl","yl","zl","Ind")
      centroidlines[,5]<-rep("centroid",nrow(centroidlines))
    }else{
      centroidlines<-data.frame(matrix(NA,nrow(frame)*2,6))
      colnames(centroidlines)<-c("lines","xl","yl","zl","Ind","Group")
      centroidlines[,5]<-rep("centroid",nrow(centroidlines))
      centroidlines[,6]<-rep("centroid",nrow(centroidlines))
    }
    
    for(a in 1:nrow(frame)){
      centroidlines[(1:2)+2*(a-1),1]<-a
      centroidlines[(1:2)+2*(a-1),5]<-"centroid"
      centroidlines[1+2*(a-1),2:4]<-centroiddata[frame[a,1],2:4]
      centroidlines[2+2*(a-1),2:4]<-centroiddata[frame[a,2],2:4]
    }
    
    if(plot.centroid==TRUE){
      linedata<-rbind(linedata,centroidlines)
      
      linedata[,5]<-factor(linedata[,5],levels=c(levels(df[,1]),"centroid"))  
      if(is.null(group)==FALSE){
        linedata[,6]<-factor(linedata[,6],levels=c(levels(group[,1]),"centroid"))
      }
    }else{
      linedata[,5]<-factor(linedata[,5],levels=levels(df[,1]))
      
      if(is.null(group)==FALSE){
        linedata[,6]<-factor(linedata[,6],levels=levels(group[,1]))
      }
    }
    
    if(is.null(group)==TRUE){
      fig<-plot_ly(linedata, x=~xl,y=~yl,z=~zl,
                   type = 'scatter3d',mode="lines+markers",
                   split=~lines,
                   color=~Ind,
                   colors=colors,
                   line = list(width = 4),
                   marker = list(size=3.5))
    }else{
      fig<-plot_ly(linedata, x=~xl,y=~yl,z=~zl,
                   type = 'scatter3d',mode="lines+markers",
                   split=~lines,
                   color=~Group,
                   colors=colors,
                   line = list(width = 4),
                   marker = list(size=3.5))
    }
    
    
  }
  
  if(plot_labels==TRUE){
    fig <- fig %>% add_trace(data = centroiddata, x = ~X, y = ~Y, z=~Z, 
                            type = "scatter3d", mode="text", text = varnames, inherit=FALSE)
  }
  fig <- fig %>% layout(scene = list(aspectratio = list(x=1, y=1, z=1)))
  
  fig
}