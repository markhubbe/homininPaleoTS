PCA<-function(Data, Names=NULL, group=NULL, cov=TRUE, plot=FALSE, colors = "auto"){ 
  
  #This will calculate Principal Components and plot a graph if desired
  #Data: a dataframe of numeric variables to that will be transformed into PCs
  #Names: vector with the id names of each row in Data
  #group: vector with the grouping variable in each row in Data
  #cov: if TRUE, PCA is calculated on covariance matrix. If FALSE, correlation matrix is used instead
  #plot: if TRUE a generic plot is created of the first 2 PCs (not a ggplot though)
  #colors: "auto" means colors are auto-picked from rainbow(). Else, a vector of colors can be passed.
  
  Data<-data.frame(Data)
  
  if(is.data.frame(group)==TRUE){
    group<-unlist(group)
  }
  
  results<-list()
  
  #1. Remove the means from each variable
  
  means<-rep(0,ncol(Data))
  means<-apply(Data,2,mean)  
  
  Data<-t(t(Data)-means)
  
  # Then get eigenvalues and eigenvectors 
  if (cov==TRUE) {VCV<-var(Data)} 
  else {VCV<-cor(Data)}
  
  Dec<-eigen(VCV)
  
  evectors<-Dec$vectors
  
  results$eigenvalues<-matrix(NA,ncol(Data),4)
  results$eigenvalues[,1]<-Dec$values
  results$eigenvalues[,2]<-results$eigenvalues[,1]/sum(results$eigenvalues[,1])*100
  results$eigenvalues[,3]<-cumsum(results$eigenvalues[,1])
  results$eigenvalues[,4]<-cumsum(results$eigenvalues[,2])
  colnames(results$eigenvalues)<-c("eigenvalues","% of var","cum evalues", "cum % var")
  rownames(results$eigenvalues)<-paste("PC",1:ncol(Data))
  
  # and here the PC scores
  PCScores<-t(evectors)%*%t(Data)
  PCScores<-PCScores/(results$eigenvalues[,1]^0.5)
  PCScores<-t(PCScores)
  colnames(PCScores)<-paste("PC",1:ncol(Data))
  
  if(is.null(Names)==FALSE){
    if(is.data.frame(Names)==TRUE){
      Names<-unlist(Names)
    }
  }else{
    Names<-1:nrow(Data)
  }
  rownames(PCScores)<-Names
  
  results$PCS<-PCScores
  results$evectors<-evectors
  
  # and finally the correlations with the vars
  results$correlations<-matrix(NA,ncol(Data),ncol(Data))
  
  for (a in 1:ncol(Data)){
    for (b in 1:ncol(Data)){
      results$correlations[a,b]<-cor(Data[,a],PCScores[,b])
    }
  }
  colnames(results$correlations)<-paste("Factor",1:ncol(Data))
  rownames(results$correlations)<-colnames(Data)
  
  
  
  if (plot==TRUE){
    
    plot.new()
    xmin<-min(PCScores[,1])-0.05*(max(PCScores[,1])-min(PCScores[,1]))
    xmax<-max(PCScores[,1])+0.05*(max(PCScores[,1])-min(PCScores[,1]))
    ymin<-min(PCScores[,2])-0.05*(max(PCScores[,2])-min(PCScores[,2]))
    ymax<-max(PCScores[,2])+0.05*(max(PCScores[,2])-min(PCScores[,2]))
    plot.window(c(xmin,xmax),c(ymin,ymax))
    
    xticks<-round(seq(xmin,xmax,length=6),2)
    yticks<-round(seq(ymin,ymax,length=6),2)                                 
    axis(1,at=xticks)
    axis(2,at=yticks)
    
    box()
    
    Nvar<-ncol(Data)
    
    wghtCorx<-rep(0,Nvar)
    wghtCory<-rep(0,Nvar)
    
    for (a in 1:Nvar){
      if (results$correlations[a,1]>0) {wghtCorx[a]<-results$correlations[a,1]*xmax}
      else {wghtCorx[a]<-results$correlations[a,1]*-1*xmin}
      
      if (results$correlations[a,2]>0) {wghtCory[a]<-results$correlations[a,2]*ymax}
      else {wghtCory[a]<-results$correlations[a,2]*-1*ymin}
      
    }
    
    for (a in 1:Nvar){
      if(abs(results$correlations[a,1])>0.5){
        lines(c(0,(wghtCorx[a])),c(0,(wghtCory[a])),col="grey", lty=3)
        text(wghtCorx[a],wghtCory[a],rownames(results$correlations)[a],col="grey",cex=0.75)
      }
      else if (abs(results$correlations[a,2])>0.5){
        lines(c(0,(wghtCorx[a])),c(0,(wghtCory[a])),col="grey", lty=3)
        text(wghtCorx[a],wghtCory[a],labels=rownames(results$correlations)[a],col="grey",cex=0.75)
      }
      
    }
    
    
    if(colors[1] != "auto"){
      
      if(is.null(group)==TRUE){
        color=colors
      }else{
        group<-factor(group)
        ngroups<-nlevels(group)
        namegroups<-levels(group)
        
        palette<-colors
        
        color<-rep(0,nrow(Data))
        
        for (a in 1:nrow(Data)){
          color[a]<-palette[which(namegroups==group[a])]
          
          
        }
      }
    }else{
      
      if(is.null(group)==TRUE){
        color="blue"
      }
      else{
        group=factor(group)
        ngroups<-nlevels(group)
        namegroups<-levels(group)
        
        palette<-rainbow(ngroups)
        print(ngroups)
        color<-rep(0,nrow(Data))
        
        for (a in 1:nrow(Data)){
          color[a]<-palette[which(namegroups==group[a])]
          
        }
        
      }
    }
    
    points(PCScores[,1],PCScores[,2],pch=16,col=color)
    
    if(is.null(Names)==FALSE){
      text(PCScores[,1],PCScores[,2],Names,pos=3)
    }
    
    title(xlab=paste("PC1 - ",round(results$eigenvalues[1,2],1),"%"),ylab=paste("PC2 - ",round(results$eigenvalues[2,2],1),"%"))
    

    
  }
  
  return(results)
}
