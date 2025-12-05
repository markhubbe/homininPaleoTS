dists2PaleoTS<-function(data,groups,chronology,avg_var=FALSE){
  
  #Quick function to process linear measurements into a list of PaleoTS values 
  #data = data.frame with ID in first column and measurements in columns 2:N
  #groups = vector with grouping variable (Taxob) of each individual
  #chronology = data frame with group name in first column and chronology of specimen in column 2
  #avg_var = whether to use average within group variances for all groups or use each groups var
  
  
  require(paleoTS)
  nvars<-ncol(data)-1
  varnames<-colnames(data)[2:ncol(data)]
  
  groups<-factor(groups)
  ngroups<-nlevels(groups)
  groupnames<-levels(groups)
  
  samples<-matrix(0,ngroups,nvars)
  row.names(samples)<-groupnames
  
  for(a in 1:ngroups){
    index<-which(groups==groupnames[a])
    
    for(b in 1:nvars){
      samples[a,b]<-length(index)-sum(is.na(data[index,b+1]))
    }
  }
  
  results<-list()
  
  
  
  for(a in 1:nvars){
    tmp_PTS<-data.frame(matrix(NA,ngroups,5))
    tmp_PTS[,1]<-groupnames
    
    #will calculate the weighted avg within group var in case we need it for N=1
    avg_within_var<-0
    total_inds<-0
    for(b in 1:ngroups){
      if(samples[b,a]>1){
        avg_within_var=avg_within_var+
          samples[b,a]*var(data[which(groups==groupnames[b]),a+1],na.rm=TRUE)
        total_inds<-total_inds+samples[b,a]
      } 
    }
    
    avg_within_var<-avg_within_var/total_inds
    
    #here we build the matrix that will create the PaleoTS file
    for(b in 1:ngroups){
      index<-which(groups==groupnames[b])
      ##mean
      tmp_PTS[b,2]<-mean(data[index,a+1],na.rm=TRUE)
      ##var
      if(avg_var==TRUE){
        tmp_PTS[b,3]<-avg_within_var
      }else if(samples[b,a]==1){
        tmp_PTS[b,3]<-avg_within_var
        warning(paste(groupnames[b],"has N=1. Weigthed average variance used for it."))
      }else{
        tmp_PTS[b,3]<-var(data[index,a+1],na.rm=TRUE)
      }
      
      ##Sample size
      tmp_PTS[b,4]<-samples[b,a]
      
      ##Chronology
      tmp_PTS[b,5]<-chronology[which(chronology[,1]==groupnames[b]),2]
    }
    
    results[[a]]<-as.paleoTS(tmp_PTS[order(tmp_PTS[,5],decreasing = TRUE),2],
                             tmp_PTS[order(tmp_PTS[,5],decreasing = TRUE),3],
                             tmp_PTS[order(tmp_PTS[,5],decreasing = TRUE),4],
                             tmp_PTS[order(tmp_PTS[,5],decreasing = TRUE),5],
                             oldest="first")
    
    
    names(results)[a]<-varnames[a]
    
  }
  
  return(results)
  
}