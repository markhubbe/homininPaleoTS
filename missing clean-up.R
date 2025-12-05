missing_reduction<-function(data,max_NA=50,by="variable",step_size=10){
  
  #this function will trim data iteratively to reduce the number of NAs in a dataset by removing variables and individuals
  #based on individuals and variables with most NAs until you end up with individuals and variables that have no more than the max NA% defined
  #data: a data_frame with variables in columns and individuals in rows. No labels accepted
  #max_NA: the maximum NA percentage accepted in the data. Default is 50, which means that at the end all variables and all individuals will have at least 50% of observations present
  #by: where should the iteration start, either "variable" or "individual". 
  #Starting with variables will maximize the number of individuals included in the final set (sligthly)  
  #Starting with individuals will maximize the number of variables included in the final data set (slightly)
  #step_size: the cut in percentage for each iteration. Default is 10, which means function will remove vars and inds with more than 90% of Missing, then 80%, then 70%, until MaxNA is reached. 
  
  #return the data cleaned and the indexes of the original individuals and original kept in the data
  
  
  #1. data prechecks
  data <- data.frame(data)
  
  if(any(lapply(data[,2:10],is.numeric)==FALSE)){
    stop("variables must be numeric")
  }
  
  if(max_NA>100 | max_NA<0){
    stop("max_NA should be a percentage, between 0 and 100.")
  }
  
  by = pmatch(by,c("variable,individual"))
  
  if(is.na(by)){
    stop("by should be either 'variable' or 'individual'")
  }
  
  
  #1. define steps for iteration.
  
  steps = seq((100-step_size),max_NA,by=step_size*-1)
  or_vars <- colnames(data)
  vars<-or_vars
  
  or_inds<-paste0("i",1:nrow(data))
  inds<-or_inds
  
  #2. start the iteration
  for(a in 1:length(steps)){
    
    #by = 1 is "variable"
    if(by==1){
      var_NA <- unlist(lapply(data,FUN=function(x) length(which(is.na(x)==TRUE))/length(x)))
      vars2remove<- which(var_NA*100>steps[a])
      
      if(length(vars2remove)>0){
        data<-data[,-vars2remove]
        
        #to remove the variables from the list
        vars_match<-vars[-vars2remove]
        
        vars<-vars[which(vars %in% vars_match)]
      }
    }
    
    #then we do individuals (will be first if by !=1)
    
    ind_NA<-apply(data,FUN=function(x) length(which(is.na(x)==TRUE))/length(x),1)
    inds2remove<-which(ind_NA*100>steps[a])
    
    if(length(inds2remove)>0){
      data<-data[-inds2remove,]
    
      inds_match<-inds[-inds2remove]
    
      inds<-inds[which(inds %in% inds_match)]
    }
    
    #then run variables second, if by==2
    #by = 2 is "individual"
    if(by==1){
      var_NA <- unlist(lapply(data,FUN=function(x) length(which(is.na(x)==TRUE))/length(x)))
      vars2remove<- which(var_NA*100>steps[a])
      
      if(length(vars2remove)>0){
        data<-data[,-vars2remove]
        
        #to remove the variables from the list
        vars_match<-vars[-vars2remove]
        
        vars<-vars[which(vars %in% vars_match)]
      }
    }
    
  }
  
  
  return(list(data = data,
              vars_kept = which(or_vars %in% vars),
              inds_kept = which(or_inds %in% inds)))
  
  
}