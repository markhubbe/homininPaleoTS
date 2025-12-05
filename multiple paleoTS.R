multiple_paleoTS<-function(data,poolvar=FALSE,models=5){
  
  #Function to run multiple instances of paleoTS from list created by dists2paleoTS
  #models is the number of models to contrast. The order is:
  #GRW,URW,Stasis,StrictStasis,PuncEq(1 step),OU,PuncEq(2steps)
  #the number of models means how far down this list the comparison goes
  
  require(paleoTS)
  require(ggplot2)
  
  nvars<-length(data)
  
  varnames<-names(data)
  
  results<-list()
  
  plotdata<-data.frame(matrix(NA,nvars*models,3))
  colnames(plotdata)<-c("variable","model","Akaike.wt")
  
  
  for(a in 1:nvars){
    
    if(any(is.nan(data[[a]]$mm))==FALSE){
      grw<-fitSimple(data[[a]],"GRW",pool=poolvar)
      urw<-fitSimple(data[[a]],"URW",pool=poolvar)
      sta<-fitSimple(data[[a]],"Stasis",pool=poolvar)
      if(models>=4){
        strsta<-fitSimple(data[[a]],"StrictStasis",pool=poolvar)
      }
      if(models>=5){
        pun2<-fitGpunc(data[[a]],minb=2,pool=poolvar)
      }
      
      if(models>=6){
        ou<-fitSimple(data[[a]],"OU",pool=poolvar)
      }
      
      if(models>=7){
        pun3<-fitGpunc(PTS_list[[a]],ng=3,minb=2,pool=poolvar)
      }
      
      if(models<4){
        results[[a]]<-compareModels(grw,urw,sta)
      }
      else if(models==4){
        results[[a]]<-compareModels(grw,urw,sta,strsta)  
      }
      else if(models==5){
        results[[a]]<-compareModels(grw,urw,sta,strsta,pun2)  
      }
      else if(models==6){
        results[[a]]<-compareModels(grw,urw,sta,strsta,pun2,ou)  
      }
      else if(models==7){
        results[[a]]<-compareModels(grw,urw,sta,strsta,pun2,pun3,ou)  
      }
      
      names(results)[a]<-varnames[a]
      
      best_model<-order(results[[a]][,4])[1]
      
      if(best_model==1){
        #plot(data[[a]],modelFit=grw)
      } else if(best_model==2){
        plot(data[[a]],modelFit=urw)
      } else if(best_model==3){
        plot(data[[a]],modelFit=sta)
      }else if(best_model==4){
        plot(data[[a]])
      }else if(best_model==5){
        plot(data[[a]],modelFit=pun2)
      }else if(best_model==6&models==6){
        plot(data[[a]],modelFit=ou)
      }
      else if(best_model==6&models==7){
        plot(data[[a]],modelFit=pun3)
      }
      else if(best_model==7){
        plot(data[[a]],modelFit=ou)
      }
      
      title(main=varnames[a])
      
      
      plotdata[(a-1)*models+1:models,1]<-rep(varnames[a],models)
      plotdata[(a-1)*models+1:models,2]<-rownames(results[[a]])
      plotdata[(a-1)*models+1:models,3]<-results[[a]][,5]
      
    }
    else{
      results[[a]]<-NA
      names(results)[a]<-varnames[a]
    }
  }
  
  #plotdata[,1]<-factor(plotdata[,1],levels=paste0("V",1:nvars))
  #plotdata[,2]<-factor(plotdata[,2])
  
  if(any(is.na(plotdata[,2])==TRUE)){
    plotdata<-plotdata[-which(is.na(plotdata[,2])==TRUE),]  
  }
  
 
  
  p1<-ggplot(plotdata,aes(x=variable, y=Akaike.wt,fill=model))+
    geom_bar(stat="identity")+
    coord_flip()+
    theme_light()
  
  print(p1)
  
  results[[nvars+1]]<-p1
  names(results)[nvars+1]<-"plot"
  
  return(results)
  
}