

# Data Merging week 11 homework #
#         What to do !!!!       #

## Merge All files into data frame
## Save data about whehter the paitent is cancer or not



#Environment Setting
setwd("/home/21500576")


# Merging files into one dataframe
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.table(file=x,header=T)})
  
  check<-lapply(datalist,dim)
  index<-sapply(check,function(x){x[1]==54675&&x[2]>3})
  
  datalist<-datalist[index]
  filelist<-filenames[index]
  
  cancer_or_not<-sapply(filelist,function(x){as.integer(substr(x,nchar(x)-4,nchar(x)-4))})
  num_of_patients<-sapply(datalist,function(x){dim(x)[2]-3})
  
  result<-mapply(function(x,y){rep(x,y)},cancer_or_not,num_of_patients)
  result<-unlist(result,use.names = FALSE)
  data<-Reduce(function(x,y){y<-y[c(-1,-2,-3)]; cbind(x,y)}, datalist)
  
  return(list(Data = data, Result = result))
  
}

datas<-multmerge("/home/data/GEO_GPL570")
datas<-multmerge("/Users/chanhee/Desktop/Data")

df<-datas$Data
result<-datas$Result
