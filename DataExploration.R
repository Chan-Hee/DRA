data<-read.csv("DRA_Data.csv")
rownames(data)<-data[,1]
data<-data[,-1]
check_na<-apply(data,1,function(x){any(is.na(x))})
print("Is there NA DATA?")
print(any(check_na))
