library(naniar)
temp.flags = data.frame(lagoslakeid = NA,eventidb = NA,programid = NA,variable=NA,value=NA,flag=NA)
temp.flags = temp.flags[-1,]

for(i in 1:length(variable.cols)) {
  sensor.col = which(names(Data)==paste(names(Data)[variable.cols[i]],"_censorcode",sep=""))
  #if(i != 11) { #Can't remember why I excluded tdn in the past...i think the algorithm crapped out
    {
    temp.data = Data[,c(variable.cols[i],sensor.col,lakeid.col,event.col,programid.col)]
    temp.data = temp.data[which(is.na(temp.data[,1])==FALSE),]
    if(nrow(temp.data)>0){
      names(temp.data)[2]="censor_code"
      temp.data <- temp.data %>% replace_with_na(replace = list(censor_code = ""))
      temp.data = temp.data[which(is.na(temp.data$censor_code)==TRUE),] #| temp.data$censor_code==""
      if(nrow(temp.data)>0) {
        temp.data$flag = "no_censor_code"
        temp.data$variable = names(temp.data)[1]
        temp.data = temp.data[,c(3,4,5,7,1,6)]
        names(temp.data)[5] = "value"
        temp.flags = rbind(temp.flags,temp.data)
      }
    }
  }
}

temp.flags <- temp.flags %>% filter(variable != "secchi")
write_csv(temp.flags,"output/missing_censor_codes.csv")

# temp.flags = temp.flags[which(temp.flags$variable!="secchi"),]
# outlier.flags = temp.flags
# 
# #determine what variablers were flagged
# out.vars = unique(outlier.flags$variable)
# 
# #link the flagged observations back to the original dataset
# for(i in 1:length(out.vars)){
#   which.col = which(names(Data)==out.vars[i])
#   temp.data = outlier.flags[which(outlier.flags$variable==out.vars[i]),]
#   #replaced flagged values with NAs in the dataset
#   Data[match(temp.data$eventidb,Data$eventidb),which.col] = NA
# }
for(i in 1:length(variable.cols)) {
  print(which(names(Data)==paste(names(Data)[variable.cols[i]],"_censorcode",sep="")))
}
