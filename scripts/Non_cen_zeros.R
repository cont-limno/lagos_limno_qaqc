temp_flag = data.frame(eventida = 1,lagoslakeid = 1,variable="noah",value=1,flag="first_row")
for(i in 1:length(variable.cols)) {
  sensor.col = which(names(Data)==paste(names(Data)[variable.cols[i]],"_censorcode",sep=""))
  #if(i != 11) { #Can't remember why I excluded tdn in the past...i think the algorithm crapped out
    {
      var_data <- Data[,c(event.col, lakeid.col,variable.cols[i],sensor.col)] %>% 
        drop_na() %>%
        filter((!!sym(variable.names[i])) == 0)
      var_data = var_data
      if(nrow(var_data)>0){
      names(var_data)[4]="censor_code"
      var_data <- var_data %>% filter(censor_code=="NC4" | censor_code == "" | is.na(censor_code)==TRUE | censor_code == "NC3")
      if(nrow(var_data)>0) {
        temp.out <- data.frame(eventida = var_data[,1],
                               lagoslakeid = var_data[,2],
                               variable = variable.names[i],
                               value = var_data[,3],
                               flag = "non_cen_zero")
        temp_flag<- rbind(temp_flag,temp.out)
      }
    }
  }
}
temp_flag <- temp_flag %>% filter(variable != "noah") %>% filter(variable !="secchi")
