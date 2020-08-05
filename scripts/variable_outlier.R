box_stats <- data.frame(variable = "noah",min_val = 0,max_val = 0, box_lower=0,box_upper=0)
for(j in 1:length(variable.cols)){
    var_data <- Data[,c(lakeid.col,variable.cols[j])] %>% drop_na() %>% 
        group_by(lagoslakeid) %>% 
        summarize_all(list(median))
    var_data2 <- Data[,c(lakeid.col,variable.cols[j])] %>% drop_na()
    if(nrow(var_data != 0)) {
        box_out <- adjbox(var_data[,2],range=20,ylab=variable.names[j])
        temp_stats <- data.frame(variable = variable.names[j],
                                 min_val = min(var_data2[,2],na.rm=T), 
                                 max_val = max(var_data2[,2],na.rm=T),
                                 box_lower=box_out$fence[1],
                                 box_upper=box_out$fence[2])
        box_stats <- rbind(box_stats,temp_stats)
    }
}
box_stats <- box_stats %>% filter(variable !="noah") %>% 
    mutate_if(is.numeric,funs(round,.args = list(digits=2))) %>% 
    left_join(read_csv("chem_thresholds.csv"))
write_csv(box_stats,"output/rawdata_summary.csv")

temp_flag <- data.frame(eventida=1,lagoslakeid = 1, variable="noah",value=0,flag ="initial_row")
for(j in 1:length(variable.cols)){
    var_data <- Data[,c(event.col, lakeid.col,variable.cols[j])] %>% drop_na() %>% 
        mutate(variable = variable.names[j]) %>% 
        left_join(box_stats) %>% 
        filter((!!sym(variable.names[j])) > upper_limit | (!!sym(variable.names[j])) < lower_limit )
    if(nrow(var_data) != 0) {
        temp.out <- data.frame(eventida = var_data[,1],
                           lagoslakeid = var_data[,2],
                           variable = variable.names[j],
                           value = var_data[,3],
                           flag = "variable_outlier")
        temp_flag<- rbind(temp_flag,temp.out)
    }
}    
temp_flag <- temp_flag %>% filter(variable != "noah")
