temp_flag <- data.frame(eventida=1,lagoslakeid = 1, variable="noah",value=0,flag ="initial_row")
for(i in 1:length(lakes)) {
    lake_data <- Data %>% filter(Data[,lakeid.col] == lakes[i])
    for(j in 1:length(variable.cols)){
        var_data <- lake_data[,c(event.col,lakeid.col,variable.cols[j])] %>% drop_na()
        if(nrow(var_data)>=10) {
            #use to boxplot stats. Adj boxplot tends to throw out low values in highly 
            #scewed data. Thus, we also estimate with standard boxplot. outliers are 
            #identified as those that are common between the two approaches
            out <- boxplot.stats(var_data[,3],coef = 2.5)
            out2 <- adjbox(var_data[,3],range=3.5,plot = FALSE)
            if(as.numeric(diff(out$conf))==0) next()
            bad <- out$out[out$out %in% out2$out]
            if(length(bad>0)){
                bad_data <- var_data %>% filter(var_data[,3] %in% bad)
                temp.out <- data.frame(eventida = bad_data[,1],
                                  lagoslakeid = bad_data[,2],
                                  variable = variable.names[j],
                                  value = bad_data[,3],
                                  flag = "lake_outlier")
                temp_flag <- rbind(temp_flag,temp.out)
            } #end loop if outliers are detected
        } #end loop of looking for outliers in datasets with >20 observations        
    }#end variable loop
}#end lakes loop

temp_flag <- temp_flag %>% filter(variable !="noah")


