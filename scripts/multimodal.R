library(diptest)
library(gridExtra)
temp_flag <- data.frame(programid="noah", variable="noah",pvalue=0)
for(i in 1:length(programs)) {
    program_data <- Data %>% filter(programname_lagos_us == programs[i]) 
    for(j in 1:length(variable.cols)){
        var_data <- program_data[,c(lakeid.col,variable.cols[j])] %>% drop_na() %>% filter(!!sym(variable.names[j])>=0)
        if(nrow(var_data >=20)) {
            out <- dip.test(log10(var_data[,2]+1),simulate.p.value = TRUE,B=2000)
            temp_stats <- data.frame(programid=programs[i], variable=variable.names[j],pvalue=out$p.value)
            temp_flag <- rbind(temp_flag,temp_stats)
        }
    }
}
temp_flag <- temp_flag %>% filter(variable != "noah") %>% 
    filter(pvalue <= 0.05) %>% 
    arrange(variable)

j=1
plot = list()

pdf(file=paste("output/multimodel.pdf",sep=""),width=8.5,height=10,onefile=TRUE,pointsize=10)
par(mfrow=c(5,3), cex=1,ps=10,mar=c(2.2,1,0,0) + 0.1,oma=c(0,0,2,0),mgp=c(1.2,.5,0),pty="s")

for(i in 1:nrow(temp_flag)) {
    temp_data <- Data %>% filter(programname_lagos_us == temp_flag[i,1]) %>% 
        select((!!sym(temp_flag[i,2]))) %>% drop_na()
    plot[[j]] <- ggplot(data = temp_data,aes(x=(!!sym(temp_flag[i,2]))+1)) + 
        geom_density() + 
        labs(title=temp_flag[i,1]) +
        scale_x_log10()
    if (j %% 12 == 0) { ## print 8 plots on a page
        print (do.call(grid.arrange,  c(plot,ncol=3)))
        plot = list() # reset plot 
        j = 0 # reset index
    }
    j = j + 1
}
if (length(plot) != 0) { 
    print (do.call(grid.arrange,  plot))
}
dev.off()
graphics.off()
