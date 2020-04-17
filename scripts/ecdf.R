if (!require("pacman")) install.packages("pacman")
pacman::p_load(robustbase,gplots,scales,gridExtra)

pdf(file=paste("output/ecdf.pdf",sep=""),width=8.5,height=10,onefile=TRUE,pointsize=10)
# par(mfrow=c(5,3), cex=1,ps=10,mar=c(2.2,1,0,0) + 0.1,oma=c(0,0,2,0),mgp=c(1.2,.5,0),pty="s")
z=1
plot = list()

for (i in 1:length(variable.cols)) {
    #change column number to match programname
    variable.data = Data[,c(program.col,variable.cols[i])] %>% 
        gather(value="value",key="key",-programname_lagos_us) %>%
        drop_na() %>% 
        filter(value >=0) %>% 
        # mutate(value = value + max(1-min(value),0)) %>% 
        group_by(programname_lagos_us) %>% 
        mutate(n=n()) %>% 
        ungroup() %>% 
        filter(n > 10)
    if(nrow(variable.data)>10) {
    plot[[z]] <- ggplot(data = variable.data,aes(x=value,color=programname_lagos_us)) + 
        stat_ecdf(geom = "point",alpha=0.5) + 
        scale_x_log10(labels = scales::comma) +
        labs(x=variable.names[i],y="ECFD") +
        geom_vline(xintercept = 1)

    if (z %% 3 == 0) { ## print 8 plots on a page
        print (do.call(grid.arrange,  c(plot,ncol=1)))
        plot = list() # reset plot 
        z = 0 # reset index
    }
    z = z + 1
    }
}
if (length(plot) != 0) { 
    print (do.call(grid.arrange,  plot))
}
dev.off()