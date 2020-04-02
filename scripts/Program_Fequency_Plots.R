if (!require("pacman")) install.packages("pacman")
pacman::p_load(robustbase,gplots,scales,gridExtra)

pdf(file=paste("output/histograms.pdf",sep=""),width=8.5,height=10,onefile=TRUE,pointsize=10)
# par(mfrow=c(5,3), cex=1,ps=10,mar=c(2.2,1,0,0) + 0.1,oma=c(0,0,2,0),mgp=c(1.2,.5,0),pty="s")
z=1
plot = list()

for (i in 1:length(variable.cols)) {
  #change column number to match programname
  variable.data = Data[,c(program.col,variable.cols[i])]#,(variable.cols[i]+37))]
  variable.data = variable.data[complete.cases(variable.data),]
  #Get a comparison dataset to for visualizing a programs data relative to the entire dataset (using a trimmed dataset to
  #decrease the importance of extreme values)
  comparison.data <- variable.data %>% gather(value="value",key="key",-programname_lagos_us) %>% 
    filter(between(value,quantile(x = value,probs=0.05),quantile(x = value,probs=0.95)))
  
  if(length(unique(comparison.data$programname_lagos_us))>=2) {

  # if(censored == 1){
  # variable.data = variable.data[which(variable.data$chla_censorcode != "NC1"),]
  # variable.data = variable.data[which(variable.data$chla_censorcode != "NC2"),]
  # variable.data = variable.data[which(variable.data$chla_censorcode != "NC3"),]
  # variable.data = variable.data[which(variable.data$chla_censorcode != "NC4"),]
  # }
  #   count.program = 0
  # pdf(file=paste("output/",names(variable.data)[2],".pdf",sep=""),width=8.5,height=10,onefile=TRUE,pointsize=10)
  # par(mfrow=c(5,2), cex=1,ps=10,mar=c(2.25,1.25,2.25,.25) + 0.1,oma=c(0,1,0,0),mgp=c(1.2,.5,0))
  
  programs.histogram <- unique(variable.data$programname_lagos_us)
  
  for(n in 1:length(programs.histogram)) {
    program.data = variable.data[which(variable.data$programname_lagos_us==programs.histogram[n]),] %>% 
      gather(value="value",key="parameter",-programname_lagos_us)
    
    if(length(program.data[,2])>0){
      lower = min(variable.data[,2])
      adj = abs(lower-1)
      if(lower<1) adj = adj else adj = 0
      plot[[z]] <- ggplot(data = comparison.data %>% filter(programname_lagos_us != programs.histogram[n]),
             aes(x=value+adj)) + 
        geom_histogram(fill="green",alpha=0.5,aes(y = stat(width*density))) + 
        geom_histogram(data = program.data,aes(x=value+adj,y=stat(width*density)),fill="red",alpha=0.5) +
        scale_x_log10() +
        scale_y_continuous(labels = percent_format()) +
        labs(title=programs.histogram[n],y="Percent Data",x=variable.names[i])
      
      z <- z +1
      
      plot[[z]] <- ggplot(data = comparison.data %>% filter(programname_lagos_us != programs.histogram[n]),
             aes(y=value+adj,x="Trimmed")) + 
        geom_boxplot() + 
        geom_boxplot(data = program.data,aes(y=value+adj,x="program")) +
        scale_y_log10() +
        labs(title=programs.histogram[n],y="Value",x=variable.names[i])
      
      
      # hist(log10(program.data[,2]),main=programs.histogram[n],xlab=paste("log10(",names(program.data)[2],")"))
      # if(lower < 1) mtext(side=1,line=1.2,adj=0.01,paste("0 =",lower))
      # adjbox(log10(program.data[,2]),horizontal=TRUE,main=programs.histogram[n],xlab=paste("log10(",names(program.data)[2],")"))
      # mtext(side=3,line=-1,text=paste("n = ",length(program.data[,2]),sep=""))
      # mtext(side=3,line=-2,text = paste("sdev = ",signif(sd(program.data[,2],na.rm=TRUE),digits =2),sep=""))
      # if(lower < 1) mtext(side=1,line=1.2,adj=0.01,paste("0 =",lower))
      if (z %% 10 == 0) { ## print 8 plots on a page
        print (do.call(grid.arrange,  c(plot,ncol=2)))
        plot = list() # reset plot 
        z = 0 # reset index
      }
      z = z + 1
      }
  
    
  }
  }
}

  if (length(plot) != 0) { 
  print (do.call(grid.arrange,  plot))
}
dev.off()
graphics.off()
