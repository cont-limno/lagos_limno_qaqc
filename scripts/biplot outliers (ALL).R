require(gplots)
library(DMwR)
library(FNN)
library(robustbase)
library(scales)
library(gridExtra)
library(mvoutlier)
# library(MVN)
library(rstatix)

outlier.dist = function(dat,k=4,method = "brute",iqr=15,alpha=0.0001) {
  #function to estimate euclidian distances between all points in a pairwise dataset (X)
  #distance to the k nearest neighbor is estimated and robust boxplot is used to identify
  #distances that are considered outliers at specified iqr value
  dist = knn.dist(data = dat,k = k,algorithm = method)
  scores = dist[,k]
  box = adjbox(scores,range=iqr,plot = FALSE)
  if (box$fence[2]==0) out.box = box$out>max(scores) else out.box = box$out>box$fence[2]
  box = box$out[out.box]
  out <- dat %>% mutate(outlier1 = (scores %in% box))
  #normal Mahalab
  threshold <- stats::qchisq(1-alpha, dim(dat)[2])
  .data <- dat %>% 
    as.matrix()
  distance <- stats::mahalanobis(
    .data,
    center = colMeans(.data),
    cov = cov(.data)
  )
  out <- out %>% mutate(outlier2 = distance > threshold)
  return(out)
}

#setup the flags dataframe
out.flags = data.frame(lagoslakeid = NA,eventidb = NA,programid = NA,variable=NA,value=NA,flag=NA)
out.flags = out.flags[-1,]

combinations = combn(x = c(1:length(variable.cols)),m = 2,simplify = TRUE)
combinations = data.frame(x=combinations[1,],y=combinations[2,])
first.cols = c(lakeid.col,event.col,programid.col)

options(device="quartz")

pdf(file=paste("output/BiPlot.pdf",sep=""),width=8.5,height=10,onefile=TRUE,pointsize=10)
par(mfrow=c(5,3), cex=1,ps=10,mar=c(2.2,1,0,0) + 0.1,oma=c(0,0,2,0),mgp=c(1.2,.5,0),pty="s")
i=1
plot = list()

for (s in 1:nrow(combinations)) { 
  x.col = variable.cols[combinations[s,1]]
  y.col = variable.cols[combinations[s,2]]
  X = Data[,c(first.cols,x.col,y.col)] %>% drop_na() %>% # lines 41 and 42 to be deleted after I have incorporated range checks
    gather(key = "parameter",value= "value", -lagoslakeid,-eventida,-programid_lagos_us) %>% 
    filter(value > 0) %>% spread(value = value, key = parameter) %>% drop_na()
  
  if(nrow(X) >= 100) { #only run this analysis if there are 100 pairs of data
    raw.data <- X
    X[,4] = log(X[,4]+1) #log transform data
    X[,5] = log(X[,5]+1)
    plot.data = outlier.dist(dat = X[,4:5]) #identify the outliers

    #create plots
    plot[[i]] <- ggplot(data = plot.data ,aes_string(x=names(plot.data)[1],y=names(plot.data)[2])) + geom_bin2d(bins=100) +
            geom_point(data = plot.data %>% filter(outlier2==TRUE),
                       aes_string(x=names(plot.data)[1],y=names(plot.data)[2]),colour=alpha("red",0.5),size=2) +
      geom_point(data = plot.data %>% filter(outlier1==TRUE),
                 aes_string(x=names(plot.data)[1],y=names(plot.data)[2]),colour=alpha("purple",0.5),size=2) +
      lims(x=range(plot.data[,1]),y=range(plot.data[,2]))
    
    #extract points to flag as outliers
    new.data = cbind(raw.data,plot.data[,3])
    new.data = new.data[which(new.data[,6]==TRUE),]
    if(nrow(new.data)>0) {
      new.data$flag = "BIPLOT"
      new.data$variable = names(new.data)[4]
      temp.flag = new.data[,c(1,2,3,8,4,7)]
      names(temp.flag)[5] = "value"
      out.flags = rbind(out.flags,temp.flag)
      new.data$variable = names(new.data)[5]
      temp.flag = new.data[,c(1,2,3,8,5,7)]
      names(temp.flag)[5] = "value"  
      out.flags = rbind(out.flags,temp.flag)
    }
    if (i %% 6 == 0) { ## print 8 plots on a page
      print (do.call(grid.arrange,  c(plot,ncol=2)))
      plot = list() # reset plot 
      i = 0 # reset index
    }
    i = i + 1
  }
  }
  if (length(plot) != 0) { 
    print (do.call(grid.arrange,  plot))
  }
  dev.off()
  graphics.off()

    
#     
#     
#     
#     
#     
#   if(length(na.omit(X[,4]))>0) {
#     lower = min(X[,4],na.rm=TRUE)
#     x.adj = abs(lower-1)
#     if(lower < 1) {
#       xadj=TRUE
#       X[,4] = X[,4]+x.adj
#     }
#     Y = Data[,y.col]
#     if(length(na.omit(Y))>0) {
#       lower = min(Y,na.rm=TRUE)
#       y.adj = abs(lower-1)
#       if(lower < 1) {
#         yadj =TRUE
#         Y = Y+y.adj
#       }
#       new.data = data.frame(X,Y)
#       names(new.data)[4:5] = c(variable.names[combinations[s,1]],variable.names[combinations[s,2]])
#       new.data = new.data[complete.cases(new.data),]
#       if(nrow(new.data)==0) {
#         plot[[i]] <- ggplot(data = new.data, aes_string(x=names(new.data)[4],y=names(new.data)[5])) + annotate("text",label = "No Data",x=0.1,y=0.5)
#       } else {
#         raw.data = new.data
#         new.data[,c(4:5)] = log(new.data[,c(4:5)])
#         if(nrow(new.data)>20){
#           plot.data = outlier.dist(new.data[,c(4:5)],k=10,iqr=25) #10 in 871
#           plot[[i]] <- ggplot(data = plot.data %>% filter(Outlier==FALSE),aes_string(x=names(plot.data)[1],y=names(plot.data)[2])) + geom_bin2d(bins=100) +
#             geom_point(data = plot.data %>% filter(Outlier==TRUE),aes_string(x=names(plot.data)[1],y=names(plot.data)[2]),colour=alpha("red",0.5),size=2) +
#             lims(x=range(plot.data[,1]),y=range(plot.data[,2]))
#           new.data = cbind(raw.data,plot.data[,3])
#           new.data = new.data[which(new.data[,6]==TRUE),]
#           if(nrow(new.data)>0) {
#             new.data$flag = "BIPLOT"
#             new.data$variable = names(new.data)[4]
#             temp.flag = new.data[,c(1,2,3,8,4,7)]
#             names(temp.flag)[5] = "value"
#             out.flags = rbind(out.flags,temp.flag)
#             new.data$variable = names(new.data)[5]
#             temp.flag = new.data[,c(1,2,3,8,5,7)]
#             names(temp.flag)[5] = "value"  
#             out.flags = rbind(out.flags,temp.flag)
#           }
#         } else { plot[[i]] <- ggplot(data = new.data, aes_string(x=names(new.data)[4],y=names(new.data)[5])) + geom_point() }
#       } 
#     } else { plot[[i]] <- ggplot(data = new.data, aes_string(x=names(new.data)[4],y=names(new.data)[5])) + annotate("text",label = "No Data",x=0.1,y=0.5) 
#       }  
#   }
#   if (i %% 6 == 0) { ## print 8 plots on a page
#     print (do.call(grid.arrange,  c(plot,ncol=2)))
#     plot = list() # reset plot 
#     i = 0 # reset index
#   }
#   i = i + 1
# }
# if (length(plot) != 0) { 
#   print (do.call(grid.arrange,  plot))
# }
# dev.off()
# graphics.off()
# 
# 
# out.flags = out.flags[!duplicated(out.flags[,c(2,4)]),]
# 
# outlier.flags = rbind(outlier.flags,out.flags)
# 
# out.vars = unique(out.flags$variable)
# 
# for(i in 1:length(out.vars)){
#   which.col = which(names(Data)==out.vars[i])
#   temp.data = out.flags[which(out.flags$variable==out.vars[i]),]
#   Data[match(temp.data$eventidb,Data$eventidb),which.col]
#   Data[match(temp.data$eventidb,Data$eventidb),which.col] = NA
# }
