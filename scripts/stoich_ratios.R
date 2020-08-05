#setup the stoic ratio flag dataframe
ratio.flags = data.frame(lagoslakeid = NA,eventidb = NA,programid = NA,variable=NA,value=NA,flag=NA)
ratio.flags = ratio.flags[-c(1:nrow(ratio.flags)),]
#Assume obs +/- 1/3 error potential in quantifying ratios
s.ratio = (1+1*1/5)/(1-1*1/5)
s.ratio.l = (1-1*1/5)/(1+1*1/5)
######
#identify columns which contain P
tp.cols = c(match("tp",names(Data)),match("tdp",names(Data)),match("srp",names(Data)))

#Go through the data and look at all the stoich ratios associated with P
####TDP/TP Ratio
temp.data =Data[which(Data[,tp.cols[2]]/Data[,tp.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,tp.cols)]
if(nrow(temp.data)>0) {
temp.data$variable = "tdp"
temp.data$flag = "RATIO"
flag.data = temp.data[,c(1,2,3,7,5,8)]
names(flag.data)[5] = "value"
ratio.flags = rbind(ratio.flags,flag.data)

temp.data$variable = "tp"
temp.data$flag = "RATIO"
flag.data = temp.data[,c(1,2,3,7,4,8)]
names(flag.data)[5] = "value"
ratio.flags = rbind(ratio.flags,flag.data)
}

#####SRP/TP Ratio
temp.data =Data[which(Data[,tp.cols[3]]/Data[,tp.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,tp.cols)]
if(nrow(temp.data)>0) {
temp.data$variable = "srp"
temp.data$flag = "RATIO"
flag.data = temp.data[,c(1,2,3,7,6,8)]
names(flag.data)[5] = "value"
ratio.flags = rbind(ratio.flags,flag.data)

temp.data$variable = "tp"
temp.data$flag = "RATIO"
flag.data = temp.data[,c(1,2,3,7,4,8)]
names(flag.data)[5] = "value"
ratio.flags = rbind(ratio.flags,flag.data)
}

#####SRP/TDP Ratio
temp.data =Data[which(Data[,tp.cols[3]]/Data[,tp.cols[2]]>s.ratio),c(lakeid.col,event.col,programid.col,tp.cols)]
if(nrow(temp.data)>0) {
temp.data$variable = "srp"
temp.data$flag = "RATIO"
flag.data = temp.data[,c(1,2,3,7,6,8)]
names(flag.data)[5] = "value"
ratio.flags = rbind(ratio.flags,flag.data)

temp.data$variable = "tdp"
temp.data$flag = "RATIO"
flag.data = temp.data[,c(1,2,3,7,5,8)]
names(flag.data)[5] = "value"
ratio.flags = rbind(ratio.flags,flag.data)
}
#######
tn.cols = c(match("tn",names(Data)),
            match("ton",names(Data)),
            match("tkn",names(Data)),
            match("tdn",names(Data)),
            match("no2no3",names(Data)),
            match("no2",names(Data)),
            match("nh4",names(Data)),
            match("dkn",names(Data)))

tn.cols <- replace_na(tn.cols,program.col)

#####TON/TN Ratio
temp.data =Data[which(Data[,tn.cols[2]]/Data[,tn.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
if(nrow(temp.data)>0) {
temp.data$variable = "ton"
temp.data$flag = "RATIO"
flag.data = temp.data[,c(1,2,3,12,5,13)]
names(flag.data)[5] = "value"
ratio.flags = rbind(ratio.flags,flag.data)

temp.data$variable = "tn"
temp.data$flag = "RATIO"
flag.data = temp.data[,c(1,2,3,12,4,13)]
names(flag.data)[5] = "value"
ratio.flags = rbind(ratio.flags,flag.data)
}

####TKN/TN Ratio
temp.data =Data[which(Data[,tn.cols[3]]/Data[,tn.cols[1]] > s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
if(nrow(temp.data)>0) {
temp.data$variable = "tkn"
temp.data$flag = "RATIO"
flag.data = temp.data[,c(1,2,3,12,6,13)]
names(flag.data)[5] = "value"
ratio.flags = rbind(ratio.flags,flag.data)

temp.data$variable = "tn"
temp.data$flag = "RATIO"
flag.data = temp.data[,c(1,2,3,12,4,13)]
names(flag.data)[5] = "value"
ratio.flags = rbind(ratio.flags,flag.data)
}

####TDN/TN RATIO
# temp.data =Data[which(Data[,tn.cols[4]]/Data[,tn.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
# if(nrow(temp.data)>0) {
#   temp.data$variable = "tdn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,7,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
#   
#   temp.data$variable = "tn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,4,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
# }

####NO2NO3/TN RATIO
temp.data =Data[which(Data[,tn.cols[5]]/Data[,tn.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
if(nrow(temp.data)>0) {
  temp.data$variable = "no2no3"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,8,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
  
  temp.data$variable = "tn"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,4,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
}

####NO2/TN Ratio
# temp.data =Data[which(Data[,tn.cols[6]]/Data[,tn.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
# if(nrow(temp.data)>0) {
#   temp.data$variable = "no2"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,9,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
#   
#   temp.data$variable = "tn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,4,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
# }
# 
####NH4/TN Ratio
temp.data =Data[which(Data[,tn.cols[7]]/Data[,tn.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
if(nrow(temp.data)>0) {
  temp.data$variable = "nh4"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,10,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
  
  temp.data$variable = "tn"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,4,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
}

####DKN/TN
# temp.data =Data[which(Data[,tn.cols[8]]/Data[,tn.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
# if(nrow(temp.data)>0) {
#   temp.data$variable = "dkn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,11,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
#   
#   temp.data$variable = "tn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,4,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
# }

####NH4/TDN
# temp.data =Data[which(Data[,tn.cols[7]]/Data[,tn.cols[4]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
# if(nrow(temp.data)>0) {
#   temp.data$variable = "tdn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data %>% select(lagoslakeid,eventidb,programid,variable,tdn,flag)
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
#   
#   temp.data$variable = "nh4"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data %>% select(lagoslakeid,eventidb,programid,variable,nh4,flag)
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
# }

####no2no3/TDN
# temp.data =Data[which(Data[,tn.cols[5]]/Data[,tn.cols[4]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
# if(nrow(temp.data)>0) {
#   temp.data$variable = "tdn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data %>% select(lagoslakeid,eventidb,programid,variable,tdn,flag)
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
#   
#   temp.data$variable = "no2no3"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data %>% select(lagoslakeid,eventidb,programid,variable,no2no3,flag)
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
# }

####no2/TDN
# temp.data =Data[which(Data[,tn.cols[6]]/Data[,tn.cols[4]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
# if(nrow(temp.data)>0) {
#   temp.data$variable = "tdn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data %>% select(lagoslakeid,eventidb,programid,variable,tdn,flag)
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
#   
#   temp.data$variable = "no2"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data %>% select(lagoslakeid,eventidb,programid,variable, no2,flag)
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
# }

####dkn/TDN
# temp.data =Data[which(Data[,tn.cols[8]]/Data[,tn.cols[4]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
# if(nrow(temp.data)>0) {
#   temp.data$variable = "tdn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data %>% select(lagoslakeid,eventidb,programid,variable,tdn,flag)
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
#   
#   temp.data$variable = "dkn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data %>% select(lagoslakeid,eventidb,programid,variable, dkn,flag)
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
# }

####NO2/NO3NO2 Ratio
# temp.data =Data[which(Data[,tn.cols[6]]/Data[,tn.cols[5]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
# if(nrow(temp.data)>0) {
#   temp.data$variable = "no2"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,9,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
#   
#   temp.data$variable = "no2no3"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,8,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
# }

####DKN/TKN Ratio
# temp.data =Data[which(Data[,tn.cols[8]]/Data[,tn.cols[3]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
# if(nrow(temp.data)>0) {
#   temp.data$variable = "dkn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,11,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
#   
#   temp.data$variable = "tkn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,6,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
# }

####NH4/TKN Ratio
temp.data =Data[which(Data[,tn.cols[7]]/Data[,tn.cols[3]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
if(nrow(temp.data)>0) {
  temp.data$variable = "nh4"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,10,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
  
  temp.data$variable = "tkn"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,6,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
}

####NH4/DKN Ratio
# temp.data =Data[which(Data[,tn.cols[7]]/Data[,tn.cols[8]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
# if(nrow(temp.data)>0) {
#   temp.data$variable = "nh4"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,10,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
#   
#   temp.data$variable = "dkn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,11,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
# }

####TON/TKN Ratio
temp.data =Data[which(Data[,tn.cols[2]]/Data[,tn.cols[3]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
if(nrow(temp.data)>0) {
  temp.data$variable = "ton"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,5,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
  
  temp.data$variable = "tkn"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,6,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
}

#The next chunk of code determines ratios based on sums of different components (e.g. DIN !> TN)
####(NO2NO3+NH4)/TN
temp.data =Data[which((Data[,tn.cols[5]]+Data[,tn.cols[7]])/Data[,tn.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
if(nrow(temp.data)>0) {
  temp.data$variable = "no2no3"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,8,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
  
  temp.data$variable = "nh4"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,10,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
  
  temp.data$variable = "tn"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,4,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
}

####(NO2NO3+NH4)/TDN
# temp.data =Data[which((Data[,tn.cols[5]]+Data[,tn.cols[7]])/Data[,tn.cols[4]]>s.ratio),c(lakeid.col,event.col,programid.col,tn.cols)]
# if(nrow(temp.data)>0) {
#   temp.data$variable = "no2no3"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,8,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
#   
#   temp.data$variable = "nh4"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,10,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
#   
#   temp.data$variable = "tdn"
#   temp.data$flag = "RATIO"
#   flag.data = temp.data[,c(1,2,3,12,7,13)]
#   names(flag.data)[5] = "value"
#   ratio.flags = rbind(ratio.flags,flag.data)
# }

####(TN-TKN)/NO2NO3
temp.data =Data[which((Data[,tn.cols[3]]+Data[,tn.cols[5]])/Data[,tn.cols[1]]>s.ratio | (Data[,tn.cols[3]]+Data[,tn.cols[5]])/Data[,tn.cols[1]]<s.ratio.l ),c(lakeid.col,event.col,programid.col,tn.cols)]
if(nrow(temp.data)>0) {
  temp.data$variable = "no2no3"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,8,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
  
  temp.data$variable = "tkn"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,6,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
  
  temp.data$variable = "tn"
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,12,4,13)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
}




######
#identify columns which contain Carbon
# carb.cols = c(which(names(Data)=="toc"),which(names(Data)=="doc"))

#Go through the data and look at all the stoich ratios associated with P
####TDP/TP Ratio
# temp.data =Data[which(Data[,carb.cols[2]]/Data[,carb.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,carb.cols)]
# if(nrow(temp.data)>0){
# temp.data$variable = "doc"
# temp.data$flag = "RATIO"
# flag.data = temp.data[,c(1,2,3,6,5,7)]
# names(flag.data)[5] = "value"
# ratio.flags = rbind(ratio.flags,flag.data)
# 
# temp.data$variable = "toc"
# temp.data$flag = "RATIO"
# flag.data = temp.data[,c(1,2,3,6,4,7)]
# names(flag.data)[5] = "value"
# ratio.flags = rbind(ratio.flags,flag.data)
# }

######
#identify columns which contain Carbon
var.cols = c(which(names(Data)=="al_tot"),which(names(Data)=="al_diss"))

#Go through the data and look at all the stoich ratios associated with P
####TDP/TP Ratio
temp.data =Data[which(Data[,var.cols[2]]/Data[,var.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,var.cols)]
if(nrow(temp.data)>0){
temp.data$variable = names(Data)[var.cols[2]]
temp.data$flag = "RATIO"
flag.data = temp.data[,c(1,2,3,6,5,7)]
names(flag.data)[5] = "value"
ratio.flags = rbind(ratio.flags,flag.data)

temp.data$variable = names(Data)[var.cols[1]]
temp.data$flag = "RATIO"
flag.data = temp.data[,c(1,2,3,6,4,7)]
names(flag.data)[5] = "value"
ratio.flags = rbind(ratio.flags,flag.data)
}

######
#identify columns which contain Carbon
var.cols = c(which(names(Data)=="as_tot"),which(names(Data)=="as_diss"))

#Go through the data and look at all the stoich ratios associated with P
####TDP/TP Ratio
temp.data =Data[which(Data[,var.cols[2]]/Data[,var.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,var.cols)]
if(nrow(temp.data)>0){
  temp.data$variable = names(Data)[var.cols[2]]
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,6,5,7)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
  
  temp.data$variable = names(Data)[var.cols[1]]
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,6,4,7)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
}

######
#identify columns which contain Carbon
var.cols = c(which(names(Data)=="atz_tot"),which(names(Data)=="atz_diss"))

#Go through the data and look at all the stoich ratios associated with P
####TDP/TP Ratio
temp.data =Data[which(Data[,var.cols[2]]/Data[,var.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,var.cols)]
if(nrow(temp.data)>0){
  temp.data$variable = names(Data)[var.cols[2]]
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,6,5,7)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
  
  temp.data$variable = names(Data)[var.cols[1]]
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,6,4,7)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
}

######
#identify columns which contain Carbon
var.cols = c(which(names(Data)=="hg_tot"),which(names(Data)=="hg_tot_diss"))

#Go through the data and look at all the stoich ratios associated with P
####TDP/TP Ratio
temp.data =Data[which(Data[,var.cols[2]]/Data[,var.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,var.cols)]
if(nrow(temp.data)>0){
  temp.data$variable = names(Data)[var.cols[2]]
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,6,5,7)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
  
  temp.data$variable = names(Data)[var.cols[1]]
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,6,4,7)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
}


######
#identify columns which contain Carbon
var.cols = c(which(names(Data)=="se_tot"),which(names(Data)=="se_diss"))

#Go through the data and look at all the stoich ratios associated with P
####TDP/TP Ratio
temp.data =Data[which(Data[,var.cols[2]]/Data[,var.cols[1]]>s.ratio),c(lakeid.col,event.col,programid.col,var.cols)]
if(nrow(temp.data)>0){
  temp.data$variable = names(Data)[var.cols[2]]
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,6,5,7)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
  
  temp.data$variable = names(Data)[var.cols[1]]
  temp.data$flag = "RATIO"
  flag.data = temp.data[,c(1,2,3,6,4,7)]
  names(flag.data)[5] = "value"
  ratio.flags = rbind(ratio.flags,flag.data)
}



temp_flag <- data.frame(eventida=ratio.flags[,2],
                        lagoslakeid = ratio.flags[,1], 
                        variable=ratio.flags[,4],
                        value=ratio.flags[,5],
                        flag ="ratio")


# outlier.flags = rbind(outlier.flags,ratio.flags)
# 
# outlier.flags = outlier.flags[!duplicated(outlier.flags[,c(2,4)]),]
# out.vars = unique(ratio.flags$variable)
# 
# for(i in 1:length(out.vars)){
#   which.col = which(names(Data)==out.vars[i])
#   temp.data = ratio.flags[which(ratio.flags$variable==out.vars[i]),]
#   Data[match(temp.data$eventidb,Data$eventidb),which.col] = NA
# }
