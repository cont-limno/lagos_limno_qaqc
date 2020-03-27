library(Hmisc)
library(robustbase)
library(tidyverse)
library(lubridate)
library(clipr)
rm(list=ls())

param_names <- unlist(strsplit( #names of the parameters included in LAGOS_US
"alk
al_tot
al_diss
as_tot
as_diss
atz_tot
atz_diss
ca_diss
chla
cl_diss
color_a
color_t
cond
do
doc
ecoli_mpn
ecoli_cfu
mg_diss
hgmeth_tot
hg_tot
hg_totdiss
mcystn
nh4
no2no3
ph_eq
ph_field
salinity
se_tot
se_diss
secchi
si_diss
so4_diss
srp
tss
temp
tdp
tkn
tn
ton
tp
turb",split = '\n'))

#Get Data
file.loc <- "~/Dropbox/CSI_LIMNO/CSI_LAGOS-exports/LAGOS-LIMNO/US_beta/" #location of file for NRL

Data <- read.table(paste(file.loc,"epi_waterquality_export.csv",sep=""), 
                   header = TRUE, 
                   sep = ",", 
                   quote = "\"", 
                   dec = ".",
                   strip.white=TRUE,
                   comment.char = "",
                   stringsAsFactors = FALSE)

#bookkeeping stuff so scripts have appropriate information to run
programs = as.character(unique(Data$programname_lagos_us))
lakeid.col = which(names(Data)=="lagoslakeid")
event.col = which(names(Data)=="eventida")
program.col = which(names(Data)=="programname_lagos_us")
programid.col = which(names(Data)=="programid_lagos_us")
Data$sampledate <- ymd(Data$sampledate)
date.col = which(names(Data)=="sampledate")

variable.cols = which(names(Data) %in% param_names)
(variable.names = names(Data)[variable.cols])
total_obs <- Data[,variable.cols] %>% 
  gather(key = variable,value = value) %>% drop_na() %>% 
  group_by(variable) %>% 
  summarize(total_obs = n())
write_clip(total_obs,return_new=TRUE) # Look at the number of data points and variables (copies data to clipboard)

# #generate histograms and boxplots of variables by program
# censored = 0 # set to 1 to only generate histograms of sensored data, 0 for all data
# source("Program_Fequency_Plots.R")
# #Copy pdf output to appropriate folder
# #Look at program frequency and see if program issues exist
# 
# #Check for any missing censor codes
# #####NOTE rewrite some error checking code for sensor_code names
# source("missing_censor_codes.R") #look at temp.flags
# 
# #Identify, flag, and remove non_cen_zeros
# source("Non_cen_zeros.R")
# 
# #Generate plots of biplot slopes by program
# source("biplot_slopes.R")
# #Copy pdf output to appropriate folder
# #Look at Biplots and see if program issues exist
# 
# #Identify and flag values that flag the ceiling and mav limits
# source("MAVS.R")

#estimate euclidian distances between all points and identify points that are long way
#away from their nearist nth neighbor
source("scripts/biplot outliers (ALL).R")
# 
# source("OutlierDetection.R") #used iqr = 8, p = 0.0001
# source("stripoutliers.R")
# 
# #Identifies variables values that are not stoichimetrically feasible and removes them
# #from the data and assigns appropriate flags
# source("stoich_ratios.R")
# 
# keep.data = c("outlier.flags","total_obs")
# rm(list=setdiff(ls(), keep.data))
# #####Now Handle Secchi
# Data <- read.table("~/Dropbox/CSI_LIMNO/CSI_LAGOS-exports/LAGOS-LIMNO/Version1.087.2/exports/all_depths_secchi_export_updated_programs.csv", 
#                    header = TRUE, 
#                    sep = ",", 
#                    quote = "\"", 
#                    dec = ".",
#                    strip.white=TRUE,
#                    comment.char = "",
#                    stringsAsFactors = FALSE)
# 
# Data <- Data %>% rename(eventidb = eventidb_secchi)
# variable.cols = c(7)
# lakeid.col = which(names(Data)=="lagoslakeid")
# event.col = which(names(Data)=="eventidb")
# program.col = which(names(Data)=="programid")
# programid.col = which(names(Data)=="programid")
# variable.names = names(Data)[variable.cols]
# Data$date = as.Date(Data$date)
# date.col = which(names(Data)=="date")
# total_obs[nrow(total_obs)+1,] <- c("secchi",nrow(Data))
# total_obs
# 
# source("MAVS.R")
# source("OutlierDetection.R") #used iqr = 8, p = 0.0001
# source("stripoutliers.R")
# 
# #cleanup the data environement
# keep.data = c("outlier.flags","total_obs")
# rm(list=setdiff(ls(), keep.data))
# save.image("v10872_final.RData")
# 
# write.table(outlier.flags,"Ver872_flagged_data.csv",row.names=FALSE,sep=",")
# 
# 
