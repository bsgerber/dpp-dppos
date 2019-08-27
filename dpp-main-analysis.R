###########################################################
#
# DPP-DPPOS
# Bernice Man's DPP GDM Analysis (Prediction Model)
#
# Started 8/27/19
#
# Description:
#
###########################################################

# Libraries
library(sas7bdat)

# Read in selected data files 
# Forms:  S03, S05, Q03
# Nonforms: basedata.sas7bdat, lab.sas7bdat, events.sas7bdat

S03 <- read.sas7bdat("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\Unzipped\\Data\\DPP_Data_2008\\Form_Data\\Data\\s03.sas7bdat")
S05 <- read.sas7bdat("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\Unzipped\\Data\\DPP_Data_2008\\Form_Data\\Data\\s05.sas7bdat")
Q03 <- read.sas7bdat("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\Unzipped\\Data\\DPP_Data_2008\\Form_Data\\Data\\q03.sas7bdat")

basedata <- read.sas7bdat("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\Unzipped\\Data\\DPP_Data_2008\\Non-Form_Data\\Data\\basedata.sas7bdat")
lab <- read.sas7bdat("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\Unzipped\\Data\\DPP_Data_2008\\Non-Form_Data\\Data\\lab.sas7bdat")
events <- read.sas7bdat("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\Unzipped\\Data\\DPP_Data_2008\\Non-Form_Data\\Data\\events.sas7bdat")


