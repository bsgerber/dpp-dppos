###########################################################
#
# DPP-DPPOS
# Bernice Man's DPP GDM Analysis (Prediction Model)
#
# Started 8/27/19
# Revised 10/22/19
#
# Description: 
# Develop a prediction model for women with gestational diabetes
# using data from the DPP trial. This data set will be read
# in by analysis 1 for descriptive table based on treatment group,
# and analysis 2 for modeling.
#
###########################################################

# Libraries
library(sas7bdat)
library(tidyverse)

# Get user name from Sys.info()
username <- Sys.info()[7]

# List paths for data directory
paths1 <- c(
  "C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\",
  "C:\\Users\\bernicem\\Box\\prediction model\\DPP-DPPOS\\Reproduced Analysis\\data\\",
  "/Users/Wholebean/R/DPP-DPPOS/Reproduced Analysis/data/"
)
names(paths1) <- c("bgerber", "bernicem", "Wholebean")

paths2 <- c(
  "Unzipped\\Data\\DPP_Data_2008\\Form_Data\\Data\\",
  "Unzipped\\Data\\DPP_Data_2008\\Form_Data\\Data\\",
  "Unzipped/Data/DPP_Data_2008/Form_Data/Data/"
)
names(paths2) <- c("bgerber", "bernicem", "Wholebean")

paths3 <- c(
  "Unzipped\\Data\\DPP_Data_2008\\Non-Form_Data\\Data\\",
  "Unzipped\\Data\\DPP_Data_2008\\Non-Form_Data\\Data\\",
  "Unzipped/Data/DPP_Data_2008/Non-Form_Data/Data/"
)
names(paths3) <- c("bgerber", "bernicem", "Wholebean")

# Set path based on user
setwd(paths1[username])

# Read in selected data files 
# Forms:  S03, S05, Q03
# Nonforms: basedata.sas7bdat, lab.sas7bdat, events.sas7bdat

S03 <- read.sas7bdat(paste0(paths1[username], paths2[username], "s03.sas7bdat"))
S05 <- read.sas7bdat(paste0(paths1[username], paths2[username], "s05.sas7bdat"))
Q03 <- read.sas7bdat(paste0(paths1[username], paths2[username], "q03.sas7bdat"))

basedata <- read.sas7bdat(paste0(paths1[username], paths3[username], "basedata.sas7bdat"))
lab <- read.sas7bdat(paste0(paths1[username], paths3[username], "lab.sas7bdat"))
events <- read.sas7bdat(paste0(paths1[username], paths3[username], "events.sas7bdat"))

activities <- read.csv("MAQactivities with Updated METs.csv")

###Bernice's new computer Feb 22,2021
S03 <- read.sas7bdat("/Users/berniceman/R/dpp-dppos/dppraw.data/dpp/Data/DPP_Data_2008/Form_Data/Data/s03.sas7bdat")
S05 <- read.sas7bdat("/Users/berniceman/R/dpp-dppos/dppraw.data/dpp/Data/DPP_Data_2008/Form_Data/Data/s05.sas7bdat")
Q03 <- read.sas7bdat("/Users/berniceman/R/dpp-dppos/dppraw.data/dpp/Data/DPP_Data_2008/Form_Data/Data/q03.sas7bdat")

basedata <- read.sas7bdat("/Users/berniceman/R/dpp-dppos/dppraw.data/dpp/Data/DPP_Data_2008/Non-Form_Data/Data/basedata.sas7bdat")
lab <- read.sas7bdat("/Users/berniceman/R/dpp-dppos/dppraw.data/dpp/Data/DPP_Data_2008/Non-Form_Data/Data/lab.sas7bdat")
events <- read.sas7bdat("/Users/berniceman/R/dpp-dppos/dppraw.data/dpp/Data/DPP_Data_2008/Non-Form_Data/Data/events.sas7bdat")

activities <- read.csv("/Users/berniceman/R/dpp-dppos/MAQactivities with Updated METs.csv")

# Select data of interest for S03, S05 (specify variables)
# Use "RUN" visit for Q03 physical activity data and lab data
# Determined by Notes/variables DPP 8-27-2019.docx

S03.sel <- S03 %>%
  select(c("RELEASE_ID", "VISIT", "SODIAB", "SOETHN", "SOHGHT1", "SOHGHT2", 
           "SOHGHT3", "SOSBPA", "SODBPA", "SOBMI", "SOLFAST", "SOL2HR", "SOPRTN", "SOHSP"))
S05.sel <- S05 %>%
  select(c("RELEASE_ID", "VISIT", "SIBIRTH", "SIMDIAB", "SIFDIAB", "SI100CG", "SISMOK","SIPRGEV",  "SIPCOS", "SIWSTC1",
           "SIWSTC2", "SIWSTC3", "SIHIP1", "SIHIP2", "SIHIP3"))
Q03.sel <- Q03 %>%
  filter(VISIT == "RUN")
lab.sel <- lab %>%
  filter(VISIT == "BAS")

# Merge data together into one data frame and create numeric ID
main.data <- basedata %>%
  left_join(events, by = "RELEASE_ID") %>%
  left_join(lab.sel, by = "RELEASE_ID") %>%
  left_join(S03.sel, by = "RELEASE_ID") %>%
  left_join(S05.sel, by = "RELEASE_ID") %>%
  mutate(RELEASE_ID = as.character(RELEASE_ID))

# Check number of distinct IDs
length(unique(Q03$RELEASE_ID))

# Inspect one of the duplicates that has two rows (example)
Q03 %>%
  filter(RELEASE_ID == "10033391") %>%
  select(c("VISIT", "KVSTTYP", "KVSTWK", ends_with("CODE"), "DAYSRAND"))

# Q03 has duplicate IDs, even after filtering by "RUN", evaluate before merging
Q03.sel %>%
  filter(duplicated(RELEASE_ID)) %>%
  select(RELEASE_ID)

# Show the top 10 rows with very large number of days to randomization
head(sort(Q03$DAYSRAND[Q03$VISIT == "RUN"]), 10)

# Since there are 2 rows for ~6 subjects' physical activity at run-in, 
# keep data with least number of days to randomization (least negative number is the maximum)
Q03.sel.uniq <- Q03.sel %>%
  mutate(RELEASE_ID = as.character(RELEASE_ID)) %>%
  group_by(RELEASE_ID) %>%
  slice(which.max(DAYSRAND)) %>%
  ungroup()

#######################################
# Convert activities to MET-hour/week
#######################################

# Show which rows have 'write-in' activities (these are currently ignored in lookup table activities)
Q03.sel.uniq %>%
  select(ends_with("CODE")) %>%
  filter_all(any_vars(. == 38))

# Calculate months, times/month, hours/time for each activity (A through L)
# To calculate MET–hour/Week: (#of months) x (times/month) x  (hours/time) ÷52 = hours/week
# Or variables: (KAAJAN-KAADEC)  x ( KAATIME) x  (KAAMIN/60 ) ÷52 = hours/week(MET value kcal/kg/hr) x (hr/week) 
# Note: multiply by # of METS based on activity code
for (x in toupper(letters[1:12])) {
  firstcol <- which(colnames(Q03.sel.uniq) == paste0("KA", x, "JAN"))
  lastcol <- which(colnames(Q03.sel.uniq) == paste0("KA", x, "DEC"))
  timecol <- which(colnames(Q03.sel.uniq) == paste0("KA", x, "TIME"))
  mincol <- which(colnames(Q03.sel.uniq) == paste0("KA", x, "MIN"))
  actcol <- which(colnames(Q03.sel.uniq) == paste0("KA", x, "CODE"))
  monthvar <- paste0("KMonth", x)
  METvar <- paste0("KMET", x)
  actvar <- paste0("KAct", x)
  Q03.sel.uniq[[actvar]] = activities[unlist(Q03.sel.uniq[, actcol]), "METs"]
  Q03.sel.uniq <- Q03.sel.uniq %>%
    mutate(!!monthvar := rowSums(.[c(firstcol:lastcol)], na.rm = T))
  Q03.sel.uniq[[METvar]] = (Q03.sel.uniq[[monthvar]] * Q03.sel.uniq[[timecol]] * Q03.sel.uniq[[mincol]] * Q03.sel.uniq[[actvar]]) / (60 * 52)
}

# For each subject, add up MET-hr/week for all activities
Q03.sel.uniq <- Q03.sel.uniq %>%
  mutate(totalMETs = rowSums(select(., c(starts_with("KMET"))), na.rm = T))

# Check conversions for activities (using A as example)
Q03.sel.uniq %>%
  with(table(KActA, KAACODE))

# Check MET calculation
Q03.sel.uniq %>%
  select(c(starts_with("KMET"), totalMETs)) %>%
  head(10)

# Find unmatched records between Q03.sel.uniq and main data (~3 records)
anti_join(main.data, Q03.sel.uniq, by = "RELEASE_ID")

# Merge Q03 activity METs data with main data
main.data <- main.data %>%
  left_join(select(Q03.sel.uniq, RELEASE_ID, totalMETs), by = "RELEASE_ID")

#######################################
# Label the factors
#######################################

# Variables to change to factors: "SEX", "SODIAB", "AGEGROUP", "RACE_ETH", "SOETHN", "SIMDIAB", "SIFDIAB",
#               "SI100CG", "SISMOK", "SIPCOS", "SOBMI", "BMI_CAT", "BMIGROUP", "SOPRTN", "DIABF",
#               "DIABV", "FASTHYPF", "SOHSP"
# compress AGEGROUP to 4 levels given small numbers in older age groups
main.data$SEX <- factor(main.data$SEX,
                        labels = c("Male", "Female"))
main.data$SODIAB <- factor(main.data$SODIAB,
                           labels = c("No", "Only during pregnancy", "Yes, borderline", "Yes", NA))
main.data$AGEGROUP <- factor(main.data$AGEGROUP,
                        labels = c("<40", "40-44", "45-49", "50-54", "55-59", "60+", "60+"))
#main.data$AGEGROUP <- factor(main.data$AGEGROUP,
                             #labels = c("<40", "40-44", "45-49", "50+", "50+", "50+", "50+"))
main.data$RACE_ETH <- factor(main.data$RACE_ETH,
                             labels = c("Caucasian", "African American", "Hispanic, of any race", "All other"))
main.data$SOETHN <- factor(main.data$SOETHN, 
                           levels = 1:7,
                           labels = c("Caucasian", "African American", "Native American or American Indian",
                                      "Eskimo", "Aleut", "Asian or Pacific Islander", "Other"))
main.data$SIMDIAB <- factor(main.data$SIMDIAB,
                            labels = c("Yes", "No", "Don't Know", NA))
main.data$SIFDIAB <- factor(main.data$SIFDIAB,
                            labels = c("Yes", "No", "Don't Know", NA))
main.data$SI100CG <- factor(main.data$SI100CG,
                            labels = c("Yes", "No"))
main.data$SISMOK <- factor(main.data$SISMOK,
                           labels = c("Current", "Former", NA))
main.data$SIPCOS <- factor(main.data$SIPCOS,
                           labels = c("Yes", "No", NA))
main.data$SOBMI <- factor(main.data$SOBMI, levels = 1:2,
                          labels = c("Yes", "No"))
main.data$BMI_CAT <- factor(main.data$BMI_CAT, 
                            labels = c("<26", "26 to <28", "28 to <30", "30 to <32", "32 to <34", "34 to <36", 
                                       "36 to <38", "38 to <40", "40 to <42", "42+"))
main.data$BMIGROUP <- factor(main.data$BMIGROUP,
                             labels = c("<30", "30 to <35", "35+"))
main.data$SOPRTN <- factor(main.data$SOPRTN, levels = 1:6,
                           labels = c("Negative", "Trace", "1+", "2+", "3+", "4+"))
main.data$DIABF <- factor(main.data$DIABF,
                          labels = c("No", "Yes"))
main.data$DIABV <- factor(main.data$DIABV,
                          labels = c("Month 6", "Year 1", "Month 18", "Year 2", "Month 30", "Year 3", "Month 42", "Year 4",
                                     "Month 54", "Year 5"))
main.data$FASTHYPF <- factor(main.data$FASTHYPF,
                             labels = c("No", "Yes"))
main.data$SOHSP <- factor(main.data$SOHSP,
                          labels = c("Yes", "No", NA))

#######################################
# Recode additional variables
#######################################

# Create new ethnicity variable- will use RACE_ETH

main.data[which(main.data$RACE_ETH == "Caucasian"), "Ethnic"] <- "Caucasian"
main.data[which(main.data$RACE_ETH == "African American"), "Ethnic"] <- "African American"
main.data[which(main.data$RACE_ETH == "Hispanic, of any race" & main.data$SOHSP == "Yes"), "Ethnic"] <- "Hispanic"
main.data[which(main.data$SOETHN == "Asian or Pacific Islander" & main.data$RACE_ETH == "All other"), "Ethnic"] <- "Asian"
main.data[which(main.data$RACE_ETH == "All other" & main.data$SOETHN != "Asian or Pacific Islander"), "Ethnic"] <- "Other"

table(main.data$Ethnic)

# Create average height, waist circumference and hip girth, and ratio (note removal NAs in average calculations)

main.data <- main.data %>%
  mutate(AvgHeight = rowMeans(.[, c("SOHGHT1", "SOHGHT2", "SOHGHT3")], na.rm = T),
         WaistCircAve = rowMeans(.[, c("SIWSTC1", "SIWSTC2", "SIWSTC3")], na.rm = T),
         HipGirthAve = rowMeans(.[, c("SIHIP1", "SIHIP2", "SIHIP3")], na.rm = T),
         WTHRatio = WaistCircAve/HipGirthAve)

# Create a family history variable, 'yes' if either mother or father with positive history
# Otherwise will be 'no'

main.data$FamHx <- factor(ifelse(main.data$SIMDIAB == "Yes" | main.data$SIFDIAB == "Yes",
                                 "Yes", "No"))

# Create logTRIG variable use natural logarithm per Herman
# Herman also excluded TG>= 1000 or on fenofibrate (consider)

main.data$logTRIG <- log(main.data$TRIG)

# Create Smoke variable to include non-smokers (<= 100 cig/lifetime), with former and current smokers
# First check to make sure all NA are <=100 cig/lifetime

table(main.data$SISMOK, main.data$SI100CG)

main.data$Smoke <- main.data$SISMOK
levels(main.data$Smoke) <- c(levels(main.data$Smoke), "<= 100 cig lifetime")
main.data$Smoke[is.na(main.data$Smoke)] <- "<= 100 cig lifetime"


#######################################
# Select population of interest
#######################################

# Ensure all IDs are unique
table(duplicated(main.data$RELEASE_ID))

cat("Total Number Subjects: ", nrow(main.data))

# See how many by SEX variable
table(main.data$SEX)

# Subset data to population of interest
sub.data <- main.data %>%
  filter(SEX == "Female")
cat("Subjects women: ", nrow(sub.data))

table(main.data$SIPRGEV)
# no of women with at least 1  birth -
table(main.data$SIBIRTH >0)


sub.data <- sub.data %>%
  filter(SIBIRTH > 0)
cat("Subjects women, births> 0: ", nrow(sub.data))

#see how many prior DM status ("ever told you had a high sugar or diabetes")
table(sub.data$SODIAB)

sub.data <- sub.data %>%
  filter(SODIAB == "Only during pregnancy")

cat("Subjects women,  births>0, DM during pregnancy: ", nrow(sub.data))

#  see ASSIGN among women births>0, DMduring pregnancy
table(sub.data$ASSIGN)

sub.data <- sub.data %>%
  filter(ASSIGN != "Troglitazone")

#drop troglitazone from factor ASSIGN 
sub.data$ASSIGN <- droplevels(sub.data$ASSIGN, "Troglitazone")
cat("Subjects women, DM during pregnancy, births>0, not on Troglitazone: ", nrow(sub.data))



#######################################
# Save the final subset of data (sub.data) with datetime stamp
#######################################

filename <- paste0(format(Sys.time(), '%Y%m%d_%H%M%S_'), 'dpp_gdm_data.Rda')
saveRDS(sub.data, file = filename)

