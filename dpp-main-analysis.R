###########################################################
#
# DPP-DPPOS
# Bernice Man's DPP GDM Analysis (Prediction Model)
#
# Started 8/27/19
# Revised 8/28/19
#
# Description:
#
###########################################################

# Libraries
library(sas7bdat)
library(tidyverse)

# Read in selected data files 
# Forms:  S03, S05, Q03
# Nonforms: basedata.sas7bdat, lab.sas7bdat, events.sas7bdat

S03 <- read.sas7bdat("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\Unzipped\\Data\\DPP_Data_2008\\Form_Data\\Data\\s03.sas7bdat")
S05 <- read.sas7bdat("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\Unzipped\\Data\\DPP_Data_2008\\Form_Data\\Data\\s05.sas7bdat")
Q03 <- read.sas7bdat("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\Unzipped\\Data\\DPP_Data_2008\\Form_Data\\Data\\q03.sas7bdat")

basedata <- read.sas7bdat("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\Unzipped\\Data\\DPP_Data_2008\\Non-Form_Data\\Data\\basedata.sas7bdat")
lab <- read.sas7bdat("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\Unzipped\\Data\\DPP_Data_2008\\Non-Form_Data\\Data\\lab.sas7bdat")
events <- read.sas7bdat("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\Unzipped\\Data\\DPP_Data_2008\\Non-Form_Data\\Data\\events.sas7bdat")

activities <- read.csv("C:\\Users\\bgerber\\Box Sync\\DPP-DPPOS\\Reproduced Analysis\\data\\MAQactivities with Updated METs.csv")

# Select data of interest for S03, S05 (specify variables)
# Use "RUN" visit for Q03 physical activity data and lab data
# Determined by Notes/variables DPP 8-27-2019.docx

S03.sel <- S03 %>%
  select(c("RELEASE_ID", "VISIT", "SODIAB", "SOETHN", "SOHGHT1", "SOHGHT2", 
           "SOHGHT3", "SOSBPA", "SODBPA", "SOBMI", "SOLFAST", "SOL2HR", "SOPRTN"))
S05.sel <- S05 %>%
  select(c("RELEASE_ID", "VISIT", "SIBIRTH", "SIMDIAB", "SIFDIAB", "SI100CG", "SISMOK", "SIPCOS", "SIWSTC1",
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
  left_join(S05.sel, by = "RELEASE_ID")

# Check number of distinct IDs
length(unique(Q03$RELEASE_ID))

# Inspect one of the duplicates that has two rows (example)
Q03 %>%
  filter(RELEASE_ID == "10033391") %>%
  select(c("VISIT", "KVSTTYP", "KVSTWK", ends_with("CODE"), "DAYSRAND"))

# Q03 has duplicate IDs, evaluate before merging
Q03.sel %>%
  filter(duplicated(RELEASE_ID)) %>%
  select(RELEASE_ID)

# Show the top 10 rows with very large number of days to randomization
head(sort(Q03$DAYSRAND[Q03$VISIT == "RUN"]), 10)

# Since there are 2 rows for ~6 subjects' physical activity at run-in, 
# keep data with least number of days to randomization (least negative number)
Q03.sel.uniq <- Q03.sel %>%
  mutate(RELEASE_ID = as.character(RELEASE_ID)) %>%
  group_by(RELEASE_ID) %>%
  slice(which.max(DAYSRAND)) %>%
  ungroup()

# Convert activities to MET-hour/week

# Show which rows have 'write-in' activities (these are currently ignored in lookup table activities)
Q03.sel.uniq %>%
  select(ends_with("CODE")) %>%
  filter_all(any_vars(. == 38))

# Calculate months, times/month, hours/time
for (x in toupper(letters[1:12])) {
  firstcol <- which(colnames(Q03.sel.uniq) == paste0("KA", x, "JAN"))
  lastcol <- which(colnames(Q03.sel.uniq) == paste0("KA", x, "DEC"))
  actcol <- which(colnames(Q03.sel.uniq) == paste0("KA", x, "CODE"))
  actvec <- unlist(Q03.sel.uniq[, actcol])
  Q03.sel.uniq[[paste0("KAct", x)]] = activities[actvec, "METs"]
  Q03.sel.uniq <- Q03.sel.uniq %>%
    mutate(!!paste0("KMonth", x) := rowSums(.[c(firstcol:lastcol)], na.rm = T)) 
}

# Check conversions for activities
Q03.sel.uniq %>%
  select("KABCODE", "KActB") %>%
  head(n=10)

# Find unmatched records between Q03.sel.uniq and main data
anti_join(main.data, Q03.sel.uniq, by = "RELEASE_ID")

# Merge Q03 with main data
main.data <- main.data %>%
  left_join(Q03.sel.uniq, by = "RELEASE_ID")




