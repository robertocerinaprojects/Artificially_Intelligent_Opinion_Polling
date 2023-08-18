# clean workspace
rm(list=ls())
# clean garbadge
gc()
# clear graphics device
# dev.off()
# set decimals to digits instead of scientific
options(scipen=999)
# set timeout limit (laxed)
options(timeout=10000)
# set work directory
setwd(dir = "~/Desktop/US 2020 Mechanical Pollster Test/")
# utils
library(data.table)
library(dplyr)
library(tidycensus)

census_api_key("f9029d49c8e9a35409acaeb3877987714f49c26c")

# get variables available for latest acs
pums_variables <- as.data.table(pums_variables)

# select identifiers for relevant variables
vars <-
  c(
    "CIT",
    "ACCESS",
    "ST",
    "REGION",
    "SEX",
    "RAC1P",
    "HISP",
    "AGEP",
    "HINCP",
    "MAR",
    "SCHL"
  )

# use most current 5-years aggregated acs values at the time of estimation 
most_current_vars_test <-
  (
    pums_variables$year == 2019 &
    pums_variables$survey == "acs5" &
    pums_variables$var_code %in% vars
  )

# load state abbreviations
state_abbreviations <- 
  fread(file = 'data_auxiliary/States_Meta/state_abbreviations.csv')

# download data
microdata <- data.table()
j

for(s in state_abbreviations$abbreviation) {
  
microdata_temp <-
  get_pums(
      variables = vars,
      survey = unique(pums_variables$survey[most_current_vars_test]),
      year =  as.numeric(unique(pums_variables$year[most_current_vars_test])),
      state = s
    )

microdata_temp  <- as.data.table(microdata_temp)[,..vars]

# recode
for(i in names(microdata_temp)){
  
  var_test <- pums_variables$var_code == i & most_current_vars_test
  
  if(unique(pums_variables$data_type[var_test]) == "num"){next}
  
  microdata_temp[[i]] <-
    pums_variables$val_label[var_test][
      match(
        microdata_temp[[i]],
        pums_variables$val_max[var_test]
      )]
}

# bind
microdata <- rbindlist(list(microdata,microdata_temp))

# save microdata
save(microdata,file = 'data_generated/SF_microdata.RData',compress = TRUE)

# completed state s
print(
  paste(
  'completed state',s,'.',
  round(100*which(state_abbreviations$abbreviation==s)/length(state_abbreviations$abbreviation),2),
  '% completed...'
  )
)

}
