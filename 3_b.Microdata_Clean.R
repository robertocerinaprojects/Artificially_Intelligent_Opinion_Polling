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
setwd(dir = "~/Desktop/Artificially Intelligent Opinion Polling/")
# utils
library(data.table)

# load microdata
load(file = 'data_generated/SF_microdata.RData')

# drop non-citizens
microdata <- microdata[microdata$CIT!="Not a citizen of the U.S."]
microdata <- microdata [,!'CIT']

# clean state
microdata$state_simple <- sub("\\/.*", "", microdata$ST)

# clean gender
microdata$gender <- substr(microdata$SEX,1,1)

# clean ethnicity
microdata$ethnicity <-
    ifelse(microdata$HISP!= 'Not Spanish/Hispanic/Latino','Hispanic and Portuguese',
        ifelse(microdata$RAC1P=='White alone','European',
            ifelse(microdata$RAC1P=='Black or African American alone','Likely African-American',
                ifelse(microdata$RAC1P=="Asian alone",'East and South Asian',
                'Other'
            ) ) ) )

# clean age
microdata$age_bins <-
as.character(unlist(
    cut(
      microdata$AGEP,
      c(-1,17,24,34,44,54,64,max(microdata$AGEP)),
      c("0-17","18-24","25-34","35-44","45-54","55-64","65+")
    ) ) )
# drop under-age people
microdata <- microdata[microdata$age_bins!='0-17']

# not going to use marital status - doesn't seem overwhelmingly relevant to
# vote choice + measurememt problems for surveys & Twitter

# clean marital status
#microdata$marital_status_code <-
#    ifelse(microdata$MAR == 'Married','Married','Not Married')

# clean education
microdata$modeled_college_grad <-
    ifelse(
        microdata$SCHL %in%
        c(
        "Bachelor's degree",
        "Doctorate degree",
        "Master's degree",
        "Professional degree beyond a bachelor's degree"
        ),
        "Modeled College Degree",
        "Modeled No College Degree"
        )

# clean household income

microdata$commercial_estimated_hh_income <-
    cut(
        microdata$HINCP,
        c(min(microdata$HINCP)-1, 24999,49999,74999,99999,max(microdata$HINCP)),
        c("[min, 25000)","[25000, 50000)","[50000, 75000)","[75000, 100000)","[100000, max]")
    )


# create stratification frame
vars <-
    c(
        'state_simple',
        'gender',
        'ethnicity',
        'age_bins',
        'modeled_college_grad',
        'commercial_estimated_hh_income'
    )
microdata$N <- 1
SF <- microdata[, lapply(.SD,sum),by = c(vars),.SDcols = c('N')]

# save clean stratification frame
save(SF,file = 'data_generated/SF.RData',compress = TRUE)
