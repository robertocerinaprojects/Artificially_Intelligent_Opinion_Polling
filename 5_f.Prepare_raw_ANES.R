# clean workspace
rm(list = ls())
# clean garbadge
gc()
# clear graphics device
# dev.off()
# set decimals to digits instead of scientific
options(scipen = 999)
# set timeout limit (laxed)
options(timeout = 10000)
# set work directory
setwd(dir = "~/Desktop/Artificially Intelligent Opinion Polling/")

# load useful package
library(data.table)

# load ANES dataset
ANES_raw <-
    fread(file = 'data_generated/ThreeDatasetsSystem/SURVEY/ANES_Survey.csv')

# load state abbreviations which help for cleaning
state_abbreviations <-
    fread(file = 'data_auxiliary/States_Meta/state_abbreviations.csv')

# assign state_simple
ANES_raw $state_simple <-
    state_abbreviations$state[
        match(
            ANES_raw $state_simple_abbreviation,
            state_abbreviations$abbreviation
            )
        ]

# focus on the relevant parties, and tally up the raw numbers
ANES_raw  <-
  table(
    ANES_raw$state_simple[
        ANES_raw$vote2020 %in% c('D','R','L','G','stay home')
        ],
    ANES_raw$vote2020[
        ANES_raw$vote2020 %in% c('D','R','L','G','stay home')
        ]
    )

# get shares
ANES_raw <- ANES_raw / rowSums(ANES_raw )

# calculate turnout share from stay_home
ANES_raw <- cbind(ANES_raw ,turnout = 1-ANES_raw[,'stay home'])
# drop stay_hoe
ANES_raw <- ANES_raw[,-which(colnames(ANES_raw )=='stay home')]

# make into a data.table
ANES_raw <-
    as.data.table(cbind(state_simple = rownames(ANES_raw),as.matrix(ANES_raw)))

# make numeric
for(i in c('D','R','L','G','turnout')){
    ANES_raw[[i]] <- as.numeric(ANES_raw[[i]])
}

# save results data
save(
    ANES_raw ,
    file = 'data_generated/raw_ANES2020_voteshare.RData',
    compress = TRUE
    )

