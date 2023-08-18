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
library(dplyr)
library(readstata13)

# Sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# # # PART 1: load and stack AMT output data

# load the AMTURK results - these contain `approved' and `submitted' -
# we use only the `approved

# define directory where the turk data is stored
dir <- "data_preferences/AMTurk HIT output/MechTurk_Output_"

# select relevant dates
dates <-
gsub("MechTurk_Output_","",
  gsub(".csv","",
    list.files("data_preferences/AMTurk HIT output/")
  )
)
# drop 1/10/2020 data , this was a test
dates <- dates[!grepl('_',dates)]

# load and stack MT data
MT <- data.table()

for(d in dates){

  MT_temp <-
  fread(
    file = paste(dir,d,'.csv',sep=""),
    integer64 = 'character',
    tz = "",
    fill = TRUE
  )

  MT <- rbindlist(list(MT,MT_temp))

}

# Check Attention Checks: drop only if they fail any attention tests
# if fail first att. test, drop;
if(
  sum(
    !grepl("Azawakh",MT $Answer.colour) &
    !grepl("instruction",MT $Answer.colour) &
    !is.na(MT $Answer.colour)
    )>0
    ){

  MT <-
    MT[
      -which(
        !grepl("Azawakh",MT $Answer.colour) &
        !grepl("instruction",MT $Answer.colour) &
        !is.na(MT $Answer.colour)
        )
      ]
  }

# if fail second att. test, drop;
if(
  sum(
    !grepl("Xanadu",MT $Answer.turkcolour) &
    !grepl("instruction",MT $Answer.turkcolour) &
    !is.na(MT $Answer.turkcolour)
    )>0
    ){
  MT <-
    MT[
      -which(
        !grepl("Azawakh",MT $Answer.turkcolour) &
        !grepl("instruction",MT $Answer.turkcolour) &
        !is.na(MT $Answer.turkcolour)
        )
      ]
}

# Finally drop non-approved ones
MT <- MT[MT$AssignmentStatus=="Approved"]

# save this dataset
save(MT,file = 'data_generated/stacked_AMT_output.RData',compress = TRUE)
