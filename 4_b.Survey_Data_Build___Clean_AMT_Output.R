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

# Sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# # # PART 2: rearrange data into survey format

# load stacked data
load(file = 'data_generated/stacked_AMT_output.RData')

# # # PART 2-a: build the `Twitter users' survey
Twitter <-
  MT[,
  !grepl("turk",colnames(MT)) & grepl("Answer",colnames(MT)) |
  grepl("created_at_10",colnames(MT)) |
  grepl("user_id",colnames(MT)) |
  grepl("WorkerId",colnames(MT)) ,
  with = FALSE
  ]

# use last tweet date for each user as the relevant trace date
Twitter[,
  grepl("created_at_10",colnames(Twitter)),
  with=FALSE
  ]

# assign source
Twitter$source <- "Twitter"

# drop 'answer' format on var names
colnames(Twitter) <- gsub("Answer.","",colnames(Twitter))

# dorp attention check
Twitter <- Twitter[,!"colour",with=FALSE]

# rename date variable and re-arrange to indicate `days to election`
colnames(Twitter)[grep("created_at_10",colnames(Twitter))] <- "days_to_el"


# 2 different date formats - allow for both
days_to_el <-
  as.numeric(
    as.character(
      unlist(
        difftime(
          as.Date("Nov 03 2020","%B %d %Y"),
          as.Date(substr(Twitter$days_to_el,1,10),
          format = "%d/%m/%Y"),
          units = 'days'
          )
        )
      )
    )

days_to_el <-
  ifelse(
    is.na(days_to_el),
    as.numeric(
      as.character(
        unlist(
          difftime(
            as.Date("Nov 03 2020","%B %d %Y"),
            as.Date(substr(Twitter$days_to_el,1,10),
            format = "%Y-%m-%d"
            ),
            units = 'days'
          )
        )
      )
    ),
    days_to_el
    )

Twitter$days_to_el <- days_to_el

# clean up names format - drop input nomenclature
colnames(Twitter) <- gsub("Input.","",colnames(Twitter))

# rename user id variable to differentiate with ids from other sources
Twitter$user_id_original = Twitter$user_id

# assign source-specific ID
Twitter$user_id <-
  paste(
    as.integer(
      as.factor(
        as.character(
          unlist(
            Twitter$user_id
            )
          )
        )
      ),
      Twitter$source
    )

# assign id for the dedicated worker who performed the inference
Twitter$DedicatedWorker <-
  paste(
    paste(
      as.integer(
        as.factor(
          as.character(
            unlist(
              Twitter$WorkerId
              )
            )
          )
        ),
        "Turks"
        )
      )

# drop then the original worker id var
Twitter = Twitter[,!"WorkerId",with=FALSE]


# # # PART 2-b: build the `Amturk workers' survey

AMT <-
  MT[,grepl("turk",colnames(MT)) & grepl("Answer",colnames(MT)) |
  grepl("SubmitTime",colnames(MT))|
  grepl("WorkerId",colnames(MT)),
  with = FALSE
  ]

# assign source
AMT$source <- "Turks"

# clean up nomenclature
colnames(AMT) <- gsub("turk","",gsub("Answer.","",colnames(AMT)))

# turn date-var to days-to-election
colnames(AMT)[grep("SubmitTime",colnames(AMT))] <- "days_to_el"

AMT$days_to_el <-
  as.numeric(
    as.character(
      unlist(
        difftime(
          as.Date("Nov 03 2020","%B %d %Y"),
          as.Date( paste( substr(AMT$days_to_el,5,10), "2020"),
                  format = "%B %d %Y"
                  ),
                units = 'days'
                )
              )
            )
          )

# drop attention check var
AMT <- AMT[,!"colour",with=FALSE]

# clean up ID name
colnames(AMT) <- gsub("WorkerId","user_id",colnames(AMT))

# again assign original ID and new id to differnetiate between sources
AMT$user_id_original <-
  paste(
    Twitter$days_to_el, # day of the Twitter user post
    as.numeric(
      as.character(
        unlist(
          difftime(
            as.Date("Nov 03 2020","%B %d %Y"),
            as.Date( paste( substr(MT$SubmitTime,5,10), "2020"),
                    format = "%B %d %Y"),
                    units = 'days'
                    )
                  )
                )
              ), # day of submission
              MT$WorkerId
              )

AMT$user_id <-
  paste(
    as.integer(
      as.factor(
        as.character(
          unlist(
            AMT$user_id
            )
          )
        )
      ),
      AMT$source
      )



# # # PART 3: Stack Twitter and AMT `survey` data,
# # # drop unreliable observations and clean answers to match SF

# stack
SurveyVOTE2020 <- rbindlist(list(Twitter,AMT),fill=TRUE)

# rename days to election
colnames(SurveyVOTE2020)[
  which(colnames(SurveyVOTE2020 )=="days_to_el")
  ] <- "dte"

# select variables to match the stratification frame
SurveyVOTE2020 <-
  SurveyVOTE2020[,
    c(
      "dte",
      "user_id",
      #'user_id_original',
      'DedicatedWorker',
      "source",
      "bot",
      "organization",
      "fakenews",
      "vote2020",
      "stateMENE",
      "state",
      "turnout2020",
      "gender",
      "ethnicity",
      "commercial_estimated_hh_income",
      # "turned_out_2016",
      "turned_out_2018",
      "party_code",
      "marital_status_code",
      "age_bins",
      "modeled_college_grad",
      "vote2016"
      ),
      with = FALSE]

# if mismatch between stateMENE and State, drop
SurveyVOTE2020 <-
  SurveyVOTE2020[
      -which(
        SurveyVOTE2020$stateMENE!="NotMENE" &
        SurveyVOTE2020$stateMENE!="DontKnow" &
        SurveyVOTE2020$state!="NE" &
        SurveyVOTE2020$state!="ME"
        ),]

# if `organization`, drop;
if(sum(SurveyVOTE2020$organization=="Yes",na.rm=TRUE)>0){
  SurveyVOTE2020 <-
  SurveyVOTE2020[-which(SurveyVOTE2020$organization=="Yes"),]
  }

# if `bot` with hjigh certainty, drop;
if(sum(SurveyVOTE2020$bot=="1",na.rm=TRUE)>0){
  SurveyVOTE2020 <-
  SurveyVOTE2020[-which(SurveyVOTE2020$bot=="1"),]
  }

# if Outside the US, drop;
if(sum(SurveyVOTE2020$state=="Outside the United States")>0){
  SurveyVOTE2020 <-
  SurveyVOTE2020[-which(SurveyVOTE2020$state=="Outside the United States"),]
  }

# if under 18, drop;
if(sum(SurveyVOTE2020$age_bins=="0-17")>0){
  SurveyVOTE2020 <- SurveyVOTE2020[-which(SurveyVOTE2020$age_bins=="0-17"),]
  }

# if don't know or prefer not so say, make NA
SurveyVOTE2020$age_bins <-
  as.factor(
    as.character(
      unlist(
        SurveyVOTE2020$age_bins
        )
      )
    )

levels(SurveyVOTE2020$age_bins)[
  which(levels(SurveyVOTE2020$age_bins) %in% c("DontKnow","PreferNotSay"))
  ] <- NA

# sample only one trace per day per worker - this makes the analysis simpler
# (some AMT workers answered like 30 times on the same day)

daily_dubs <-
  names(
    which(
      table(
        paste(SurveyVOTE2020$dte,SurveyVOTE2020$user_id)
        )>1
      )
    )

for(i in daily_dubs){

  temp <- which(paste(SurveyVOTE2020$dte,SurveyVOTE2020$user_id)==i)

  temp_star <- sample(temp,length(temp)-1)

  SurveyVOTE2020 <- SurveyVOTE2020[-temp_star]

}


# create function to sub levels for cleaning from here on
sub_levels <-
  function(x,condition,sub){
    levels(x)[which(levels(x) %in% condition)] <- sub
    return(x)
  }

# clean state variable
SurveyVOTE2020$state <-
  as.factor(as.character(unlist(SurveyVOTE2020$state)))

SurveyVOTE2020$state <-
  sub_levels(
    SurveyVOTE2020$state,
    c("DontKnow","PreferNotSay"),
    NA
    )

# if we have a missing state, we don't need this record
SurveyVOTE2020 <- SurveyVOTE2020[!is.na(SurveyVOTE2020$state)]

# clean income variable
SurveyVOTE2020$commercial_estimated_hh_income <-
  as.factor(as.character(unlist(SurveyVOTE2020$commercial_estimated_hh_income)))

SurveyVOTE2020$commercial_estimated_hh_income <-
  sub_levels(
    SurveyVOTE2020$commercial_estimated_hh_income,
    c("DontKnow","PreferNotSay"),
    NA)
SurveyVOTE2020$commercial_estimated_hh_income <-
  sub_levels(
    SurveyVOTE2020$commercial_estimated_hh_income,
    "25000",
    "[min, 25000)"
    )
SurveyVOTE2020$commercial_estimated_hh_income <-
  sub_levels(
    SurveyVOTE2020$commercial_estimated_hh_income,
    "50000",
    "[25000, 50000)"
    )
SurveyVOTE2020$commercial_estimated_hh_income <-
  sub_levels(
    SurveyVOTE2020$commercial_estimated_hh_income,
    "75000",
    "[50000, 75000)"
    )
SurveyVOTE2020$commercial_estimated_hh_income <-
  sub_levels(
    SurveyVOTE2020$commercial_estimated_hh_income,
    "100000",
    "[75000, 100000)"
    )
SurveyVOTE2020$commercial_estimated_hh_income <-
  sub_levels(
    SurveyVOTE2020$commercial_estimated_hh_income,
    "over",
    "[100000, max]"
    )

# clean ethnicity variable
SurveyVOTE2020$ethnicity <-
  as.factor(as.character(unlist(SurveyVOTE2020$ethnicity)))

levels(SurveyVOTE2020$ethnicity) <-
  gsub("\\|","",levels(SurveyVOTE2020$ethnicity))

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    c("DontKnow","PreferNotSay"),
    NA)


SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherAmerican",
    NA
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherAsia",
    "East and South Asian"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherbiracial blackj/white",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherAsian",
    "East and South Asian"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherCaucasian",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermultiracial",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherWhite",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othercaucasian",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherJewish American",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherWhite / Hispanic",
    "Hispanic and Portuguese"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherCaucasion",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermixed",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherwhite",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherAfrican-American Latina",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherBlack Caribbean",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherMixed",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherNative American",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherwhite/european",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "European0",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherBlack",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherMixed - Black/White",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherPacific Islander",
    "East and South Asian"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherwhite American",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherWhite American",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherWhite/Caucasian",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherNon Hispanic -White",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othersoutheast asian, white, pacific islander",
    "East and South Asian"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "EuropeanWhite & Hispanic",
    "Hispanic and Portuguese"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherAFRICAN,AFRICA",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherasian/caucasion",
    "East and South Asian"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherBiracial Afro Caribbean American / White European American",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherIndigenous",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherLatina",
    "Hispanic and Portuguese"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermiddle eastern",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherMulti",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherWest Asian",
    "East and South Asian"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "southeast asianEast and South Asian",
    "East and South Asian"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherasian european",
    "East and South Asian"
    )
SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherbiracial black/white",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherblack and white",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherblack hispanic",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherblack/white",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherChild of God",
    NA
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherJapanese",
    "East and South Asian"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherMiddle East",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherMiddle Eastern",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermixed jewish and european",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermulti-ethnic",
    "Other")

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherNative american caucasian",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherNon Hispanic-White",
    "Hispanic and Portuguese"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherSalvadoran/indian/iranian",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherwhite american",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherarabic",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherBlack, white, Lakota Sioux",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherCaucasian - white",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherCaucasian American",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherJamaican",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermulti",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherAFRICAN, AFRICA",
    "Likely African-American"
    )


SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermultiracial: Asian and European",
    "East and South Asian"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherWhite - Caucasian",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherwhite and black",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherWhite-Asian Biracial",
    "East and South Asian"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherWhite/Pacific Islander",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "WhiteEuropean",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Hispanic and PortugueseWhite European & Hispanic",
    "Hispanic and Portuguese"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otheramerican",
    NA
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherCaribbean",
    "Likely African-American"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherCaucasian/White",
    "European"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othercaucasion",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherJewish",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherLatino",
    "Hispanic and Portuguese"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherMENA",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermixed - black/white",
    "Likely African-American"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherMulti Racial",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othernative american",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othernative american/caucasian",
    "Other"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherWHITE",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherWhite (he wouldn't like to be called European)",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherWhite European / Middle Eastern",
    "European"
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherHuman",
    NA
    )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherJewish-American",
    "European"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherMixed - black/white",
    "Likely African-American"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermixed race",
    "Other"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherwhite and native american",
    "Other"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherAfrican American",
    "Likely African-American"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "EuropeanWhite",
    "European"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherAfrican American",
    "Likely African-American"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherItalian American",
    "European"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermixed black and white",
    "Likely African-American"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermixed race white and black",
    "Likely African-American"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherMixed white and native american",
    "Other"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherNative America",
    "Other"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherNON HISPANIC",
    NA)
SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherOther",
    "Other"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherScandinavian",
    "European"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherWhite-Latino",
    "Hispanic and Portuguese"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherAmerican/ White",
    "European"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherAmerican/White",
    "European"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherasian/white",
    "East and South Asian"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherasian/white/pacific islander",
    "East and South Asian"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherbiracial",
    "Other"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherEuropean American",
    "European"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherEuropean and Hispanic",
    "Hispanic and Portuguese"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "OtherIndian",
    "East and South Asian"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermiddle east",
    "Other"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Othermiddle-eastern",
    "Other"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otherwhite/american",
    "European"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "whiteEuropean",
    "European"
  )

SurveyVOTE2020$ethnicity <-
  sub_levels(
    SurveyVOTE2020$ethnicity,
    "Otheraverage white american",
    "European"
  )

# clean gender variable
SurveyVOTE2020$gender <-
  as.factor(as.character(unlist(SurveyVOTE2020$gender)))

levels(SurveyVOTE2020$gender) <-
  gsub("\\|","",levels(SurveyVOTE2020$gender))

SurveyVOTE2020$gender <-
  sub_levels(SurveyVOTE2020$gender,c("DontKnow","PreferNotSay"),NA)

SurveyVOTE2020$gender <- as.character(unlist(SurveyVOTE2020$gender))

SurveyVOTE2020$gender[
  SurveyVOTE2020$gender!="M" &
  SurveyVOTE2020$gender!="F" &
  !is.na(SurveyVOTE2020$gender)
  ] <- NA

SurveyVOTE2020$gender <- as.factor(as.character(unlist(SurveyVOTE2020$gender)))

# clean marital status
SurveyVOTE2020$marital_status_code <-
  as.factor(as.character(unlist(SurveyVOTE2020$marital_status_code)))

levels(SurveyVOTE2020$marital_status_code) <-
  gsub("\\|","",levels(SurveyVOTE2020$marital_status_code) )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    c("DontKnow","PreferNotSay"),
    NA
    )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherdivorced",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherDivorced",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherEngaged",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Othersignificant other",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherwidowed",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherWidowed",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherSeperated",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherseparated",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherlive with long term partner",
    "Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherpartnered",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherPartner",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherSeparated",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherwidow",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherwidower",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherCohabiting",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Othercohabitating",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherdomestic partnership",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherDomestic Partnership",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherPartnered and Cohabitating",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherDivorced, Married Once",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Othercommitted relationship",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherdivirced",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherLive with girlfriend",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherLiving together as married, soon to be married",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherWidower",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Not MarriedWidowed",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherCivil partnership",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherDiorced",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherI kind of think this is a bot",
    NA
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherliving together-in relationship",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherLiving with partner",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherseperated",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Other",
    NA
    )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherdomestic partner",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherengaged",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherlive with partner",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherLiving together",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherCohabitating",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherDivorce ld",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherDomestic Partner",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherHe's been stringing along the same girlfriend for years and has cheated on her",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherLiving toigether",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherLiving with",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Otherliving with partner",
    "Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherPartnereed",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherWidow",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Othercivil union",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherDomestic partnership",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherPartnered",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "OtherLiving with partner engaged",
    "Not Married"
  )

SurveyVOTE2020$marital_status_code <-
  sub_levels(
    SurveyVOTE2020$marital_status_code,
    "Othercohabiting",
    "Not Married"
  )


# clean college grad
SurveyVOTE2020$modeled_college_grad <-
  as.factor(as.character(unlist(SurveyVOTE2020$modeled_college_grad )))

levels(SurveyVOTE2020$modeled_college_grad) <-
  gsub("\\|","",levels(SurveyVOTE2020$modeled_college_grad) )

SurveyVOTE2020$modeled_college_grad <-
  sub_levels(
    SurveyVOTE2020$modeled_college_grad,
    c("DontKnow","PreferNotSay"),
    NA
  )

SurveyVOTE2020$modeled_college_grad <-
  sub_levels(
    SurveyVOTE2020$modeled_college_grad,
    "ModeledCollegeDegree",
    "Modeled College Degree"
    )

SurveyVOTE2020$modeled_college_grad <-
  sub_levels(
    SurveyVOTE2020$modeled_college_grad,
    "ModeledNoCollegeDegree",
    "Modeled No College Degree"
  )


# clean party code
SurveyVOTE2020$party_code <-
  as.factor(as.character(unlist(SurveyVOTE2020$party_code )))

levels(SurveyVOTE2020$party_code) <-
  gsub("\\|","",levels(SurveyVOTE2020$party_code) )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    c("DontKnow","PreferNotSay"),
    NA
  )


SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Independent",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherConstitution Party",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Othergreen",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherGreen",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherLibertarian",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Otherdon't know",
    NA
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherNeither major party",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherTea Party",
    "Republican"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherUnaffiliated",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherSocialist",
    "Democrat"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherDemocratic Socialist",
    "Democrat"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Othergreen party",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Othernone",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherGreen Party",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherNo affiliation",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherSocial anarchist",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherDemocratic Socialists of America",
    "Democrat"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Othersocialist",
     "Democrat"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Othercommunist",
     "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherGreen party",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherSocliast",
    "Democrat"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherI kind of think this is a Russian Bot. I've run into these things that are inconsistent in their posts. It's more helpful if I can see the number of people following the though. Usually, it's very few to none,but they are following many.",
    NA
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherIndependent",
     "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Otherlibertarian",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherLibertarian Party",
     "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherNATIONALIST",
     "Republican"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Otherneutral",
     "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherNeutral",
     "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherA",
     "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherAnarchist",
     "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherDSA",
    "Democrat"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherLeft leaning liberal",
    "Democrat"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherLiberatarian",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherLibetarian",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherNo party affiliation",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Otherapolitical",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Othersocialist-leaning",
    "Democrat"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherThe Honest Party - does not exist",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Otheranarchist/socialistt",
    "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherConservative",
     "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "aIndependent",
     "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "Otheranarchist/socialist",
     "Nonpartisan or Other"
  )

SurveyVOTE2020$party_code <-
  sub_levels(
    SurveyVOTE2020$party_code,
    "OtherSocialist Equality Party",
     "Nonpartisan or Other"
  )

# clean turnout 2018 var.
SurveyVOTE2020$turned_out_2018 <-
  as.factor(as.character(unlist(SurveyVOTE2020$turned_out_2018)))

levels(SurveyVOTE2020$turned_out_2018) <-
  gsub("\\|","",levels(SurveyVOTE2020$turned_out_2018) )

SurveyVOTE2020$turned_out_2018 <-
  sub_levels(
    SurveyVOTE2020$turned_out_2018,
    c("DontKnow","PreferNotSay"),
    NA
  )

SurveyVOTE2020$turned_out_2018 <-
  ifelse(SurveyVOTE2020$turned_out_2018=="Yes",1,0)

# clean vote 2016 var.
# using only R,D and other

SurveyVOTE2020$vote2016 <-
  as.factor(as.character(unlist(SurveyVOTE2020$vote2016)))

levels(SurveyVOTE2020$vote2016) <-
  gsub("\\|","",levels(SurveyVOTE2020$vote2016) )


SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,
    c("DontKnow","PreferNotSay"),
    NA
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"DonaldTrumpRepublican","Trump"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"HillaryClintonDemocrat","Clinton"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"LikelyDidNotVote","Stayed Home"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Other","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherConstitution Party","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherDarrell Castle","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherGary Johnson","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Othergreen party","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherJill Stein","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherJohn Kasich","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherJohnson","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherBernie Sanders","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Otherbernue","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Othergreen","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Otherindependent","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherLibertarian Party","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherSanders","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherWrote in a local politican","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherAbstained","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherGloria La Riva","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Otherjohnson","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherLibertarian","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherAmerican Solidarity Party","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Othercant recall. third party","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherEvan McCullin","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherI wrote in Bernie Sanders","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Othergary johnson","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"HillaryClintonDemocratthisname","Clinton"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherIndependent or possibly Democrat","Clinton"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherJ Stein","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherJohnson libertarian","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherBernie Sanders/Write-in","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Otherbloomberg","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherGary Johnson - Libretarian","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherGary Johnson Libertarian","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherGary Johnson/Libertarian","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherBernie Sanders Write-in","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherGarry Johnson","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Othergloria la riva","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherGreen Party Candidate","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherIndependent","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Otherjill stein","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherLibertarian Gary Johnson","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Othercaucused for Bernie","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Otherbernie sanders","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherBernie sanders","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherBernie Sanders (write-in vote)","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherBernie Sanders Write-In","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherDarrel Castle","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherEvan McMullin","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherEvan McMillan","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Othergary Johnson","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Othergary johnson libertarian","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherGreen party","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherJill Stein - Green Party","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherJill Stein/Green Party","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherJohn McCain","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherMichelle Obama","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherStein","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherBernie Saunders","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherGary Johnson, Libertarian","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherGreen Party candidate","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherJIll Stein","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherWrite in","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Otherbernie","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherBernie","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherDolly Parton",NA
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherModerate",NA
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherRocky","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Otherstein","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherGary Johnson - Libertarian","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherGreen Party","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherJoe Biden write-in","Clinton"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Otherliberterian","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherLibetarian","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherMcMullin","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"Otherstein","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,"OtherMullin","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherTed Cruz" ,"Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherGary johnson","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherI left it blank." ,"Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherJill Stein/ Green Party" ,"Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherJohnson - Green Party" ,"Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherKasich","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "Otherlibertarian" ,"Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherMaybe Jill Stein?" ,"Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "Otherwrote in Bernie Sanders" ,"Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "Other3rd party","Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherBiden" ,"Clinton"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherNot sure who though" ,NA
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016,
    "OtherPossibly voted Clinton, leaning more toward a third party candidate" ,
    "Clinton"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "Otherchip" ,NA
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "Otherdemocrat" ,"Clinton"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherDr. Ben Carson" , "Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherJohnson - Libertarian" ,"Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherSocialist Equality Party" ,"Other"
  )

SurveyVOTE2020$vote2016 <-
  sub_levels(
    SurveyVOTE2020$vote2016, "OtherThird Party" ,"Other"
  )

# clean source
SurveyVOTE2020$source <- as.factor(SurveyVOTE2020$source)

# Clean 2020 turnout
SurveyVOTE2020$turnout2020 <-
  as.factor(as.character(unlist(SurveyVOTE2020$turnout2020)))

levels(SurveyVOTE2020$turnout2020) <-
  gsub("\\|","",levels(SurveyVOTE2020$turnout2020) )

SurveyVOTE2020$turnout2020 <-
  sub_levels(
    SurveyVOTE2020$turnout2020,
    c("DontKnow","PreferNotSay"),
    NA
  )

SurveyVOTE2020$turnout2020 <-
  sub_levels(
    SurveyVOTE2020$turnout2020,
    "NotEligible",
    "0"
  )

SurveyVOTE2020$turnout2020 <-
  as.numeric(as.character(unlist(SurveyVOTE2020$turnout2020)))

# Clean 2020 VOTE CHOICE
SurveyVOTE2020$vote2020 <-
  as.factor(as.character(unlist(SurveyVOTE2020$vote2020)))

levels(SurveyVOTE2020$vote2020) <-
  gsub("\\|","",levels(SurveyVOTE2020$vote2020) )


SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    c("DontKnow","PreferNotSay"),
    NA
  )

SurveyVOTE2020$vote2020 <-
  as.character(unlist(SurveyVOTE2020$vote2020))

SurveyVOTE2020$vote2020[which(SurveyVOTE2020$turnout2020<0.75)] = "StayHome"

SurveyVOTE2020$vote2020 <-
  as.factor(as.character(SurveyVOTE2020$vote2020))


SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "NotEligible","StayHome"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "DonaldTrumpRepublican","R"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "JoeBidenDemocrat","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "StayHome","stay home"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherAndrew Yang","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherGreen","Green"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherGreen Party","Green"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherSanders","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherHowie Hawkins","Green"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJo Jorgensen","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJo Jorgensen Libertarian Party","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJoe Biden","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Othernot sure, but they wouldn't vote for Biden or Trump",NA
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "JoeBidenDemocratthisname","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherDonald trump","R"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Othergreen party","Green"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJo Jorgenson","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "thisnameJoeBidenDemocrat","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJo Jorgensen/Libertarian","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Othergloria la riva","Party for Socialism and Liberation"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherDemocrat","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherLibertarian","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherAnything anti establishment but since hes antifa he hates trump","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherBernie","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherDon Blankenship","Other"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherHowie Hawkins - Green Party","Green"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJill Stein","Green"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherBernie Sanders","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherBiden","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherGloria La Riva, Party of Socialism and Liberation",
    "Party for Socialism and Liberation"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherBrian T. Carroll","American Solidarity Party"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherHowie Hawkins - Green","Green"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJo Jo","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJo Jogersen","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJorgenson","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherBernie write in","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherIndependent","Other"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Otherjo jorgensen","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJorgensen- Libertarian","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherLa Rivieria","Party for Socialism and Liberation"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Otherlibertarian","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherMark Charles","Other"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherSocialist party","Green"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJoe biden","Green"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJorgensen","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Other3rd party",NA
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherAnyone/auto pilot",NA
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherGloria La Riva","Party for Socialism and Liberation"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Otherjo jergonson","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJo Jorgenson Libertarian","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJo Johnson","Other"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherKanye","Birthday Party"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Other50 percent between Biden and an Independent 3rd party","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherDr Fauci","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJo Jorgensen - Libertarian","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJo Jurgenson","Libertarian"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherPelosi","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherSocialist Equality Party","Socialist Equality Party"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherVermin Supreme","R"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Otherbernie","D"
  )

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "PreferNotSayJo Jorgensen","Libertarian"
  )

# remove non-state voters (territories)
SurveyVOTE2020 <- SurveyVOTE2020[SurveyVOTE2020$state!="Guam"]

levels(SurveyVOTE2020$vote2020 )[
  which(levels(SurveyVOTE2020$vote2020)=="American Solidarity Party")
  ] = 'other'

levels(SurveyVOTE2020$vote2020 )[
  which(levels(SurveyVOTE2020$vote2020)=="Party for Socialism and Liberation")
  ] = 'other'

SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Green partyJoeBidenDemocrat","Green"
  )
SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherAn independent","Other"
  )

  SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherGreen Party Candidate","Green"
  )

  SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Otherhawkins","Green"
  )

  SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherHe seems to hate Biden and Trump, so I guess Kanye?","Other"
  )

    SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Otherhowie","Green"
  )


    SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherHowie Hawkins / Green Party","Green"
  )

    SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "Otherindependent candidate","Other"
  )

    SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherJo jorgenson","Libertarian"
  )

    SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherKanye West","Other"
  )

  SurveyVOTE2020$vote2020 <-
  sub_levels(
    SurveyVOTE2020$vote2020,
    "OtherNot sure who","Other"
  )


levels(SurveyVOTE2020$vote2020 )[
  which(levels(SurveyVOTE2020$vote2020)=="Green")
  ] = 'G'
levels(SurveyVOTE2020$vote2020 )[
  which(levels(SurveyVOTE2020$vote2020)=="Libertarian")
  ] = 'L'

levels(SurveyVOTE2020$vote2020 )[
  which(levels(SurveyVOTE2020$vote2020)=="Socialist Equality Party")
  ] = 'other'

levels(SurveyVOTE2020$vote2020 )[
  which(levels(SurveyVOTE2020$vote2020)=="did not vote")
  ] = 'stay home'


levels(SurveyVOTE2020$vote2020 )[
  which(levels(SurveyVOTE2020$vote2020)=="Other")
  ] = 'other'

SurveyVOTE2020 <-
  SurveyVOTE2020[SurveyVOTE2020$age_bins!="0-17"]

levels(SurveyVOTE2020$vote2016)[
  levels(SurveyVOTE2020$vote2016)=="Trump"
  ] = 'R'
levels(SurveyVOTE2020$vote2016)[
  levels(SurveyVOTE2020$vote2016)=="Clinton"
  ] = 'D'
levels(SurveyVOTE2020$vote2016)[
  levels(SurveyVOTE2020$vote2016)=="Other"
  ] = 'other'
levels(SurveyVOTE2020$vote2016)[
  levels(SurveyVOTE2020$vote2016)=="Stayed Home"
  ] = 'stay home'

names(SurveyVOTE2020)[which(names(SurveyVOTE2020)=="state")] <-
'state_simple_abbreviation'

# drop missing states
SurveyVOTE2020 <-
  SurveyVOTE2020[!is.na(SurveyVOTE2020$state_simple_abbreviation)]

# # # Store output

# Map for going from masked user id to original user id for Twitter users
Twitter_Map <-
  data.table(
    Input.user_id = MT$Input.user_id,
    user_id =
      paste(
        as.integer(
          as.factor(
            as.character(
              unlist(MT$Input.user_id)
              )
            )
          ),
        "Twitter"
        )
      )

fwrite(
    Twitter_Map,
    file = "data_generated/ThreeDatasetsSystem/MAP/Twitter_Map.csv"
    )

# Cleaned and labeled information for each twitter user
Twitter_Survey <- SurveyVOTE2020[which(SurveyVOTE2020$source=="Twitter")]

fwrite(
  Twitter_Survey,
  file = "data_generated/ThreeDatasetsSystem/SURVEY/Twitter_Survey_Turks_Labels.csv"
  )

# Twitter users' metadata
Twitter_Meta <- unique(MT[,grep("Input",colnames(MT)),with=FALSE])

fwrite(
  Twitter_Meta,
  file = "data_generated/ThreeDatasetsSystem/META/Twitter_Meta.csv"
  )

# Map for going from masked user id to original user id for AMT workers
Turks_Map <-
  data.table(
    WorkerId = MT$WorkerId,
    user_id =
      paste(
        as.integer(
          as.factor(
            as.character(
              unlist(MT$WorkerId)
              )
            )
          ),
        "Turks"
        )
      )

fwrite(
  Turks_Map,
  file = "data_generated/ThreeDatasetsSystem/MAP/Turks_Map.csv"
  )

tmp = fread("data_generated/ThreeDatasetsSystem/MAP/Turks_Map.csv")

# Cleaned and labeled information for AMT workers
Turks_Survey <- SurveyVOTE2020[which(SurveyVOTE2020$source=="Turks")]

fwrite(
  Turks_Survey,
  file = "data_generated/ThreeDatasetsSystem/SURVEY/Turks_Survey.csv"
  )

# Metadata from the Turk workers
Turk_Meta <- unique(MT[,!grepl("Input",colnames(MT)),with=FALSE])

fwrite(
  Turk_Meta,
  file = "data_generated/ThreeDatasetsSystem/META/Turks_Meta.csv"
  )