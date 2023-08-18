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

# # # PART 4: get traditional survey data from Roper centre, and clean to make
# into the same format.

# Thinking about the upcoming presidential election, do you think you are
# definitely going to vote for Donald Trump, probably going to vote for Donald
# Trump, probably going to vote for Joe Biden, or are you definitely going to
# vote for Joe Biden?

# The Kaiser survey vote question is a bit weird, but we needed a national
# sample including individuals from all states.
# after removing third party voters from other states,
# and predicting turnout, these should be excangeable with the other
# information from other polls.

# 4 - alpha: ANES
dir <- "data_preferences/ANES 2020 Time Series/"
anes <-
    fread(
      file =
        paste(
          dir,
          'anes_timeseries_2020_csv_20220210.csv',
          sep = ""
        )
      )

# drop unreigistered voters (not in our frame)
anes <- anes[anes$V201008!=3]

# days to election
anes$dte <-
  as.numeric(
    unlist(
      difftime(
        as.Date("03/11/2020","%d/%m/%Y"),
        as.Date(
          anes$V203055,
          format = "%Y%m%d"
          ),
          units = 'days'
        )
      )
    )

# vote 2016
anes$vote2016 <-
as.factor(
  ifelse(
    anes$V201102==2|anes$V201101==2,'Stayed Home',
    ifelse(
      anes$V201103==1,'Clinton',
      ifelse(
        anes$V201103==2,'Trump',
        ifelse(
          anes$V201103==5,'Other',NA
        )
      )
    )
  )
)

# vote 2020

anes$vote2020 <-
as.factor(
  ifelse(
    anes$V201032==2|
    anes$V201100 %in% c(3:5),
    # is not very likely at least, consider stays home
    'stay home',
    ifelse(
      anes$V201033==1,'D',
      ifelse(
        anes$V201033==2,'R',
        ifelse(
          anes$V201033==3,'L',
            ifelse(
            anes$V201033==4,'G',
              ifelse(
              anes$V201033==5,'Other',NA
              )
            )
        )
      )
    )
  )
)


# party code
party_levels <-
  data.table(
    code =
      c( -8,-9,-4,0,1,2,3,5),
    level =
      c(
        NA,NA,NA,
        'Nonpartisan or Other',
        'Democrat',
        'Republican',
        'Nonpartisan or Other',
        'Nonpartisan or Other'
      )
  )
anes$party_code <-
  as.factor(
    party_levels$level[match(anes$V201228,party_levels$code)]
  )

# education
anes$modeled_college_grad <-
  as.factor(
    ifelse(
      anes$V201510 %in% c(-9,-8),NA,
      ifelse(
        anes$V201510 %in% c(6:8),
        'Modeled College Degree',
        'Modeled No College Degree'
      )
    )
  )

# marital status
anes$marital_status_code <-
as.factor(
  ifelse(
    anes$V201508 %in% c(-9,-5),NA,
    ifelse(
      anes$V201508 %in% c(1,2),'Married','Not Married'
    ) )
)

# gender
anes$gender <-
  as.factor(ifelse(anes$V201600==1,'M',ifelse(anes$V201600==2,'F',NA)))

# ethnicity
ethnicity_levels <-
  data.table(
    code =
      c( -5,-9,1:6),
    level =
      c(
        NA,NA,
        'European',
        'Likely African-American',
        'Hispanic and Portuguese',
        'East and South Asian',
        rep('Other',2)
      )
  )
anes$ethnicity <-
  as.factor(
    ethnicity_levels$level[match(anes$V201549x,ethnicity_levels$code)]
  )

# age
anes$age_bins <-
as.factor(
  ifelse(
    anes$V201507x<0,NA,
    as.character(unlist(
    cut(
      anes$V201507x,
      c(-1,17,24,34,44,54,64,max(anes$V201507x)),
      c("0-17","18-24","25-34","35-44","45-54","55-64","65+")
    ) ) ) )
)

# income
income_levels <-
  data.table(
    code =
      c( -5,-9,1:22 ),
    level =
      c(
        NA,NA,
        rep('[min, 25000)',4),
        rep('[25000, 50000)',5),
        rep('[50000, 75000)',4),
        rep('[75000, 100000)',3),
        rep('[100000, max]',6)
      )
  )

anes$commercial_estimated_hh_income <-
  as.factor(
    income_levels$level[match(anes$V202468x,income_levels$code)]
  )

# state
state_levels <-
  data.table(
    code =
      c(
        -1,1:2,4:6,8:13,15:42,44:51,53:56),
    level =
      c(
        NA,
        'Alabama',
        'Alaska',
        'Arizona',
        'Arkansas',
        'California',
        'Colorado',
        'Connecticut',
        'Delaware',
        'District of Columbia',
        'Florida',
        'Georgia',
        'Hawaii',
        'Idaho',
        'Illinois',
        'Indiana',
        'Iowa',
        'Kansas',
        'Kentucky',
        'Louisiana',
        'Maine',
        'Maryland',
        'Massachusetts',
        'Michigan',
        'Minnesota',
        'Mississippi',
        'Missouri',
        'Montana',
        'Nebraska',
        'Nevada',
        'New Hampshire',
        'New Jersey',
        'New Mexico',
        'New York',
        'North Carolina',
        'North Dakota',
        'Ohio',
        'Oklahoma',
        'Oregon',
        'Pennsylvania',
        'Rhode Island',
        'South Carolina',
        'South Dakota',
        'Tennessee',
        'Texas',
        'Utah',
        'Vermont',
        'Virginia',
        'Washington',
        'West Virginia',
        'Wisconsin',
        'Wyoming'
      )
  )

anes$state <-
  as.factor(
    state_levels$level[match(anes$V201014b,state_levels$code)]
  )


state_abbreviations <-
  fread(file = 'data_auxiliary/States_Meta/state_abbreviations.csv')

levels(anes$state) <-
  state_abbreviations$abbreviation[
    match(levels(anes$state),state_abbreviations$state)
  ]

# source
anes$source <- 'ANES'

# ID
anes$user_id <- paste('ANES',anes$V200001)

# state
ANES_Survey <-
  anes[,
  c(
    'user_id',
    'dte',
    'source',
    'state',
    'commercial_estimated_hh_income',
    'age_bins',
    'ethnicity',
    'gender',
    'marital_status_code',
    'modeled_college_grad',
    'party_code',
    'vote2016',
    'vote2020'
    ), with = F]

# final cleaning to make uniform with other samples

#levels(ANES_Survey$vote2020 )[
#  which(levels(ANES_Survey$vote2020)=="L")
#  ] = 'Other'

#levels(ANES_Survey$vote2020 )[
#  which(levels(ANES_Survey$vote2020)=="G")
#  ] = 'Other'
levels(ANES_Survey$vote2020 )[
  which(levels(ANES_Survey$vote2020)=="Other")
  ] = 'other'



levels(ANES_Survey$vote2016)[
  levels(ANES_Survey$vote2016)=="Trump"
  ] = 'R'
levels(ANES_Survey$vote2016)[
  levels(ANES_Survey$vote2016)=="Clinton"
  ] = 'D'
levels(ANES_Survey$vote2016)[
  levels(ANES_Survey$vote2016)=="Other"
  ] = 'other'
levels(ANES_Survey$vote2016)[
  levels(ANES_Survey$vote2016)=="Stayed Home"
  ] = 'stay home'


names(ANES_Survey)[which(names(ANES_Survey)=="state")] <-
'state_simple_abbreviation'


# drop missing states
ANES_Survey <-
  ANES_Survey[!is.na(ANES_Survey$state_simple_abbreviation)]

table(ANES_Survey$state,ANES_Survey$vote2020)


# Cleaned and labeled information for AMT workers
fwrite(
  ANES_Survey,
  file = "data_generated/ThreeDatasetsSystem/SURVEY/ANES_Survey.csv"
  )
