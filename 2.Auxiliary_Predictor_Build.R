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
library(zoo)
library(stringr)
library(geosphere)
library(miceRanger)
library(mltools)

# Sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# # # BUILD AUXILIARY AREA-LEVEL PREDICTOR

# Load abbreviations - this could be helpful to bridge different datasets
state_abbreviations <-
fread(file = "data_auxiliary/States_Meta/state_abbreviations.csv")[1:51,]

# # # (1)
# Load national economic index -
# https://projects.fivethirtyeight.com/2020-election-forecast/
nat_econ_index <-
fread(file = "data_auxiliary/f38_Model_Data_and_Predictions/economic_index.csv")

# clean economic index - create a single indicator
nat_econ_index$category_indicator <-
  paste(nat_econ_index $category,nat_econ_index $indicator)

# select relevant variables
nat_econ_index <-
  nat_econ_index [,c("modeldate","current_zscore","category_indicator")]

# reshape to be wide
nat_econ_index <-
  reshape(
    nat_econ_index  ,
    timevar = c("category_indicator"),
    idvar = "modeldate",
    direction = 'wide'
    )

# merge with date to have a date-level index

nat_econ_index $modeldate <- as.Date(nat_econ_index$modeldate,'%m/%d/%Y')

nat_econ_index <-
  merge(
    expand.grid(
      state = state_abbreviations$state,
      modeldate = nat_econ_index $modeldate
      ),
      nat_econ_index ,
      by = 'modeldate',
      all = TRUE
      )

# # # (2)
# Economic data at the state level :
# https://apps.urban.org/features/state-economic-monitor/

# unemployment rate
state_unemp <-
  fread(
    file = 'data_auxiliary/Urban_Institute_economic_indicators_state_data/unemployment_rate_raw.csv',
    header = TRUE
    )
state_unemp <- melt(state_unemp, id.vars = "Geography")

state_unemp <- state_unemp[-which(state_unemp$Geography=="United States"),]

state_unemp[, lag.value:=c(NA, value[-.N]), by="Geography"]

state_unemp$monthly_delta_Unemp <- state_unemp$value-state_unemp$lag.value

state_unemp <- state_unemp[grep("2020",state_unemp$variable),]

state_unemp <- state_unemp [,-which(colnames(state_unemp )=="lag.value"),with=FALSE]

colnames(state_unemp) <- c("state","date","Unemp","monthly_delta_Unemp")

state_unemp$date <- as.Date(state_unemp$date,"%Y-%m-%d")

state_unemp <-
  merge(
    state_unemp,
    expand.grid(
      state = unique(state_unemp$state),
      date = seq.Date(
        from = min(state_unemp$date),
        to = as.Date("03-11-2020","%d-%m-%Y"),
        by = 1)
        ),
        by = c("state","date"),
        all = TRUE
        )

# use last observation carried forward - assumpion here is that unemp only
# becomes meaningful for public opinion once it's published.
state_unemp <- na.locf(state_unemp)

# # # (3)
# house price index - percentage change from the same time last year
state_houseprice <-
  fread(file = 'data_auxiliary/Urban_Institute_economic_indicators_state_data/house_price_index_yoy_percent_change.csv')

state_houseprice <- melt(state_houseprice,id.vars = "Geography")

state_houseprice <-
  state_houseprice[-which(state_houseprice$Geography=="United States"),]

state_houseprice <- state_houseprice[grep("2020",state_houseprice$variable),]

state_houseprice$date <-
  as.Date(
    ifelse(grepl("Q1",state_houseprice$variable),"01-01-2020",
      ifelse(grepl("Q2",state_houseprice$variable),"01-04-2020","01-07-2020")),
      "%d-%m-%Y"
      )

state_houseprice <-
  state_houseprice[,-which(colnames(state_houseprice)=="variable"),with = FALSE]

colnames(state_houseprice) <- c("state","HousePriceIndex","date")

state_houseprice <-
  merge(
    state_houseprice,
    expand.grid(
      state = unique(state_houseprice$state),
      date = seq.Date(
        from = min(state_houseprice$date),
        to = as.Date("03-11-2020","%d-%m-%Y"),
        by = 1
        ) ),
        by = c("state","date"),
        all = TRUE
        )

# use last observation carried forward - assumpion here is that house-price
# index only becomes meaningful for public opinion once it's published.
state_houseprice <- na.locf(state_houseprice)

# # # (4)
# gdp delta - percentage change from the same time last year
state_gdpdelta <-
  fread(file = 'data_auxiliary/Urban_Institute_economic_indicators_state_data/state_gdp_yoy_percent_change.csv')

state_gdpdelta <- melt(state_gdpdelta,id.vars = "Geography")

state_gdpdelta <-
  state_gdpdelta[-which(state_gdpdelta$Geography=="United States"),]

# calculate lag
state_gdpdelta[, lag.value:=c(NA, value[-.N]), by="Geography"]

# calculate delta
state_gdpdelta$quarterly_delta_gdpdelta <-
  state_gdpdelta$value-state_gdpdelta$lag.value

# drop lag
state_gdpdelta <- state_gdpdelta[grep("2020",state_gdpdelta$variable),]

state_gdpdelta <-
  state_gdpdelta [,-which(colnames(state_gdpdelta )=="lag.value"),with=FALSE]

# rename covariates
colnames(state_gdpdelta) <-
  c("state","date","gdpdelta","quarterly_delta_gdpdelta")

# assign dates
state_gdpdelta$date <-
  as.Date(
    ifelse(grepl("Q1",state_gdpdelta$date),"01-01-2020",
      ifelse(grepl("Q2",state_gdpdelta$date),"01-04-2020","01-07-2020")
      ),
      "%d-%m-%Y"
      )
state_gdpdelta$date <- as.Date(state_gdpdelta$date,"%Y-%m-%d")

state_gdpdelta <-
  merge(
    state_gdpdelta,
    expand.grid(
      state = unique(state_gdpdelta$state),
      date = seq.Date(
        from = min(state_gdpdelta$date),
        to = as.Date("03-11-2020","%d-%m-%Y"),
        by = 1
        ) ),
        by = c("state","date"),
        all = TRUE
        )

# use last observation carried forward - assumpion here is that GDP
# number only becomes meaningful for public opinion once it's published.
state_gdpdelta <- na.locf(state_gdpdelta)

# # # (5)
# Load Covid Data - https://github.com/nytimes/covid-19-data
covid_state <- fread(file = 'data_auxiliary/Covid_NYT_Data_States.csv')
covid_state$date <- as.Date(covid_state$date,'%Y-%m-%d')

# # # (6)
# # # # 2020 State Similarity Index:
# https://www.dailykos.com/stories/2020/2/19/1917029/-How-similar-is-each-state-to-every-other-Daily-Kos-Elections-State-Similarity-Index-will-tell-you

similarity_state <-
  fread(file = 'data_auxiliary/Daily_Kos_Data/Daily Kos Elections State Similarity Index - Similarity.csv')

similarity_state <-
  merge(
    similarity_state,
    state_abbreviations,
    by.x = "Geography",
    by.y = "state",
    all.x=TRUE
    )

similarity_state <-
  similarity_state[-which(similarity_state$Geography=="United States"),]

similarity_state$`Median home value (dollars)` <-
  as.numeric(
    as.character(
      unlist(
        gsub("\\,","",
        gsub("\\$","",
        similarity_state$`Median home value (dollars)`
        ) ) ) ) )

similarity_state$`Median household income (dollars)` <-
  as.numeric(
    as.character(
      unlist(
        gsub("\\,","",
        gsub("\\$","",
        similarity_state$`Median household income (dollars)`
        ) ) ) ) )

similarity_state$`Per capita income (dollars)` <-
  as.numeric(
    as.character(
      unlist(
        gsub("\\,","",
        gsub("\\$","",
        similarity_state$`Per capita income (dollars)`
        ) ) ) ) )

# # # (7)
# Load Relgious composition:
# https://www.dailykos.com/stories/2018/1/7/1728838/-The-Daily-Kos-Elections-guide-to-the-nation-s-religious-populations-by-congressional-district

rel_state <-
  fread(file = 'data_auxiliary/Daily_Kos_Data/Religion by CD 2017.csv')

rel_state$state <- substr(rel_state $District,1,2)

rel_state <- rel_state[,!"District"]

# remove catholic double
rel_state <-
  rel_state [,-which(colnames(rel_state)=="Catholic %")[2],with=FALSE]

rel_state <-
  rel_state[,
    lapply(.SD,function(x){
      as.numeric(as.character(unlist(gsub("\\%","",gsub("\\,","",x)))))
      }),
      by = c("state"),
      .SDcols = colnames(rel_state)[-which(colnames(rel_state)=="state")]
      ]

rel_state <-
  rel_state[,
    c(
      "state",
      "Evangelical %",
      "Black Protestant %",
      "Mainline %",
      "Orthodox %",
      "Catholic %",
      "All Other %",
      "Unclaimed %",
      "LDS %"
      ) ]

# take average per state
rel_state <-
  rel_state[,
    lapply(.SD,function(x){mean(x,na.rm=TRUE)}),
    by = "state",
    .SDcols = c(colnames(rel_state)[-1])
    ]

rel_state <- rel_state [-which(rel_state $state=="* "),]

# # # (8)
# US presidential elections - historical: https://electionlab.mit.edu/data
load(file = 'data_auxiliary/MIT_Election_Results/1976-2020-president.RData')

president_historical <- as.data.table(x)

president_historical <-
  president_historical[which(president_historical$year<2020),]

president_historical <-
  president_historical[,
  c("year","state","party_detailed","candidatevotes","totalvotes")
  ]

president_historical$president_voteshare <-
  president_historical$candidatevotes/president_historical$totalvotes

president_historical <-
  president_historical[,
    lapply(.SD,sum),
    by = c("year","state","party_detailed"),
    .SDcols= 'president_voteshare'
    ]

president_historical <-
  reshape(
    president_historical ,
    idvar = c("year","state"),
    timevar = 'party_detailed',
    direction = 'wide'
    )

president_historical$president_voteshare.REPUBLICAN[
    is.na(president_historical$president_voteshare.REPUBLICAN)]=0
president_historical$president_voteshare.OTHER[
    is.na(president_historical$president_voteshare.OTHER)]=0
president_historical$president_voteshare.DEMOCRAT[
    is.na(president_historical$president_voteshare.DEMOCRAT)]=0
president_historical$president_voteshare.LIBERTARIAN[
    is.na(president_historical$president_voteshare.LIBERTARIAN)]=0
president_historical$president_voteshare.GREEN[
    is.na(president_historical$president_voteshare.GREEN)]=0

president_historical$state = str_to_title(tolower(president_historical$state))

president_historical =
  president_historical[,
    c(
      "state",
      "year",
      "president_voteshare.REPUBLICAN",
      "president_voteshare.DEMOCRAT",
      "president_voteshare.LIBERTARIAN",
      "president_voteshare.GREEN"
      ) ]

president_historical$president_voteshare.OTHER =
  1-rowSums(
      president_historical[,
        c(
          "president_voteshare.REPUBLICAN",
          "president_voteshare.DEMOCRAT",
          "president_voteshare.LIBERTARIAN",
          "president_voteshare.GREEN"
          ) ],
          na.rm=TRUE
          )

president_historical <-
  merge(
    president_historical,
    expand.grid(
      state = unique(president_historical$state),
      year = min(president_historical$year):2020),
      by = c('state',"year"),
      all=TRUE
      )

colnames(president_historical) <-
  gsub("president_voteshare.","",colnames(president_historical))

president_historical <- melt(president_historical,id.vars = c("state","year"))

colnames(president_historical )[
    which(colnames(president_historical )=="value")
    ] <- "voteshare"

colnames(president_historical )[
    which(colnames(president_historical )=="variable")
    ] <- "party"

president_historical $type = 'president'


# and turnout too, including 2012
# https://www.electproject.org/election-data/voter-turnout-data


tmp <- fread(input = 'data_auxiliary/Election Project - Turnout Data/2000 November General Election - Turnout Rates.csv')
turnout_2000 <- tmp[V1 %in% state_abbreviations$state,tmp[1] %in% c('State')|tmp[2] %in% c('VEP Highest Office'),with=F]
names(turnout_2000) <- c('state_simple','turnout_highest.office_VEP')
turnout_2000$turnout_highest.office_VEP <- as.numeric(gsub('%','',turnout_2000$turnout_highest.office_VEP))/100
turnout_2000$stay_home <- 1-turnout_2000$turnout_highest.office_VEP
turnout_2000$year <- 2000

tmp <- fread(input = 'data_auxiliary/Election Project - Turnout Data/2002 November General Election - Turnout Rates.csv')
turnout_2002 <- tmp[V1 %in% state_abbreviations$state,tmp[1] %in% c('State')|tmp[2] %in% c('VEP Highest Office'),with=F]
names(turnout_2002) <- c('state_simple','turnout_highest.office_VEP')
turnout_2002$turnout_highest.office_VEP <- as.numeric(gsub('%','',turnout_2002$turnout_highest.office_VEP))/100
turnout_2002$stay_home <- 1-turnout_2002$turnout_highest.office_VEP
turnout_2002$year <- 2002

tmp <- fread(input = 'data_auxiliary/Election Project - Turnout Data/2004 November General Election - Turnout Rates.csv')
turnout_2004 <- tmp[V1 %in% state_abbreviations$state,tmp[1] %in% c('State')|tmp[2] %in% c('VEP Highest Office'),with=F]
names(turnout_2004) <- c('state_simple','turnout_highest.office_VEP')
turnout_2004$turnout_highest.office_VEP <- as.numeric(gsub('%','',turnout_2004$turnout_highest.office_VEP))/100
turnout_2004$stay_home <- 1-turnout_2004$turnout_highest.office_VEP
turnout_2004$year <- 2004

tmp <- fread(input = 'data_auxiliary/Election Project - Turnout Data/2006 November General Election - Turnout Rates.csv')
turnout_2006 <- tmp[V1 %in% state_abbreviations$state,tmp[1] %in% c('State')|tmp[2] %in% c('VEP Highest Office'),with=F]
names(turnout_2006) <- c('state_simple','turnout_highest.office_VEP')
turnout_2006$turnout_highest.office_VEP <- as.numeric(gsub('%','',turnout_2006$turnout_highest.office_VEP))/100
turnout_2006$stay_home <- 1-turnout_2006$turnout_highest.office_VEP
turnout_2006$year <- 2006

tmp <- fread(input = 'data_auxiliary/Election Project - Turnout Data/2008 November General Election - Turnout Rates.csv')
turnout_2008 <- tmp[V1 %in% state_abbreviations$state,tmp[1] %in% c('State')|tmp[2] %in% c('VEP Highest Office'),with=F]
names(turnout_2008) <- c('state_simple','turnout_highest.office_VEP')
turnout_2008$turnout_highest.office_VEP <- as.numeric(gsub('%','',turnout_2008$turnout_highest.office_VEP))/100
turnout_2008$stay_home <- 1-turnout_2008$turnout_highest.office_VEP
turnout_2008$year <- 2008

tmp <- fread(input = 'data_auxiliary/Election Project - Turnout Data/2010 November General Election - Turnout Rates.csv')
turnout_2010 <- tmp[V1 %in% state_abbreviations$state,tmp[1] %in% c('State')|tmp[2] %in% c('VEP Highest Office'),with=F]
names(turnout_2010) <- c('state_simple','turnout_highest.office_VEP')
turnout_2010$turnout_highest.office_VEP <- as.numeric(gsub('%','',turnout_2010$turnout_highest.office_VEP))/100
turnout_2010$stay_home <- 1-turnout_2010$turnout_highest.office_VEP
turnout_2010$year <- 2010

tmp <- fread(input = 'data_auxiliary/Election Project - Turnout Data/2012 November General Election v2.0 - Turnout Rates.csv')
turnout_2012 <- tmp[V1 %in% state_abbreviations$state,tmp[1] %in% c('State')|tmp[2] %in% c('VEP Highest Office'),with=F]
names(turnout_2012) <- c('state_simple','turnout_highest.office_VEP')
turnout_2012$turnout_highest.office_VEP <- as.numeric(gsub('%','',turnout_2012$turnout_highest.office_VEP))/100
turnout_2012$stay_home <- 1-turnout_2012$turnout_highest.office_VEP
turnout_2012$year <- 2012

tmp <- fread(input = 'data_auxiliary/Election Project - Turnout Data/2014 November General Election - Turnout Rates.csv')
turnout_2014 <- tmp[V1 %in% state_abbreviations$state,tmp[1] %in% c('State')|tmp[2] %in% c('VEP Highest Office'),with=F]
names(turnout_2014) <- c('state_simple','turnout_highest.office_VEP')
turnout_2014$turnout_highest.office_VEP <- as.numeric(gsub('%','',turnout_2014$turnout_highest.office_VEP))/100
turnout_2014$stay_home <- 1-turnout_2014$turnout_highest.office_VEP
turnout_2014$year <- 2014

tmp <- fread(input = 'data_auxiliary/Election Project - Turnout Data/2016 November General Election - Turnout Rates.csv')
turnout_2016 <- tmp[V1 %in% state_abbreviations$state,tmp[1] %in% c('State')|tmp[2] %in% c('VEP Highest Office'),with=F]
names(turnout_2016) <- c('state_simple','turnout_highest.office_VEP')
turnout_2016$turnout_highest.office_VEP <- as.numeric(gsub('%','',turnout_2016$turnout_highest.office_VEP))/100
turnout_2016$stay_home <- 1-turnout_2016$turnout_highest.office_VEP
turnout_2016$year <- 2016

tmp <- fread(input = 'data_auxiliary/Election Project - Turnout Data/2018 November General Election - Turnout Rates.csv')
turnout_2018 <- tmp[V1 %in% state_abbreviations$state,tmp[1] %in% c('State')|tmp[2] %in% c('2018 Vote for Highest Office VEP Turnout Rate'),with=F]
names(turnout_2018) <- c('state_simple','turnout_highest.office_VEP')
turnout_2018$turnout_highest.office_VEP <- as.numeric(gsub('%','',turnout_2018$turnout_highest.office_VEP))/100
turnout_2018$stay_home <- 1-turnout_2018$turnout_highest.office_VEP
turnout_2018 <- turnout_2018[complete.cases(turnout_2018)]
turnout_2018$year <- 2018


turnout_dt <-
  rbindlist(
    list(
      turnout_2000,
      turnout_2002,
      turnout_2004,
      turnout_2006,
      turnout_2008,
      turnout_2010,
      turnout_2012,
      turnout_2014,
      turnout_2016,
      turnout_2018
      )
    )

# # # (9)
# # # US senate elections - historical: https://electionlab.mit.edu/data

load(file = 'data_auxiliary/MIT_Election_Results/1976-2020-senate.RData')

senate_historical <- as.data.table(x)

senate_historical <- senate_historical[which(senate_historical$year<2020),]

senate_historical <-
  senate_historical[,
    c("year","state","party_detailed","candidatevotes","totalvotes")
    ]

senate_historical_turnout.count <-
  unique(senate_historical[,c("year","state","totalvotes")])

senate_historical_turnout.count <-
  senate_historical_turnout.count[,
    lapply(.SD,sum),
    by = c("year","state"),
    .SDcols = 'totalvotes'
    ]

senate_historical_vote.count =
  unique(senate_historical[,
    c("year","state","party_detailed","candidatevotes")
    ])

senate_historical_vote.count =
  senate_historical_vote.count[,
    lapply(.SD,sum),
    by = c("year","state","party_detailed"),
    .SDcols = 'candidatevotes'
    ]

senate_historical <-
  merge(
    senate_historical_turnout.count,
    senate_historical_vote.count,
    by = c("year","state"),
    all=TRUE
    )

senate_historical$senate_voteshare <-
  senate_historical$candidatevotes/senate_historical$totalvotes

senate_historical$party_detailed[
  which(
    senate_historical$party_detailed=="REPUBLICAN (NOT IDENTIFIED ON BALLOT)"
    )] = "REPUBLICAN"
senate_historical$party_detailed[
  which(
    senate_historical$party_detailed=="DEMOCRAT (NOT IDENTIFIED ON BALLOT)"
    )] = "DEMOCRAT"

senate_historical <-
  senate_historical[,
    lapply(.SD,sum),
    by = c("year","state","party_detailed"),
    .SDcols= 'senate_voteshare']

senate_historical <-
  reshape(
    senate_historical ,
    idvar = c("year","state"),
    timevar = 'party_detailed',
    direction = 'wide'
    )

senate_historical$senate_voteshare.REPUBLICAN[
    is.na(senate_historical$senate_voteshare.REPUBLICAN)]=0
senate_historical$senate_voteshare.OTHER[
    is.na(senate_historical$senate_voteshare.OTHER)]=0
senate_historical$senate_voteshare.DEMOCRAT[
    is.na(senate_historical$senate_voteshare.DEMOCRAT)]=0
senate_historical$senate_voteshare.LIBERTARIAN[
    is.na(senate_historical$senate_voteshare.LIBERTARIAN)]=0
senate_historical$senate_voteshare.GREEN[
    is.na(senate_historical$senate_voteshare.GREEN)]=0

senate_historical$state <- str_to_title(tolower(senate_historical$state))

senate_historical <-
  senate_historical[,
    c(
      "state",
      "year",
      "senate_voteshare.REPUBLICAN",
      "senate_voteshare.DEMOCRAT",
      "senate_voteshare.LIBERTARIAN",
      "senate_voteshare.GREEN"
      )]

# bernie
senate_historical[
  which(
    senate_historical$state=="Vermont" &
    senate_historical$year %in% c(2006,2012,2018)
    ),
    "senate_voteshare.DEMOCRAT"] <- c(0.654,0.71,0.6744)

senate_historical$senate_voteshare.OTHER <-
  1-rowSums(
    senate_historical[,
    c(
      "senate_voteshare.REPUBLICAN",
      "senate_voteshare.DEMOCRAT",
      "senate_voteshare.LIBERTARIAN",
      "senate_voteshare.GREEN"
      )],
      na.rm=TRUE)

senate_historical <-
  merge(
    senate_historical,
    expand.grid(
      state = unique(senate_historical$state),
      year = min(senate_historical$year):2020),
      by = c('state',"year"),
      all=TRUE
      )

colnames(senate_historical) <-
  gsub("senate_voteshare.","",colnames(senate_historical))

senate_historical <- melt(senate_historical,id.vars = c("state","year"))

colnames(senate_historical )[
    which(colnames(senate_historical )=="value")] <- "voteshare"
colnames(senate_historical )[
  which(colnames(senate_historical )=="variable")] <- "party"

senate_historical $type <- 'senate'

senate_historical <-
  bind_rows(
    senate_historical ,
    expand.grid(
      state = 'District of Columbia',
      year = min(senate_historical$year):2020,
      party = levels(senate_historical$party),
      type = 'senate'
      ) )

# # # (10)
# # # US house elections - historical: https://electionlab.mit.edu/data

load(file = 'data_auxiliary/MIT_Election_Results/1976-2018-house3.RData')

house_historical <- as.data.table(x)

house_historical <- house_historical[which(house_historical$year<2020),]

house_historical <-
  house_historical[,
  c("year","state",'district',"party","candidatevotes","totalvotes")
  ]

house_historical_turnout.count <-
  house_historical[,c("year","state",'district',"candidatevotes")]

house_historical_turnout.count <-
  house_historical[,
    lapply(.SD,sum),
    by = c('state','year'),
    .SDcols = c("candidatevotes")]

colnames(house_historical_turnout.count) <- c("state","year","totalvotes")

house_historical_vote.count <-
  house_historical[,c("year","state",'district',"party","candidatevotes")]

house_historical_vote.count <-
  house_historical_vote.count[,
    lapply(.SD,sum),
    by = c("year","state","party"),
    .SDcols = 'candidatevotes'
    ]

house_historical <-
  merge(
    house_historical_turnout.count,
    house_historical_vote.count,
    by = c("year","state"),
    all=TRUE
    )

house_historical$house_voteshare <-
  house_historical$candidatevotes/house_historical$totalvotes

house_historical <-
  reshape(
    house_historical,
    idvar = c("year","state"),
    timevar = 'party',
    direction = 'wide'
    )

house_historical$house_voteshare.REPUBLICAN[
  is.na(house_historical$house_voteshare.REPUBLICAN)]=0
house_historical$house_voteshare.OTHER[
  is.na(house_historical$house_voteshare.OTHER)]=0
house_historical$house_voteshare.DEMOCRAT[
  is.na(house_historical$house_voteshare.DEMOCRAT)]=0
house_historical$house_voteshare.LIBERTARIAN[
  is.na(house_historical$house_voteshare.LIBERTARIAN)]=0
house_historical$house_voteshare.GREEN[
  is.na(house_historical$house_voteshare.GREEN)]=0

house_historical$state <- str_to_title(tolower(house_historical$state))

house_historical <-
  house_historical[,
  c(
    "state",
    "year",
    "house_voteshare.REPUBLICAN",
    "house_voteshare.DEMOCRAT",
    "house_voteshare.LIBERTARIAN",
    "house_voteshare.GREEN"
    )]

house_historical$house_voteshare.OTHER <-
  1-rowSums(
      house_historical[,
        c(
          "house_voteshare.REPUBLICAN",
          "house_voteshare.DEMOCRAT",
          "house_voteshare.LIBERTARIAN",
          "house_voteshare.GREEN"
          )],
          na.rm=TRUE
          )

colnames(house_historical) <-
  gsub("house_voteshare.","",colnames(house_historical))

house_historical <- melt(house_historical,id.vars = c("state","year"))
colnames(house_historical )[
  which(colnames(house_historical )=="value")] <- "voteshare"
colnames(house_historical )[
  which(colnames(house_historical )=="variable")] <- "party"

house_historical $type <- 'house'

house_historical <-
  bind_rows(
    house_historical ,
    expand.grid(
      state = 'District of Columbia',
      year = min(house_historical$year):2020,
      party = levels(house_historical$party),
      type = 'house'
      ) )

# # # (11) harmonize elections into a single dataset
# stack elections together
historical_elections <-
  bind_rows(president_historical,senate_historical,house_historical)
historical_elections$state[
  which(historical_elections$state=="District Of Columbia")
  ] <- "District of Columbia"

# # # (12)
# add informatiuon about mid-term,
# presidential incumbency and presidential approval
historical_elections $off_year <-
  ifelse(historical_elections $year %in% seq(from = 1976,to = 2020,by = 4),0,1)

historical_elections $presidential_incumbent <-
  ifelse(
    historical_elections $year %in% seq(from = 1969,to = 1976,by = 1) &
    historical_elections $party == "REPUBLICAN",1,
  ifelse(
    historical_elections $year %in% seq(from = 1977,to = 1980,by = 1) &
    historical_elections $party == "DEMOCRAT",1,
  ifelse(
    historical_elections $year %in% seq(from = 1981,to = 1992,by = 1) &
    historical_elections $party == "REPUBLICAN",1,
  ifelse(
    historical_elections $year %in% seq(from = 1993,to = 2000,by = 1) &
    historical_elections $party == "DEMOCRAT",1,
  ifelse(
    historical_elections $year %in% seq(from = 2001,to = 2008,by = 1) &
    historical_elections $party == "REPUBLICAN",1,
  ifelse(
    historical_elections $year %in% seq(from = 2009,to = 2016,by = 1) &
    historical_elections $party == "DEMOCRAT",1,
  ifelse(historical_elections $year %in% seq(from = 2017,to = 2020,by = 1) &
    historical_elections $party == "REPUBLICAN",1,
  NA ) ) ) ) ) ) )

historical_elections $presidential_incumbent <-
ifelse(
  is.na(historical_elections $presidential_incumbent ),
  0,
  historical_elections $presidential_incumbent
  )

# # # (13)
# # # gallup presidential job approvals from https:
# //www.presidency.ucsb.edu/statistics/data/presidential-job-approval
pres_approval <-
  fread(file = 'data_auxiliary/Gallup - Presidential Job Approval.csv')
# take yearly approval rating average
pres_approval $end_date = as.Date(str_trim(str_remove(pres_approval $end_date, "^0+")),'%m/%d/%Y')
pres_approval $year = as.numeric(substr(pres_approval $end_date ,1,4))
pres_approval$net_pres_approval <-
  pres_approval$Approving-pres_approval$Disapproving
pres_approval <-
  pres_approval [,lapply(.SD,mean),
  .SDcols = c("net_pres_approval","Approving","Disapproving","Unsure/NoData"),
  by = "year"]

historical_elections <-
  merge(
    pres_approval[,c("year","net_pres_approval"),
    with=FALSE] ,historical_elections,
    by = "year",
    all.y=TRUE
    )

historical_elections $net_pres_approval_AND_presidential_incumbency <-
  historical_elections $net_pres_approval*
  historical_elections $presidential_incumbent

# # # This is the basics for us to generate 2018, 2016, 2014 and 2016 complete dataset of past elections.
historical_elections <-
  reshape(
    historical_elections[,!c("net_pres_approval_AND_presidential_incumbency")],
    timevar = c('party'),
    idvar = c(
      'type',
      "state",
      "year",
      "off_year",
      "net_pres_approval"
      ),
    direction = 'wide')

historical_elections <-
  historical_elections[,
  -grep("presidential_incumbent",colnames(historical_elections))[-1],
  with=FALSE
  ]

historical_elections <-
   historical_elections[,
   c(
    "year",
    "state",
    "type",
    "off_year",
    "presidential_incumbent.REPUBLICAN",
    "net_pres_approval",
    "voteshare.REPUBLICAN",
    "voteshare.DEMOCRAT",
    "voteshare.LIBERTARIAN",
    "voteshare.GREEN",
    "voteshare.OTHER"
    )]

# # # (14)
# # # add to this geographical distance and more geographical groupings
regions  <- fread(file = 'data_auxiliary/States_Meta/state_region_division.csv')
areas <- fread(file = 'data_auxiliary/States_Meta/US_States_Areas.csv')

state_distance <-
  distm(
    areas[,c("Longitude","Latitude"),with=FALSE],
    areas[,c("Longitude","Latitude"),with=FALSE],
    fun = distHaversine
    )
regions$State <- as.factor(regions$State)

state_distance <-
  state_distance[
    match(levels(regions$State),areas$`State and other areas2`),
    match(levels(regions$State),areas$`State and other areas2`)
    ]
colnames(state_distance) = paste("km_distance_to_",levels(regions$State),sep="")
state_distance <- state_distance /1000
state_distance <- data.table(state = levels(regions$State),state_distance)
state_distance <-
  merge(
    state_distance,
    regions[,!c("State Code")],
    by.x = "state",
    by.y= c("State"),
    all.x = TRUE
    )

# # # Complete the elections dataset
# We will use the election dataset at the area-level to explain area-level
# variance. But not all states are always up for election - therefore
# we impute the values for the elections they're not up for.
# We do this via miceRanger, including variables compatible with the
# classic `time for change` model , such as econ & presidential approval.

# # # (15)
# include unemployment rate (yearly average)

temp_econ <- fread(
    file = 'data_auxiliary/Urban_Institute_economic_indicators_state_data/unemployment_rate_raw.csv',
    header = TRUE
    )

temp_econ <- melt(temp_econ,id.vars = c("Geography"))

temp_econ <- temp_econ [-which(temp_econ $Geography=="United States"),]

# use yearly average

temp_econ$year <-
  as.numeric(substr(temp_econ$variable,1,4))

temp_econ <-
  temp_econ[,-which(colnames(temp_econ)=="variable"), with=FALSE]

temp_econ <-
  temp_econ[,lapply(.SD,mean),by = c('Geography','year'),.SDcols = 'value']

colnames(temp_econ)  <-  c("state","year","unemployment")

# # # (16)
# add state-level GDP data
#"SQINC1 Personal Income Summary: Personal Income, Population, Per Capita Personal Income"
#"Bureau of Economic Analysis"
#"1/ Midquarter population estimates by state are derived by BEA based on unpublished Census Bureau estimates of beginning-of-month population. Quarterly estimates for 2010-2019 reflect unpublished monthly population estimates available as of February 2019."
#"2/ Per capita personal income is total personal income divided by total quarterly population estimates."
#"* Estimates prior to 1950 are not available for Alaska and Hawaii."
#"Note-- Millions of dollars, seasonally adjusted at annual rates. All dollar estimates are in current dollars (not adjusted for inflation). Calculations are performed on unrounded data."
#"(NA) Not available."
#"Last updated: December 18, 2019-- new statistics for 2019:Q3; revised statistics for 2019:Q1-2019:Q2."
state_gdp = fread(file = 'data_auxiliary/BEA_state_GDP.csv')
state_gdp$GeoName = gsub(" \\*","",state_gdp$GeoName)
state_gdp = state_gdp [state_gdp $Description=="Personal income (millions of dollars, seasonally adjusted)",-c("Description","LineCode","GeoFips")]
state_gdp = melt(state_gdp,id.vars = "GeoName")
colnames(state_gdp) = c("state","year","GDP")
state_gdp$year = as.numeric(substr(state_gdp$year,1,4))
state_gdp $GDP = as.numeric(as.character(unlist(state_gdp $GDP)))
state_gdp = state_gdp[,lapply(.SD,mean,na.rm=TRUE),by =c('state','year'),.SDcols = 'GDP']
state_gdp = state_gdp [complete.cases(state_gdp ),]
state_gdp = state_gdp [-which(state_gdp $state=="United States"),]
state_gdp = state_gdp[, lag.GDP:=c(NA, GDP[-.N]), by="state"]
state_gdp$GDP_delta = 100*(state_gdp$GDP-state_gdp$lag.GDP)/state_gdp$lag.GDP

temp_econ <- merge(state_gdp[,c("state",'year','GDP','GDP_delta')],temp_econ,by = c('state','year'),all=T)

# merge econ with election and impute
historical_elections <-
  merge(
    historical_elections,
    temp_econ,
    by = c('state','year'),
    all = TRUE
    )
historical_elections<- historical_elections[!is.na(historical_elections$type)]

historical_elections <-
  merge(
    historical_elections,
    state_distance,
    by = 'state',
    all = TRUE
    )

historical_elections$state <- as.factor(historical_elections$state)
historical_elections$type <- as.factor(historical_elections$type)
historical_elections$Region <- as.factor(historical_elections$Region)
historical_elections$Division <- as.factor(historical_elections$Division)

colnames(historical_elections) <-
  make.names(colnames(historical_elections),unique = TRUE)

historical_elections$incumbent.REPUBLICAN_AND_net_pres_approval <-
  historical_elections$presidential_incumbent.REPUBLICAN *
  historical_elections$net_pres_approval

# # # (17)
# Perform mice to get voteshares for missing elections

parTime.histel <- system.time(
  miceObj.histel <-
    miceRanger(
      historical_elections,
      m = 10,
      num.trees = 500,
      verbose = TRUE,
      maxiter = 30
      )
      )

historical_elections_imp <- historical_elections

for(i in
  c(
    "voteshare.REPUBLICAN",
    "voteshare.DEMOCRAT",
    "voteshare.LIBERTARIAN",
    'voteshare.GREEN',
    'voteshare.OTHER',
    "GDP",
    "GDP_delta"
    )
    ){

  historical_elections_imp[[i]] <-
  rowMeans(
  cbind(
    miceRanger::completeData(miceObj.histel)[[1]][[i]],
    miceRanger::completeData(miceObj.histel)[[2]][[i]],
    miceRanger::completeData(miceObj.histel)[[3]][[i]],
    miceRanger::completeData(miceObj.histel)[[4]][[i]],
    miceRanger::completeData(miceObj.histel)[[5]][[i]]
  ))
}

# create unique identifier
historical_elections_imp$type_year <-
  paste(
    historical_elections_imp$type,
    historical_elections_imp$year
    )
# select last 4 election years, and reshape these voteshares into wide format
historical_elections_imp_statelevel <-
  reshape(
    historical_elections_imp[
      which(historical_elections_imp$year %in% c(2012,2014,2016,2018)),
      c(
        "state",
        "type_year",
        "voteshare.REPUBLICAN",
        "voteshare.DEMOCRAT",
        "voteshare.LIBERTARIAN",
        "voteshare.GREEN"
        )],
        idvar = c("state"),
        timevar = c('type_year'),
        direction = 'wide'
        )

# merge back with km distance
historical_elections_imp_statelevel <-
  merge(
    historical_elections_imp_statelevel,
    state_distance,
    by = "state",
    all.x = TRUE
    )

# In the end we prefer not to use the elections which are fully imputed
# remove elections which are fully imputed
historical_elections_imp_statelevel <-
historical_elections_imp_statelevel[,
  !c(

    "voteshare.DEMOCRAT.president 2014",
    "voteshare.DEMOCRAT.president 2018",

    "voteshare.REPUBLICAN.president 2014",
    "voteshare.REPUBLICAN.president 2018",

    "voteshare.LIBERTARIAN.president 2014",
    "voteshare.LIBERTARIAN.president 2018",

    "voteshare.GREEN.president 2014",
    "voteshare.GREEN.president 2018"
    )]


turnout_dt <-
  reshape(
    turnout_dt[,c('state_simple','year','stay_home')] ,
    idvar = c('state_simple'),
    direction = 'wide',
    timevar = c('year')
    )

historical_elections_imp_statelevel <-
  merge(
    historical_elections_imp_statelevel,
    turnout_dt,
    by.x = c('state'),
    by.y = c('state_simple'),
    all.x = TRUE
    )


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Now we move to bring all these datasets together to build the linear predictor

# # # Build - 1 : Area Level Prediction
# # # Area vars:

historical_elections$state <-
  as.factor(as.character(unlist(historical_elections$state)))
historical_elections$type <-
  as.factor(historical_elections$type)

# add state-similarity index
colnames(similarity_state)[1] = 'state'

AREA_pred <-
  merge(rel_state,state_abbreviations,by.x = 'state',by.y = 'abbreviation')

AREA_pred <-
  merge(
    similarity_state,
    AREA_pred,
    by.x = c('state',"abbreviation"),
    by.y = c('state.y','state'),
    all=TRUE
    )

AREA_pred <-
  merge(AREA_pred ,historical_elections_imp_statelevel,by = 'state',all=TRUE)

# # # Build - 2 : TIME-level predictor

# # # Time vars during the campaign:

nat_econ_index $modeldate = as.Date(nat_econ_index $modeldate,"%Y-%m-%d")



pres_approval <-
  fread(file = 'data_auxiliary/f38_Model_Data_and_Predictions/approval_topline.csv')
pres_approval $net_pres_approval <- pres_approval$approve_estimate-pres_approval $disapprove_estimate
pres_approval <- pres_approval[pres_approval$subgroup=="Voters"]
pres_approval $modeldate <- as.Date(pres_approval$modeldate,"%m/%d/%Y")
pres_approval <- pres_approval[,c('modeldate','net_pres_approval')]

pres_approval <-
  pres_approval[-which(
    pres_approval$modeldate>max(nat_econ_index $modeldate) |
    pres_approval$modeldate<min(nat_econ_index $modeldate)
    ),]

TIME_pred <-
  merge(
    nat_econ_index ,
    pres_approval,
    by = 'modeldate',
    all=TRUE
    )

TIME_pred <- data.table(TIME_pred )

TIME_pred <- unique(TIME_pred[,-c("state")])
TIME_pred <-
  merge(
    expand.grid(
      state = state_abbreviations$state,
      modeldate = TIME_pred$modeldate
      ),
      TIME_pred ,
      by = 'modeldate',
      all = TRUE
      )

# # # Build - 3 : AREA by TIME - level predictor

# # # Area by Time vars:
AREA_by_TIME_pred <-
  merge(
    as.data.table(state_unemp),
    as.data.table(state_houseprice),
    by = c('state','date'),
    all=TRUE
    )

AREA_by_TIME_pred <-
  merge(
    AREA_by_TIME_pred,
    as.data.table(state_gdpdelta),
    by = c('state','date'),
    all=TRUE
    )

AREA_by_TIME_pred <-
  merge(
    AREA_by_TIME_pred,
    as.data.table(covid_state),
    by = c('state','date'),
    all=TRUE
    )

AREA_by_TIME_pred <-
  AREA_by_TIME_pred[-which(
    AREA_by_TIME_pred$date>max(nat_econ_index $modeldate) |
    AREA_by_TIME_pred$date<min(nat_econ_index $modeldate)
    ),]

AREA_by_TIME_pred <- AREA_by_TIME_pred[,!"fips"]


# # # # # # # # # # # # # # # #
# # # OMNIBUS PREDICTOR # # # #
# # # # # # # # # # # # # # # #

OMNI_pred <-
  merge(
    AREA_pred ,
    AREA_by_TIME_pred ,
    by = 'state',
    all=TRUE
    )

OMNI_pred <-
  merge(
    OMNI_pred ,
    TIME_pred ,
    by.x = c('date','state'),
    by.y = c("modeldate",'state'),
    all=TRUE
    )

OMNI_pred <-
  OMNI_pred[-which(
    OMNI_pred$state=="Guam"|
    OMNI_pred$state=="Northern Mariana Islands"|
    OMNI_pred$state=="Puerto Rico"|
    OMNI_pred$state=="Virgin Islands"
    ),]

OMNI_pred$Division <- as.factor(OMNI_pred$Division)
OMNI_pred$Region <- as.factor(OMNI_pred$Region)

OMNI_pred <- one_hot(OMNI_pred,cols = c("Division","Region"))

names(OMNI_pred)[which(names(OMNI_pred)=="abbreviation")] <- "state_simple_abbreviation"
names(OMNI_pred)[which(names(OMNI_pred)=="state")] <- "state_simple"


# # # Here we choose a subset of this large OMNI_pred to fit our models.
# I try to create a sensible DGP to minimise computational effort and
# maximise bias reduction.

vars <-c('state_simple','state_simple_abbreviation','date',
    names(OMNI_pred)[
        grepl('Region',names(OMNI_pred)) | # region
        grepl('president',names(OMNI_pred)) | # past votes for president for each party by state
        grepl('stay_home.2012',names(OMNI_pred)) |
        grepl('stay_home.2016',names(OMNI_pred)) |
        grepl('Evangelical',names(OMNI_pred)) | # % evangelical by state
        grepl('White alone',names(OMNI_pred)) | # % white in each state
        grepl("Percent bachelor's degree or higher",names(OMNI_pred)) | # % degree holders in each state
        grepl('Median household income (dollars)',names(OMNI_pred)) | # median income in each state

        grepl('deaths',names(OMNI_pred)) | # state-level severity of COVID pandemic
        grepl('current_zscore.combined',names(OMNI_pred))| # state-level economic situation
        grepl('net_pres_approval',names(OMNI_pred)) # national presidential approval rating
        ] )

OMNI_pred <- OMNI_pred[,..vars]
names(OMNI_pred) <- tolower(make.names(names(OMNI_pred)))
names(OMNI_pred)[grepl('stay_home',names(OMNI_pred))] <-
  c('voteshare.stay_home.president.2012','voteshare.stay_home.president.2016')

# # # save the predictor
save(
  OMNI_pred,
  file = 'data_generated/AREA_by_CAMPAIGN_PREDICTOR.RData',
  compress = TRUE
  )


