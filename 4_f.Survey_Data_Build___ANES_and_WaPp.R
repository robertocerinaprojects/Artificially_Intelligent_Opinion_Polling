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

# 4 - b: ABC/Washington Post - Swing State Polls

dir <- "data_preferences/Roper Polls/ABC News - Washington Post "

abc.florida.october <-
  fread(
    file = paste(dir,'October 2020 Florida Poll/31118054.csv',sep="")
  )

abc.michigan.october <-
  fread(
    file = paste(dir,'October 2020 Michigan Poll/31118045.csv',sep="")
  )

abc.northcarolina.october <-
  fread(
    file = paste(dir,'October 2020 North Carolina Poll/31118040.csv',sep="")
  )

abc.pennsylvania.october <-
  fread(
    file = paste(dir,'October 2020 Pennsylvania Poll/31118055.csv',sep="")
  )

abc.wisconsin.october <-
  fread(
    file = paste(dir,'October 2020 Wisconsin Poll/31118044.csv',sep="")
  )

abc.arizona.september <-
  fread(
    file = paste(dir,'September 2020 Arizona Poll/31118046.csv',sep="")
  )

abc.florida.september <-
  fread(
    file = paste(dir,'September 2020 Florida Poll/31117992.csv',sep="")
  )

abc.minnesota.september <-
  fread(
    file = paste(dir,'September 2020 Minnesota Poll/31117705.csv',sep="")
  )

abc.pennsylvania.september <-
  fread(
    file = paste(dir,'September 2020 Pennsylvania Poll/31118039.csv',sep="")
  )

abc.wisconsin.september <-
  fread(
    file = paste(dir,'September 2020 Wisconsin Poll/31117704.csv',sep="")
  )


abc <-
  rbindlist(
    list(
      abc.florida.october,
      abc.michigan.october,
      abc.northcarolina.october,
      abc.pennsylvania.october,
      abc.wisconsin.october,
      abc.arizona.september,
      abc.florida.september,
      abc.minnesota.september,
      abc.pennsylvania.september,
      abc.wisconsin.september
    ),
    fill = TRUE
  )

abc$religion <-
  ifelse(
    grepl("Catholic",abc$q911),
    "Catholic",
    ifelse(
      grepl("Baptist",abc$q911) |
        grepl("Episcopalian",abc$q911) |
        grepl("Lutheran",abc$q911) |
        grepl("Methodist",abc$q911)|
        grepl("Presbyterian",abc$q911)|
        grepl("Protestant",abc$q911) |
        grepl("Southern Baptist",abc$q911),
      "Mainline Protestant",
      ifelse(
        grepl("Agnostic",abc$q911) |
          grepl("Atheist",abc$q911)|
          grepl("NONE",abc$q911),
        "Not religious",
        ifelse(
          is.na(abc$q911) |
            abc$q911=="",
          NA,
          "Other"
        )
      )
    )
  )

abc$religion <-
  ifelse(
    grepl("Mormon",abc$q911) |
      grepl("Yes",abc$q911b),
    "Evangelical or Mormon",
    abc$religion
  )

abc$dte <-
  as.numeric(
    unlist(
      difftime(as.Date("03/11/2020","%d/%m/%Y") ,
               as.Date(
                 paste(
                   substr(abc$date8,1,4),
                   substr(abc$date8,5,6),
                   substr(abc$date8,7,8)
                 ),
                 format = "%Y %m %d"
               )
      )
    )
  )

abc$user_id <- paste('abc-wapo',abc$respo)
abc$source <- 'RDD'

abc$turnout2020 <-
  ifelse(
    abc$q2=="(VOL) Already voted",
    1,
    ifelse(
      abc$q2=="(VOL) Don't think will vote",
      0,
      ifelse(
        abc$q2=="Absolutely certain to vote",
        1,
        ifelse(
          abc$q2=="Chances 50-50",
          0.5,
          ifelse(
            abc$q2=="Less than that",
            0.25,
            ifelse(
              abc$q2=="Will probably vote",
              1,
              NA
            )
          )
        )
      )
    )
  )


abc$turnout2020 <-
  ifelse(
    !abc$q905 %in% c("Yes","Will register by Election Day"),
    # if unregistered assume no turnout
    0,
    abc$turnout2020
  )

abc$turned_out_2016 <-
  ifelse(
    abc$q107=="Did vote",
    1,
    ifelse(
      abc$q107=="Did not vote",
      0,
      NA
    )
  )

abc$q21 <- ifelse(abc$q21=="Did vote",1,ifelse(abc$q21=="Did not vote",0,NA))
abc$q23 <- ifelse(abc$q23=="Did vote",1,ifelse(abc$q23=="Did not vote",0,NA))
abc$q24 <- ifelse(abc$q24=="Did vote",1,ifelse(abc$q24=="Did not vote",0,NA))

abc$turned_out_2018 <-
  ifelse(
    is.na(abc$q21) & is.na(abc$q23) & is.na(abc$q24),
    NA,
    rowSums(cbind(abc$q21,abc$q23,abc$q24),
            na.rm=TRUE
    )
  )

abc$vote2016 <-
  ifelse(abc$q108=="Donald Trump",
         "Trump",
         ifelse(abc$q108=="Hillary Clinton",
                "Clinton",
                ifelse(
                  abc$q108=="Gary Johnson" |
                    abc$q108=="Jill Stein" |
                    abc$q108=="Someone else",
                  "Other",
                  NA
                )
         )
  )


abc$vote2016 <- ifelse(abc$turned_out_2016==0,"Stayed Home",abc$vote2016)


abc$vote2020 <-
  ifelse(
    abc$q5a=="Donald Trump and Mike Pence, the Republicans",
    "R",
    ifelse(
      abc$q5a=="Howie Hawkins and Angela Walker of the Green Party",
      "Green",
      ifelse(
        abc$q5a=="Jo Jorgensen and Spike Cohen of the Libertarian Party",
        "Libertarian",
        ifelse(
          abc$q5a=="Joe Biden and Kamala Harris, the Democrats",
          "D",
          ifelse(
            abc$q5a=="(VOL) Other candidate",
            "Other",
            NA
          )
        )
      )
    )
  )


abc$vote2020  <-
  ifelse(
    is.na(abc$vote2020),
    ifelse(
      abc$q6=="(VOL) Other candidate",
      "Other",
      ifelse(
        abc$q6=="(VOL) Other candidate (SPECIFY)",
        "Other",
        ifelse(
          abc$q6=="(VOL) Would not vote",
          "stay home",
          ifelse(
            abc$q6=="Donald Trump and Mike Pence, the Republicans",
            "R",
            ifelse(
              abc$q6=="Howie Hawkins and Angela Walker of the Green Party",
              "Green",
              ifelse(
                abc$q6=="Jo Jorgensen and Spike Cohen of the Libertarian Party",
                "Libertarian",
                ifelse(
                  abc$q6=="Joe Biden and Kamala Harris, the Democrats",
                  "D",
                  NA
                )
              )
            )
          )
        )
      )
    ),
    abc$vote2020
  )

abc$vote2020 <-
  ifelse(
    abc$turnout2020<0.75,
    'stay home',
    abc$vote2020
  )

abc$state <-
  ifelse(
    abc$q905a %in%
      c(
        "Arizona",
        "Florida",
        "Michigan",
        "Minnesota",
        "North Carolina",
        "Pennsylvania",
        "Wisconsin"
      ),
    abc$q905a ,
    NA
  )

abc$state <- ifelse(is.na(abc$state),abc$qd1,abc$state)

abc$state <- ifelse(abc$state=="",NA,abc$state)

abc$modeled_college_grad <-
  ifelse(
    abc$colleduc=="college degree",
    "Modeled College Degree",
    ifelse(
      abc$colleduc=="no college degree",
      "Modeled No College Degree",
      NA
    )
  )

abc$age_bins <-
  cut(
    as.numeric(abc$q910),
    breaks = c(17,24,34,44,54,64,max(as.numeric(abc$q910),na.rm=TRUE)),
    labels = c("18-24","25-34","35-44","45-54","55-64","65+"
    )
  )

abc$party_code <-
  ifelse(
    abc$q901=="A Republican",
    "Republican",
    ifelse(
      abc$q901=="A Democrat\xa0",
      "Democrat",
      ifelse(
        abc$q901=="An Independent",
        "Nonpartisan or Other",
        ifelse(
          abc$q901=="Or what? (SPECIFY)",
          "Nonpartisan or Other",
          NA
        )
      )
    )
  )

abc$ethnicity <-
  ifelse(
    abc$racenet=="white",
    "European",
    ifelse(
      abc$racenet=="Asian",
      "East and South Asian",
      ifelse(
        abc$racenet=="black",
        "Likely African-American",
        ifelse(
          abc$racenet=="Hispanic",
          "Hispanic and Portuguese",
          ifelse(
            abc$racenet=="other",
            "Other",
            NA
          )
        )
      )
    )
  )

abc$gender <- substr(abc$q921,1,1)

state_abbreviations <-
  fread(file = 'data_auxiliary/States_Meta/state_abbreviations.csv')

abc$state_simple_abbreviation <-
  state_abbreviations$abbreviation[
    match(abc$state,state_abbreviations$state)
  ]


anes <- fread(file = 'data_generated/ThreeDatasetsSystem/SURVEY/ANES_Survey.csv')

RDD_Surveys <-
  rbindlist(
    list(
      anes,
      abc[,colnames(abc)[colnames(abc) %in% colnames(anes)],with=FALSE]
    ),
    fill = TRUE
  )

RDD_Surveys$state_simple_abbreviation <-
  ifelse(RDD_Surveys$state_simple_abbreviation=="",NA,RDD_Surveys$state_simple_abbreviation)

RDD_Surveys <- RDD_Surveys[!is.na(RDD_Surveys$state_simple_abbreviation)]

RDD_Surveys$commercial_estimated_hh_income <-
  ifelse(RDD_Surveys$commercial_estimated_hh_income=="",NA,RDD_Surveys$commercial_estimated_hh_income)

RDD_Surveys$age_bins <- as.character(unlist(RDD_Surveys$age_bins ))
RDD_Surveys$age_bins <-
  ifelse(RDD_Surveys$age_bins=="",NA,RDD_Surveys$age_bins)

RDD_Surveys$ethnicity <-
  ifelse(RDD_Surveys$ethnicity=="",NA,RDD_Surveys$ethnicity)

RDD_Surveys$gender <-
  ifelse(RDD_Surveys$gender=="",NA,RDD_Surveys$gender)

RDD_Surveys$modeled_college_grad<-
  ifelse(RDD_Surveys$modeled_college_grad=="",NA,RDD_Surveys$modeled_college_grad)

RDD_Surveys$party_code<-
  ifelse(RDD_Surveys$party_code=="",NA,RDD_Surveys$party_code)

RDD_Surveys$vote2016<-
  as.factor(
    ifelse(RDD_Surveys$vote2016=="",NA,RDD_Surveys$vote2016)
  )

RDD_Surveys$vote2020<-
  as.factor(
    ifelse(RDD_Surveys$vote2020=="",NA,RDD_Surveys$vote2020)
  )

RDD_Surveys $marital_status_code <-
  ifelse(RDD_Surveys $marital_status_code=="",NA,RDD_Surveys $marital_status_code)


levels(RDD_Surveys$vote2016)[
  levels(RDD_Surveys$vote2016)=="Trump"
] = 'R'
levels(RDD_Surveys$vote2016)[
  levels(RDD_Surveys$vote2016)=="Clinton"
] = 'D'
levels(RDD_Surveys$vote2016)[
  levels(RDD_Surveys$vote2016)=="Other"
] = 'other'
levels(RDD_Surveys$vote2016)[
  levels(RDD_Surveys$vote2016)=="Stayed Home"
] = 'stay home'


levels(RDD_Surveys$vote2020)[
  levels(RDD_Surveys$vote2020)=="Libertarian"
] = 'L'
levels(RDD_Surveys$vote2020)[
  levels(RDD_Surveys$vote2020)=="Green"
] = 'G'
levels(RDD_Surveys$vote2020)[
  levels(RDD_Surveys$vote2020)=="Other"
] = 'other'

# complete income
library(miceRanger)
RDD_Surveys_mice <- miceRanger(data = RDD_Surveys,m = 5,maxiter = 10,verbose = T)

plotDistributions(RDD_Surveys_mice)

plotVarConvergence(RDD_Surveys_mice)

plotModelError(RDD_Surveys_mice)

getMode <- function(v) {
  freq_table <- table(v)
  max_count <- max(freq_table)
  modes <- names(freq_table)[which(freq_table == max_count)]
  return(sample(modes, 1))
}

for(j in c('commercial_estimated_hh_income','marital_status_code')){
# get mode for each imputation

  RDD_Surveys[[j]] <- as.character(unlist(RDD_Surveys[[j]]))

imp_matrix <-
  sapply(1:RDD_Surveys_mice $callParams$m,
         function(x){
           RDD_Surveys_mice $finalImps[[x]][[j]]
           }
         )
imp_mode <-
  apply(imp_matrix,1,getMode)

RDD_Surveys[[j]][
  RDD_Surveys_mice$naWhere[,j]
  ] <- imp_mode
}

ANES_and_ABC.WaPo_Survey <- RDD_Surveys

# Cleaned and labeled information for AMT workers
fwrite(
  ANES_and_ABC.WaPo_Survey,
  file = "data_generated/ThreeDatasetsSystem/SURVEY/ANES_and_ABC.WaPo_Survey.csv"
)

