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

# load utils
library(data.table)

# Sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# # # Part 5: AI survey extraction

# use gpt to generate survey from twitter data

# install openAI library
#if (!require(remotes))
#  install.packages("remotes")
#remotes::install_github("irudnyts/openai")

# load openAI library
library(openai)
library(R.utils)

# place API key in the environment
# openAI API Key: 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
Sys.setenv(
  OPENAI_API_KEY = "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"
)

# get original twitter meta-data
Twitter_Meta <-
  fread(file = 'data_generated/ThreeDatasetsSystem/META/Twitter_Meta.csv')

# rename - we are going to manipulate this
tweets.profiles_input <- Twitter_Meta

# collate the text  of the tweets
tweets.profiles_input$text <-
       apply(
              tweets.profiles_input[,
                     grepl("text",names(tweets.profiles_input)),
                     with=F],
                     1,
                     function(x){
                            paste0(
                            gsub('Input.','',
                            names(tweets.profiles_input[,
                                   grepl("text",names(tweets.profiles_input)),
                                   with=F])),":",
                                   x,
                                   collapse = "|")
                     }
              )
# nake name easier for loop
tweets.profiles_input$location <- tweets.profiles_input$Input.location
tweets.profiles_input[,!'Input.location']

# collate single bio entry
tweets.profiles_input$bio <-
  paste(
  tweets.profiles_input$Input.name,
  tweets.profiles_input$Input.screen_name,
  tweets.profiles_input$Input.location,
  tweets.profiles_input$Input.description,
  sep = "|")

# generate string of questions- we will randomise this in the prompt so it's
# useful to have it in this format
demos_string <- c(
'ETHNICITY:
E1) White
E2) Black
E3) Hispanic
E4) Asian
E5) Other

',
'AGE:
A1) between 0 and 17 year old
A2) 18 to 24 years old
A3) 25 to 34 years old
A4) 35 to 44 years old
A5) 45 to 54 years old
A6) 55 to 64 years old
A7) 65 or older

',
'SEX:
S1) Male
S2) Female

',
'MARITAL STATUS:
M1) Married
M2) Not married

',
'HIGHEST EDUCATIONAL QUALIFICATION:
Q1) no formal education
Q2) completed high-school but did not go to college
Q3) obtained a Bachelor degree or higher

',
'HOUSEHOLD INCOME BRACKET:
H1) up to 25000 USD per year
H2) between 25000 and 50000 USD per year
H3) between 50000 and 75000 USD per year
H4) between 75000 and 100000 USD per year
H5) more than 100000 USD per year

',
'THIS INDIVIDUAL IS REGISTERED AS:
R2) a Democrat
R3) a Republican
R4) an Independent

',
'2016 US PRESIDENTIAL ELECTION VOTE:
L1) did not vote
L2) voted for Donald Trump, the Republican candidate
L3) voted for Hillary Clinton, the Democrat candidate
L4) voted for Gary Johnson, the Libertarian candidate
L5) voted for Jill Stein, the Green Party candidate

',
'2018 MIDTERM ELECTION VOTE:
T1) did not vote
T2) voted for the Republican Party
T3) voted for the Democratic Party
T4) voted for a third party

',
'2020 US PRESIDENTIAL ELECTION VOTE:
V1) did not vote
V2) voted for Donald Trump, the Republican candidate
V3) voted for Joe Biden, the Democrat candidate
V4) voted for Jo Jorgensen, the Libertarian candidate
V5) voted for Howie Hawkins, the Green Party candidate

')

# setup a data list for easy conversion of the answers to survey-like
# data points

conversion <-
  strsplit(demos_string,split = '\n\n')

conversion <-
  sapply(conversion,function(x){strsplit(x,':')})

names(conversion) <-
  sapply(conversion,function(x){x[[1]]})

conversion <-
  sapply(conversion,function(x){x[[2]]})

conversion <-
  sapply(conversion,function(x){strsplit(x,split = '\n')[[1]][-1]})

conversion_labels <-
  sapply(conversion,function(x){as.data.table(strsplit(x,split = ') '))})


# initialise loop
survey.list = data.table()
meta.list = data.table()

# for each user, extract labels
for(tweet.id  in 1:dim(tweets.profiles_input)[1]){

# # # (1) derive state field

  gen.state.function = function(){
  openai::create_chat_completion(
    model = "gpt-3.5-turbo",
    temperature = 0,
    messages = list(
      list(
        "role" = "user",
        "content" =
          paste(
            "A person writes their location in their bio as follows:
            <<",tweets.profiles_input$location[tweet.id],'>>.

            Which state in the US do they live in ?

            For this answer consider Washington DC and other Territories of the US as states.

            Write out just the full name of the state, and if not from US, write "Not from US".

            '
            ) ) )
  ) }

  gen.state =
    tryCatch(
      { withTimeout( {gc();gen.state.function()}, timeout = 5) },
      error=function(e) e
    )
  while(inherits(gen.state, "error")){
    gen.state =
      tryCatch(
        { withTimeout( {gc();gen.state.function()}, timeout = 5) },
        error=function(e) e
      )
  }

  print('extracted state')

# # # (2) derive individual characteristics

  gen.demo.function <- function(){
    create_chat_completion(
      model = "gpt-3.5-turbo",
      temperature = 0,
      messages = list(
        list(
"role" = "user",
"content" =  paste(
  "A person has in their Twitter bio the following information:

<<",tweets.profiles_input$bio[tweet.id],">> ;

Further, they have written the following 10 tweets:

<<",tweets.profiles_input$text[tweet.id],'>>.

I will now show you a number of categories to which this user may belong to.
The categories are preceded by a header (e.g. "AGE:" or "SEX:" etc.) and an identifier (e.g. "A1", "A2" or "E1)" etc.).
Please select, for each header, the most likely category to which this user belongs to.
In your answer present, for each header, the selected identifier.

',

paste0(sample(demos_string,replace = FALSE),collapse = '\n')

) ) ) )

}

  gen.demo =
    tryCatch(
      { withTimeout( {gc();gen.demo.function()}, timeout = 15) },
      error=function(e) e
    )
 while(inherits(gen.demo, "error")){
    gen.demo =
      tryCatch(
        { withTimeout({gc(); gen.demo.function()}, timeout = 15) },
        error=function(e) e
      )
  }

  print('extracted demo')

# # # (3) pack output into a data.table

  survey =
    data.table(
      tweet.id = tweet.id,
      state = gen.state$choices$message.content,
      demo = gen.demo$choices$message.content
    )

# # # (4) convert output to variables

  for(k in names(conversion_labels)){

    convert <-
      as.character(unlist(conversion_labels[[k]][2]))[
        which(sapply(
          conversion_labels[[k]][1],
          function(x){
            grepl(x,survey$demo)
          }))
      ]

   if(length(convert)==0){
     survey$temp =NA
      }else{

    # if more than one match, assign at random between the two
    survey$temp <- sample(convert,size = 1)
    }

    names(survey)[which(names(survey)=="temp")] <- k
  }

# # # (5) build and save extracted survey

  survey.list = rbindlist(list( survey , survey.list ))
  print(survey.list)
# save(survey.list,file = 'data_generated/AI_Survey.RData',compress = T)

# # # (6) build and save meta-data (helps with identifying user id after) -
# good practice not to save these in the same dataset as teh original ids

  meta = cbind(tweet.id = tweet.id,tweets.profiles_input[tweet.id])
  meta.list =  rbindlist(list( meta , meta.list ))
# save(meta.list,file = 'data_generated/AI_Survey_meta.RData',compress = T)

}

# load extracted files
load(file = 'data_generated/AI_Survey.RData')
load(file = 'data_generated/AI_Survey_meta.RData')

# clean AI survey
survey.list$Input.user_id <- meta.list$Input.user_id

  survey.list <-
    cbind(
      survey.list,
      meta.list[,
      grepl("Input.created_at",names(meta.list)),
      with=F]
      )

# impress the same Twitter ID as we did other Twitter users in the AMT labeling

Twitter_Map <-
  fread(file = 'data_generated/ThreeDatasetsSystem/MAP/Twitter_Map.csv')
Twitter_Map$Input.user_id <- as.character(unlist(Twitter_Map$Input.user_id))

survey.list <- merge(survey.list,Twitter_Map,by = c('Input.user_id'),all=T)
survey.list <- survey.list[,!c('Input.user_id','tweet.id')]
survey.list <- unique(survey.list)

# clean up output
AI_Survey <- data.table()
AI_Survey$user_id <- survey.list$user_id
AI_Survey$dte <-
  as.numeric(
    as.character(
      unlist(
        difftime(
          as.Date("Nov 03 2020","%B %d %Y"),
          as.Date(
            substr(
              survey.list$Input.created_at_10,
              1,
              10
              ),
              format = "%Y-%m-%d"
              ),
              units = 'days'
              )
            ) ) )

AI_Survey$source <- "gpt-3.5-turbo"

AI_Survey$vote2020 <-
  ifelse(
    grepl('Republican',survey.list$`2020 US PRESIDENTIAL ELECTION VOTE`),'R',
    ifelse(
      grepl('Democrat',survey.list$`2020 US PRESIDENTIAL ELECTION VOTE`),'D',
        ifelse(
        grepl('Libertarian',survey.list$`2020 US PRESIDENTIAL ELECTION VOTE`),
        'L',
          ifelse(
          grepl('Green',survey.list$`2020 US PRESIDENTIAL ELECTION VOTE`),'G',
            ifelse(
            grepl(
              'did not vote',
              survey.list$`2020 US PRESIDENTIAL ELECTION VOTE`
              ),
              'did not vote',
              NA
            ) ) ) ) )
AI_Survey$vote2020 <- as.factor(AI_Survey$vote2020)

AI_Survey$state <-
ifelse(
  grepl(
    "Not enough|Not from US|impossible|cannot determine|not clear",
    survey.list$state),
  NA,
  survey.list$state
  )

AI_Survey$state  <- gsub('\\.','',AI_Survey$state )

AI_Survey$state[
  which(
    AI_Survey$state==
    "It is unclear which state the person lives in as they have listed two possible options: Louisiana (LA) and Mississippi (MS)"
    )] <- sample(c("Louisiana","Mississippi"),size = 1)
AI_Survey$state[
  which(
    AI_Survey$state==
    "It is unclear which state the person lives in as they have listed two options: LA (Louisiana) and MS (Mississippi)"
    )] <- sample(c("Louisiana","Mississippi"),size = 1)
AI_Survey$state[
  which(
    AI_Survey$state==
    "They could live in either New Jersey or Vermont"
    )] <- sample(c("New Jersey","Vermont"),size = 1)
AI_Survey$state[
  which(
    AI_Survey$state==
    "They could live in either Oklahoma or Arkansas"
    )] <- sample(c("Oklahoma","Arkansas"),size = 1)
AI_Survey$state[
  which(
    AI_Survey$state==
    "They could live in either Arizona or Nevada"
    )] <- sample(c("Arizona","Nevada"),size = 1)
AI_Survey$state[
  which(
    AI_Survey$state==
    "It is unclear which state the person lives in as they have listed two locations"
    )] <- NA
AI_Survey$state[
  which(
    AI_Survey$state==
    "It is unclear which state the person lives in as they have listed two options"
    )] <- NA
AI_Survey$state[
  which(
    AI_Survey$state==
    "Washington, DC is not a state, it is a federal district Therefore, the person does not live in a state"
    )] <- 'District of Columbia'
AI_Survey$state[
  which(
    AI_Survey$state==
    "Washington, DC is not a state, but a federal district Therefore, the person does not live in a state"
    )] <- 'District of Columbia'
AI_Survey$state[
  which(
    AI_Survey$state==
    "Washington, DC"
    )] <- 'District of Columbia'
AI_Survey$state[
  which(
    AI_Survey$state==
    "Washington DC"
    )] <- 'District of Columbia'

# load state abbreviations
state_abbreviations <-
  fread(file = 'data_auxiliary/States_Meta/state_abbreviations.csv')


AI_Survey$state <-
  state_abbreviations$abbreviation[
    match(AI_Survey$state,state_abbreviations$state)
    ]

AI_Survey$gender <- ifelse(survey.list$SEX=="Female","F","M")

AI_Survey$ethnicity <-
  ifelse(
    survey.list$ETHNICITY=="White","European",
      ifelse(
      survey.list$ETHNICITY=="Asian",'East and South Asian',
        ifelse(
        survey.list$ETHNICITY=="Black",'Likely African-American',
          ifelse(
          survey.list$ETHNICITY=="Hispanic",'Hispanic and Portuguese',
            ifelse(
            survey.list$ETHNICITY=="Other",'Other',NA
            ) ) ) ) )

AI_Survey$commercial_estimated_hh_income <-
  ifelse(
  survey.list$`HOUSEHOLD INCOME BRACKET`=="up to 25000 USD per year",
  "[min, 25000)",
    ifelse(
    survey.list$`HOUSEHOLD INCOME BRACKET`=="between 25000 and 50000 USD per year",
    "[25000, 50000)",
      ifelse(
      survey.list$`HOUSEHOLD INCOME BRACKET`=="between 50000 and 75000 USD per year",
      "[50000, 75000)",
        ifelse(
        survey.list$`HOUSEHOLD INCOME BRACKET`=="between 75000 and 100000 USD per year",
        "[75000, 100000)",
          ifelse(
          survey.list$`HOUSEHOLD INCOME BRACKET`=="more than 100000 USD per year",
          "[100000, max]",
          NA
          ) ) ) ) )

AI_Survey$age_bins <-
  ifelse(
  survey.list$AGE=="between 0 and 17 year old","0-17",
    ifelse(
    survey.list$AGE=="18 to 24 years old","18-24",
      ifelse(
      survey.list$AGE=="25 to 34 years old","25-34",
        ifelse(
        survey.list$AGE=="35 to 44 years old","35-44",
          ifelse(
          survey.list$AGE=="45 to 54 years old","45-54",
            ifelse(
            survey.list$AGE=="55 to 64 years old","55-64",
              ifelse(
              survey.list$AGE=="65 or older","65+", NA
              ) ) ) ) ) ) )
AI_Survey$age_bins <- as.factor(AI_Survey$age_bins)

AI_Survey$party_code <-
  ifelse(
    survey.list$`THIS INDIVIDUAL IS REGISTERED AS`=="a Republican",
    'Republican',
    ifelse(
      survey.list$`THIS INDIVIDUAL IS REGISTERED AS`=="a Democrat",
      'Democrat',
      ifelse(
        survey.list$`THIS INDIVIDUAL IS REGISTERED AS`=="an Independent",
        'Nonpartisan or Other',
      NA ) ) )

AI_Survey$vote2016 <-
  ifelse(
    grepl('Trump',survey.list$`2016 US PRESIDENTIAL ELECTION VOTE`),
    'Trump',
    ifelse(
      grepl('Clinton',survey.list$`2016 US PRESIDENTIAL ELECTION VOTE`),
      'Clinton',
          ifelse(
          grepl(
            'did not vote',
            survey.list$`2016 US PRESIDENTIAL ELECTION VOTE`
            ),
          'Stayed Home',
          'Other'
    ) ) )
AI_Survey$vote2016 <- as.factor(AI_Survey$vote2016)

AI_Survey$modeled_college_grad <-
  ifelse(
    grepl(
      "did not go to college",
      survey.list$`HIGHEST EDUCATUIONAL QUALIFICATION`
      ),
      'Modeled No College Degree',
    ifelse(
      survey.list$`HIGHEST EDUCATUIONAL QUALIFICATION`==
      "obtained a Bachelor degree or higher",
      "Modeled College Degree",
      NA
      ) )

AI_Survey$marital_status_code <- survey.list$`MARITAL STATUS`

AI_Survey$DedicatedWorker <- 'gpt-3.5-turbo'


# final cleaning to make uniform with other samples

#levels(AI_Survey$vote2020 )[
#  which(levels(AI_Survey$vote2020)=="L")
#  ] = 'Other'

#levels(AI_Survey$vote2020 )[
#  which(levels(AI_Survey$vote2020)=="G")
#  ] = 'Other'
levels(AI_Survey$vote2020 )[
  which(levels(AI_Survey$vote2020)=="Other")
  ] = 'other'
levels(AI_Survey$vote2020 )[
  which(levels(AI_Survey$vote2020)=="did not vote")
  ] = 'stay home'


levels(AI_Survey$vote2016)[
  levels(AI_Survey$vote2016)=="Trump"
  ] = 'R'
levels(AI_Survey$vote2016)[
  levels(AI_Survey$vote2016)=="Clinton"
  ] = 'D'
levels(AI_Survey$vote2016)[
  levels(AI_Survey$vote2016)=="Other"
  ] = 'other'
levels(AI_Survey$vote2016)[
  levels(AI_Survey$vote2016)=="Stayed Home"
  ] = 'stay home'

# drop underage people
AI_Survey <-
  AI_Survey[AI_Survey$age_bins!="0-17"]

names(AI_Survey)[which(names(AI_Survey)=="state")] <-
'state_simple_abbreviation'

AI_Survey <- AI_Survey[!is.na(AI_Survey$state_simple_abbreviation)]

# Cleaned and labeled information for AMT workers
fwrite(
  AI_Survey,
  file = "data_generated/ThreeDatasetsSystem/SURVEY/Twitter_Survey_GPT_Labels_10_Tweets.csv"
  )
