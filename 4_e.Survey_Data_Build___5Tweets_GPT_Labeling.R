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

# load twitter corpus
load(
  file = 'data_preferences/Twitter_corpus.RData'
 )

# shrink twitter corpus to users who have tweeted in the final month
tweets.list$N <- 1
tweets.list$created_at_date <- 
  substr(tweets.list$created_at,start = 1,stop = 10)
users_by_date <- 
  tweets.list[,lapply(.SD,sum),by = c('user_id','created_at_date'),.SDcols = c('N')]
users_by_date_recent <- 
  users_by_date[users_by_date$created_at_date > as.Date("02-10-2020","%d-%m-%Y")]

# aggregate now to get number of tweets per user over the time period
users_recent <- users_by_date_recent[,lapply(.SD,sum),by = c('user_id'),.SDcols = c('N')]
# drop any user who has tweeted more than once per day - likely unrepresentative,
# bots or organisations ? 
users_recent <- users_recent[users_recent$N<=30]

# only take users who have tweeted 5 or more times 
users_recent <- users_recent[users_recent$N>=5]

# ok this is the user list. Now get the tweets for these guys.
tweets.list <- tweets.list[tweets.list$user_id %in% users_recent$user_id]

# for each user, get the most recent tweet
# order by user id and created at
tweets.list <- tweets.list [rev(order(tweets.list $created_at))]

# Now for each 'user_id', select the top row when ordered by 'created_at'
# in descending order.
tweets.list$count <- 1
tweets.list[,count := sum(count),by = c('user_id')]
tweets.list[,tweet_order := 1:.N,by =  c('user_id')]
# select individuals who have tweeted more than 5 times over the period -
# we need this for a large dataset
tweets.list <- tweets.list[tweets.list$tweet_order<=5 & tweets.list$count>=5]

# prepare bio
tweets.list$bio <-
  paste(
    tweets.list$name,
    tweets.list$screen_name,
    tweets.list$location,
    tweets.list$description,
    sep = "|")

# turn into user-level dataset
users <-
  reshape(
    tweets.list,
    direction = 'wide',
    timevar = c('tweet_order'),
    idvar = c('user_id')
    )

# get text of tweets in a single variable
tweet.text <- 
  users[,
        grepl("text.",names(users)) & !grepl("retweet|quoted|display",names(users)),
        with=F]
text.names <- 
  names(
    users[,
          grepl("text.",names(users)) & !grepl("retweet|quoted|display",names(users)),
          with=F]
    )
users$text <-
  sapply(1:dim(tweet.text)[1],function(t){
    apply(
      tweet.text[t],
      1,
      function(x){
        paste0(
          text.names,":\n<<",
          x,
          collapse = ">>\n")
      } ) } )

# use gpt to generate survey from twitter data

# install openAI library
#if (!require(remotes))
#  install.packages("remotes")
#remotes::install_github("irudnyts/openai")

# load openAI library
library(openai)
library(R.utils)

# place API key in the environment
# openAI API Key: ...
Sys.setenv(
  OPENAI_API_KEY = "INSERT_KEY"
)

# define demographics we will ask to label
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

# prepare receiver
survey.list = list()

for(user in 1:dim(users)[1]){
# stopped and restarted at user = 
  
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
              <<",users$location.5[user],'>>.

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

  <<",users$bio.5[user],">> ;

  Further, they have written the following tweets:

  <<",users$text[user],'>>.

  I will now show you a number of categories to which this user may belong to.
  The categories are preceded by a header (e.g. "AGE:" or "SEX:" etc.) and an identifier (e.g. "A1", "A2" or "E1" etc.).
  Please select, for each header, the most likely category to which this user belongs to.
  In your answer present, for each header, the selected identifier.

  ',

  paste0(sample(demos_string,replace = FALSE),collapse = '\n')

  ) ) ) )

  }

    gen.demo =
      tryCatch(
        { withTimeout( {gc();gen.demo.function()}, timeout = 10) },
        error=function(e) e
      )
   while(inherits(gen.demo, "error")){
      gen.demo =
        tryCatch(
          { withTimeout({gc(); gen.demo.function()}, timeout = 10) },
          error=function(e) e
        )
    }

    print('extracted demo')


    # pack everything into a data.table
    survey =
      data.table(
        user = user,
        state = gen.state$choices$message.content,
        demo = gen.demo$choices$message.content
      )

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

    survey.list = append( list(survey), survey.list ) 

      print(survey)
      print(users[user][,c('bio.5','location.5',paste('text',5,sep='.')),with=F])
    
    # save(survey.list,file = 'data_generated/large_AI_Survey.RData',compress = T)
}

# we find more independents with this approach. It could be due to the noisier
# digital trace, as well as a genuine reflection of people who don't tweet very
# often.

# save large twitter meta data
Large_Twitter_Meta <- users

fwrite(
  Large_Twitter_Meta,
  file = "data_generated/ThreeDatasetsSystem/META/Large_Twitter_Meta.csv"
)

# clean large AI survey
load(file = 'data_generated/large_AI_Survey.RData')

survey.list <- rbindlist(survey.list)

survey.list$user_id <-
  paste(
    as.integer(as.factor(as.character(unlist(
      rev(users$user_id)
      )))),
    "Large Twitter"
    )

survey.list <-
  cbind(
    survey.list,
    users[,
          grepl("created_at",names(users)) & !grepl("retweet|quoted|display|account|date",names(users)),
          with=F][rev(1:dim(users)[1])]
  )


Large_Twitter_Map =
  data.table(
    Input.user_id = rev(users$user_id),
    user_id =
      paste(
        as.integer(as.factor(as.character(unlist(
          rev(users$user_id)
        )))),
        "Large Twitter"
      )
  )



survey.list <- merge(survey.list,Large_Twitter_Map,by = c('user_id'),all=T)
survey.list <- survey.list[,!c('Input.user_id','user')]
survey.list <- unique(survey.list)


Large_AI_Survey <- data.table()
Large_AI_Survey$user_id <- survey.list$user_id
Large_AI_Survey$dte <-
  as.numeric(
    as.character(
      unlist(
        difftime(
          as.Date("Nov 03 2020","%B %d %Y"),
          as.Date(
            substr(
              survey.list$created_at.1,
              1,
              10
            ),
            format = "%Y-%m-%d"
          ),
          units = 'days'
        )
      ) ) )

Large_AI_Survey$source <- "gpt-3.5-turbo_5_tweets"

Large_AI_Survey$vote2020 <-
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

Large_AI_Survey$state <-
  ifelse(
    grepl(
      "Not enough|Not from US|impossible|cannot determine|not clear",
      survey.list$state),
    NA,
    survey.list$state
  )

Large_AI_Survey$state  <- gsub('\\.','',Large_AI_Survey$state )


Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Hawai'i"
  )] <- "Hawaii"
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Both Texas and California"
  )] <- sample(c("Texas","California"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "I'm sorry, but I cannot provide an accurate answer as there are multiple states in the US where Waukee could be located"
  )] <- NA
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "I'm sorry, but providing a state name based on the given information is not possible as Kinnick Stadium is located in Iowa City, Iowa Therefore, the state name is Iowa"
  )] <- 'Iowa'
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "I'm sorry, but providing a state name based on the given information is not possible as Kinnick Stadium is not located in a specific state It is the football stadium of the University of Iowa, located in Iowa City, Iowa"
  )] <- 'Iowa'
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "It is not possible to determine which state in the US they live in based on the information provided \"Red State\" is a term used to describe states that typically vote for Republican candidates in elections, but it does not refer to a specific state"
  )] <- NA
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "It is unclear which state the person lives in as they have listed both Florida and Georgia"
  )] <- sample(c("Florida","Georgia"),size = 1)

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "It is unclear which state the person lives in as they have listed both Florida and Texas"
  )] <- sample(c("Florida","Texas"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "It is unclear which state the person lives in as they have listed both Texas and Oregon in their bio"
  )] <- sample(c("Oregon","Texas"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "It is unclear which state the person lives in as they have listed two different states in their bio"
  )] <- NA
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "It is unclear which state the person lives in as they have listed two different states, California and Arizona"
  )] <- sample(c("California","Arizona"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "It is unclear which state the person lives in as they have listed two options"
  )] <- NA
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "It is unclear which state the person lives in as they have listed two states in their bio"
  )] <- NA
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Not specific enough to determine a state"
  )] <- NA
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Not specific enough to determine a state The Appalachian Mountains run through multiple states including Maine, Vermont, New Hampshire, Massachusetts, Connecticut, New York, Pennsylvania, New Jersey, Maryland, West Virginia, Virginia, Kentucky, Tennessee, North Carolina, South Carolina, Georgia, and Alabama"
  )] <- sample(
  c(
    'Maine','Vermont','New Hampshire','Massachusetts','Connecticut',
    'New York', 'Pennsylvania', 'New Jersey', 'Maryland', 'West Virginia',
    'Virginia', 'Kentucky', 'Tennessee', 'North Carolina', 'South Carolina',
    'Georgia', 'Alabama'
    ),
  size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Not specific enough to determine the state The Eastern Shore refers to the eastern side of the Chesapeake Bay, which includes parts of Maryland and Virginia"
  )] <- sample(c("Maryland","Virginia"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could live in either California or Montana"
  )] <- sample(c("California","Montana"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could live in either Illinois or Tennessee"
  )] <- sample(c("Illinois","Tennessee"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could live in either Iowa or Minnesota"
  )] <- sample(c("Iowa","Minnesota"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could live in either Minnesota or California"
  )] <- sample(c("Minnesota","California"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could live in either Mississippi or Alabama"
  )] <- sample(c("Mississippi","Alabama"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could live in either Pennsylvania or Florida"
  )] <- sample(c("Pennsylvania","Florida"),size = 1)

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person lives in California"
  )] <- 'California'
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person lives in Georgia"
  )] <- 'Georgia'
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person lives in Oregon"
  )] <- 'Oregon'
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person lives in Vermont"
  )] <- 'Vermont'
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The state is Massachusetts"
  )] <- 'Massachusetts'



Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Alabama or Georgia"
  )] <- sample(c("Alabama","Georgia"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Arizona or California"
  )] <- sample(c("Arizona","California"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Arizona or Colorado"
  )] <- sample(c("Arizona","Colorado"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Arizona or Mississippi"
  )] <- sample(c("Arizona","Mississippi"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Arizona or Nevada"
  )] <- sample(c("Arizona","Nevada"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either California or Nevada"
  )] <- sample(c("California","Nevada"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either California or Oregon"
  )] <- sample(c("California","Oregon"),size = 1)

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Florida or Connecticut"
  )] <- sample(c("Florida","Connecticut"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Florida or Michigan"
  )] <- sample(c("Florida","Michigan"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Florida or New Hampshire"
  )] <- sample(c("Florida","New Hampshire"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Indiana or Ohio"
  )] <- sample(c("Indiana","Ohio"),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Kentucky or Alabama"
  )] <- sample(c("Kentucky","Alabama" ),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Massachusetts or Vermont"
  )] <- sample(c("Massachusetts","Vermont" ),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Minnesota or Wisconsin"
  )] <- sample(c("Minnesota","Wisconsin" ),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either New Jersey or Vermont"
  )] <- sample(c("New Jersey","Vermont" ),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Texas or Colorado"
  )] <- sample(c("Texas","Colorado" ),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Texas or Louisiana"
  )] <- sample(c("Texas","Louisiana" ),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Texas or Oklahoma"
  )] <- sample(c("Texas","Oklahoma" ),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Texas or Utah"
  )] <- sample(c("Texas","Utah" ),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Utah or Nevada"
  )] <- sample(c("Utah","Nevada" ),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They could live in either Virginia or North Carolina"
  )] <- sample(c("Virginia","North Carolina" ),size = 1)
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They do not specify which state they live in, as they have listed two states"
  )] <- NA
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "They live in Ohio"
  )] <- "Ohio"

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Washington, DC (District of Columbia)"
  )] <- 'District of Columbia'
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Washington, DC"
  )] <- 'District of Columbia'
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Washington DC"
  )] <- 'District of Columbia'

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Washington, DC is not a state It is a federal district Therefore, the person does not live in a state"
  )] <- 'District of Columbia'

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Washington state"
  )] <- 'Washington'

Large_AI_Survey$state <- gsub("The person lives in ","",Large_AI_Survey$state)

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could potentially live in any state in the southern region of the United States Without further information, it is not possible to determine the specific state they live in"                                                                                            
  )] <- NA

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could potentially live in any state located east of the Mississippi River, including Washington DC and other US territories Without further information, it is not possible to determine the specific state they live in"                                                                                            
  )] <- NA

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could potentially live in any state located in the eastern region of the United States"
    )] <- NA
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could potentially live in any state within the Midwest region of the United States Some possible states they could be from include Illinois, Indiana, Iowa, Kansas, Michigan, Minnesota, Missouri, Nebraska, North Dakota, Ohio, South Dakota, and Wisconsin"            
  )] <- NA
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could potentially live in any of the following states:\n\n- Delaware\n- Maryland\n- Pennsylvania\n- Virginia\n- West Virginia\n- Washington, DC"
    )] <- sample(c('Delaware','Maryland','Pennsylvania','Virginia','West Virginia','District of Columbia'),size = 1)

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could potentially live in any state along the east coast of the United States"
    )] <- NA                    
 
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Louisville is located in Kentucky, so the person would live in the state of Kentucky"
  )] <- "Kentucky"    
                     
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Minnesota, United States"
  )] <- "Minnesota"                                                                                                                                                                        
       
Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "Washington DC is not a state It is a federal district"
  )] <- "District of Columbia"        

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could potentially live in any state within the Midwest region of the United States Some possible states could include Illinois, Indiana, Iowa, Kansas, Michigan, Minnesota, Missouri, Nebraska, North Dakota, Ohio, South Dakota, or Wisconsin"                          
  )] <- NA      

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could be living in any state within the Eastern Time Zone in the USA"                                                                                                                                                                                                    
  )] <- NA  

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could potentially live in any of the following states:\n\n- Delaware\n- Maryland\n- Washington, DC\n- Pennsylvania\n- Virginia\n- West Virginia\n\nTherefore, the person could live in any of these states listed above"                                                 
  )] <- NA  

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could potentially live in any of the following states:\n\n- Delaware\n- Maryland\n- Washington, DC\n- Pennsylvania\n- Virginia\n- West Virginia\n\nTherefore, the full name of the state they live in could be any of the above options"                                 
  )] <- NA

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could potentially live in any state along the Southeastern Seaboard of the USA Some possible states include Florida, Georgia, South Carolina, North Carolina, Virginia, Maryland, Delaware, New Jersey, New York, Connecticut, Rhode Island, and Massachusetts"          
  )] <- NA

Large_AI_Survey$state[
  which(
    Large_AI_Survey$state==
      "The person could potentially live in any of the following states or territories:\n\n- Delaware\n- Maryland\n- Washington, DC\n- Pennsylvania\n- Virginia\n- West Virginia\n\nTherefore, it is not possible to determine the exact state they live in based on the given information"
  )] <- NA

# load state abbreviations
state_abbreviations <-
  fread(file = 'data_auxiliary/States_Meta/state_abbreviations.csv')

Large_AI_Survey$state <-
  state_abbreviations$abbreviation[
    match(Large_AI_Survey$state,state_abbreviations$state)
  ]

Large_AI_Survey$gender <- ifelse(survey.list$SEX=="Female","F","M")

Large_AI_Survey$ethnicity <-
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

Large_AI_Survey$commercial_estimated_hh_income <-
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

Large_AI_Survey$age_bins <-
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


Large_AI_Survey$party_code <-
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

Large_AI_Survey$vote2016 <-
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

Large_AI_Survey$modeled_college_grad <-
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

Large_AI_Survey$marital_status_code <- survey.list$`MARITAL STATUS`

# format factors and drop needless categories
Large_AI_Survey$source <- as.factor(Large_AI_Survey$source)

Large_AI_Survey$vote2020 <- as.factor(Large_AI_Survey$vote2020)

levels(Large_AI_Survey$vote2020 )[
  which(levels(Large_AI_Survey$vote2020)=="did not vote")
] = 'stay home'

#levels(Large_AI_Survey$vote2020 )[
#  which(levels(Large_AI_Survey$vote2020)=="L")
#] = 'other'

#levels(Large_AI_Survey$vote2020 )[
#  which(levels(Large_AI_Survey$vote2020)=="G")
#] = 'other'

Large_AI_Survey$vote2020 <-
  as.factor(as.character(unlist(Large_AI_Survey$vote2020)))

Large_AI_Survey$state <-
  as.factor(as.character(unlist(Large_AI_Survey$state)))

Large_AI_Survey$gender <-
  as.factor(as.character(unlist(Large_AI_Survey$gender)))

Large_AI_Survey$ethnicity <-
  as.factor(as.character(unlist(Large_AI_Survey$ethnicity)))

Large_AI_Survey$commercial_estimated_hh_income <-
  as.factor(as.character(unlist(Large_AI_Survey$commercial_estimated_hh_income)))

Large_AI_Survey <-
  Large_AI_Survey[Large_AI_Survey$age_bins!="0-17"]

Large_AI_Survey$age_bins <-
  as.factor(as.character(unlist(Large_AI_Survey$age_bins)))

Large_AI_Survey$party_code <-
  as.factor(as.character(unlist(Large_AI_Survey$party_code)))

Large_AI_Survey$vote2016 <-
  as.factor(as.character(unlist(Large_AI_Survey$vote2016)))
levels(Large_AI_Survey$vote2016)[levels(Large_AI_Survey$vote2016)=="Trump"] = 'R'
levels(Large_AI_Survey$vote2016)[levels(Large_AI_Survey$vote2016)=="Clinton"] = 'D'
levels(Large_AI_Survey$vote2016)[levels(Large_AI_Survey$vote2016)=="Other"] = 'other'
levels(Large_AI_Survey$vote2016)[levels(Large_AI_Survey$vote2016)=="Stayed Home"] = 'stay home'


Large_AI_Survey$modeled_college_grad <-
  as.factor(as.character(unlist(Large_AI_Survey$modeled_college_grad)))

Large_AI_Survey$marital_status_code <-
  as.factor(as.character(unlist(Large_AI_Survey$marital_status_code)))

names(Large_AI_Survey)[which(names(Large_AI_Survey)=="state")] <-
  'state_simple_abbreviation'

# drop missing states
Large_AI_Survey <-
  Large_AI_Survey[!is.na(Large_AI_Survey$state_simple_abbreviation)]

sum(Large_AI_Survey$user_id %in% Large_Twitter_Map$user_id)

# SPLIT SURVEY DATA INTO THREE-TABLES SYSTEM


fwrite(
  Large_Twitter_Map,
  file = "data_generated/ThreeDatasetsSystem/MAP/Large_Twitter_Map.csv"
)

fwrite(
  Large_AI_Survey,
  file = "data_generated/ThreeDatasetsSystem/SURVEY/Twitter_Survey_GPT_Labels_5_Tweets.csv"
)

