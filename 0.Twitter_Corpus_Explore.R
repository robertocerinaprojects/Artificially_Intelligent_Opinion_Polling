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
dates <- 
  seq(
    from=as.Date("2020-07-24",format = '%Y-%m-%d'),
    to=as.Date("2020-11-02",format = '%Y-%m-%d'),
    by = 1
    )

# load tweets
tweets.list <- list()
for(d in as.character(unlist(dates))){
  load(file = paste('Tweets/tweets_table_tibble_',d,'.RData',sep=""))
  tweets_table_tibble <- as.data.table(tweets_table_tibble)
  # drop non-english tweets
  tweets_table_tibble[lang=='eng']
  # append to list 
  tweets.list <- append(tweets.list,list(tweets_table_tibble))
  # print days processed
  print(paste('processed',which(dates == d),'days worth of tweets...'))
}

# make a single object 
tweets.list <- rbindlist(tweets.list)

# dedupe
tweets.list <- tweets.list[!duplicated(tweets.list$status_id),]

# numerf of unique tweets
dim(tweets.list)

# tweets date range
range(tweets.list$created_at)

# total number of unique users 
tweets.list$N <- 1
user.freq <- tweets.list[,lapply(.SD,function(x){sum(x)}),by = c('user_id'),.SDcols = c('N')]

# make an example for latex paper 
library(xtable)
# pick 10 users at random 
tweets.list.xtable <- tweets.list[user_id %in% sample(unique(tweets.list$user_id),size = 5)]
# make sure date is in character format for xtable
tweets.list.xtable$created_at <- as.character(unlist(tweets.list.xtable$created_at))
# only show a few variables
tweets.list.xtable <- tweets.list.xtable[,c('screen_name','location','description','created_at','text','user_id','status_id')]
# order
tweets.list.xtable <- tweets.list.xtable[order(user_id,status_id,created_at)]
# print in latex code
print(xtable(tweets.list.xtable), include.rownames=FALSE)

# select only individuals who tweeted at least 5 times 
user.freq.5 <- user.freq[N>=5]

# and individuals who tweeted at least 5 times
tweets.list <- tweets.list[user_id %in% user.freq.5$user_id]

# save corpus 
save(
  tweets.list,
  file = 'data_preferences/Twitter_corpus.RData'
)