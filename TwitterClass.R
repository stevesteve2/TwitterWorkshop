#### Set Up ####

#Set Working Directory 

setwd("/Users/steverathje/Desktop/Twitter\ Workshop")

#Download Packages

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "XQuartz", "tidygraph", "rtweet", "ggraph", "tidytext", "stopwords", "sentimentr", "lubridate", "textfeatures", "wordcloud", "RColorBrewer", "academicTwitterR", "dotwhisker", "jtools")
ipak(packages)
packages <- c("rtweet", "plyr")
ipak(packages)

install.packages("academictwitteR")
library(academictwitterR)

#### How to get Twitter Data ####

#Information about rtweet: https://github.com/cran/rtweet
#Apply for twitter API account: https://developer.twitter.com/en/apply-for-access 

#Create Twitter Token With Regular Account
#Enter your credentials from the Twitter API below 

create_token(
  app = "",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = "",
  set_renv = TRUE
)

#Create Twitter Token with Academic Account, see https://github.com/cjbarrie/academictwitteR
set_bearer()

#### Getting Profile #### 

steverathje2 <- get_timeline("steverathje2", n = 200)

DailyCaller <- get_timeline("DailyCaller", n = 3200)
MailOnline <- get_timeline("MailOnline", n = 3200)
FoxNews <- get_timeline("FoxNews", n = 3200)
nypost <- get_timeline("nypost", n = 3200)
BrietbartNews <- get_timeline("BreitbartNews", 3200)

dataset <- rbind(DailyCaller, MailOnline, FoxNews, nypost, BrietbartNews)

saveRDS(dataset, "dataset.rds")

#start here and read in the file if you could not download tweets
dataset <- readRDS("dataset.rds")

#### Get Recent Tweets ####

tweets <- search_tweets("steve OR bob", n = 100, include_rts = FALSE, geocode = lookup_coords("usa"))

#right-leaning low quality news sites 
infowars <- search_tweets("infowars.com*", n = 3200)
brietbart <- search_tweets("breitbart.com*", n = 3200)

#right-leaning low quality new sites
occupy <- search_tweets("occupydemocrats.com*", n = 3200)
palmer <- search_tweets("palmerreport.com*", n = 3200)

#### Academic API #### 

#View documentation here: https://github.com/cjbarrie/academictwitteR

tweets <-
  get_all_tweets(
    query = "#fakenews",
    start_tweets = "2020-01-01T00:00:00Z",
    end_tweets = "2020-01-05T00:00:00Z",
    file = "fakenews"
  )

View(tweets)

#### Other rtweet tricks ####

network <- get_friends("steverathje2")
followers <- get_followers("steverathje2")
users <- lookup_users(network$user_id)
users <- lookup_users(network$screen_name)

rate_limit(get_friends)

# DATA ANALYSIS # 

#### Get most retweeted tweets/words ####

#Look at most popular tweets 
mostPopular <-  dataset %>% 
  dplyr::select(text, retweet_count, screen_name) %>% 
  arrange(desc(retweet_count)) 

nGrams <- mostPopular %>%
  unnest_tokens(word, text, token = "ngrams", n = 1) 

nGramSort <- nGrams %>%
  group_by(word) %>%
  dplyr::summarize(n = n(),
                   avg_retweets = mean(retweet_count)) %>%
  filter(n > 10) %>%
  arrange(desc(avg_retweets))

View(nGramSort)

#### Analysis ##### 

# Read in dictionaries from dictionaries folder 
MoralEmotional <- scan("Dictionaries/MoralEmotional.txt", what='character', sep="\n", skipNul = TRUE)
Polarization <- scan("Dictionaries/Polarization.txt", what='character', sep="\n", skipNul = TRUE)
TopRepublican <- scan("Dictionaries/MostFamousRepublicans.txt", what='character', sep="\t", skipNul = TRUE)
TopDemocrat <- scan("Dictionaries/MostFamousDemocrats.txt", what='character', sep="\t", skipNul = TRUE)
DemocratCongress <- scan("Dictionaries/DemocratCongress.txt", what='character', sep="\t", skipNul = TRUE)
RepublicanCongress <- scan("Dictionaries/RepublicansCongress.txt", what='character', sep="\t", skipNul = TRUE)

#Another way to enter dictionaries 
liberalidentity = c("socialist*", "communist*", "marxist*", "leftist*", "liberal*", "left-wing*", "progressive*", "social justice warrior", "antifa", "democrat*", "dem", "dems", "libs")
conservativeidentity = c("conservative*", "gop", "republican*", "libertarian*", "alt-right", "right-wing", "fascist*", "far-right", "far right", "repub", "repubs", "maga")

#Create list of dictionaries
dictionary = dictionary(list(MoralEmotional = MoralEmotional,
                             Polarization = Polarization, 
                             Republican = TopRepublican,
                             Republican = RepublicanCongress,
                             Republican = conservativeidentity, 
                             Democrat = TopDemocrat, 
                             Democrat = DemocratCongress, 
                             Democrat = liberalidentity))

#quanteda steps 
dataset_corpus <- corpus(dataset)
toks <- tokens(dataset_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
dataset_dict <- dfm(toks, dictionary = dictionary)
dataset_dict_df <- quanteda::convert(dataset_dict, to='data.frame')
datasetcombined = cbind(dataset_dict_df, dataset)
datasetcombined$doc_id <- NULL

#### Predicting Retweets ####

datasetcombined$has_media <- is.na(datasetcombined$media_type) == FALSE
datasetcombined$has_URL <- is.na(datasetcombined$urls_url) == FALSE

#Log Transform
datasetcombined$retweet_count_log <- log(datasetcombined$retweet_count + 1)

#Model  
lm <- glm(retweet_count_log ~ Democrat + Republican + MoralEmotional + Polarization + has_media + has_URL + followers_count + is_retweet, data=datasetcombined)
lmsumm <- summ(lm, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
lmsumm

#### Crowdtangle Data ####

# download crowdtangle data 
dataset <- read_csv("crowdtangleSampleSmall.csv")

#quanteda steps 
dataset_corpus <- corpus(dataset$Message)
toks <- tokens(dataset_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
dataset_dict <- dfm(toks, dictionary = dictionary)
dataset_dict_df <- quanteda::convert(dataset_dict, to='data.frame')
datasetcombined = cbind(dataset_dict_df, dataset)
datasetcombined$doc_id <- NULL

#Add Variables
datasetcombined$has_URL <- ifelse(datasetcombined$Type == "Link", TRUE, FALSE)
datasetcombined$has_media <- ifelse(datasetcombined$Type != "Link" & datasetcombined$Type != "Status", TRUE, FALSE)
#Conservative Media Analysis
datasetcombined$shares_log <- log(datasetcombined$Shares + 1)
datasetcombined$angry_log <- log(datasetcombined$Angry + 1)


#Model  
lm <- glm(shares_log ~ Democrat + Republican + MoralEmotional + Polarization + has_media + has_URL + `Likes at Posting`, data=datasetcombined)
lmsumm <- summ(lm, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
lmsumm

#### Plotting Models #### 

plot <- dwplot(lmsumm, conf.level = .95, dot_args = list(size = 1.2),
               whisker_args = list(size = 1)) 
plot

#### Extra - TweetScores Package and Scape Congress Data ####

#Below is a loop that gets the timelines of every US congressmember: 

install_github("pablobarbera/twitter_ideology/pkg/tweetscores")
library(tweetscores)
congress <- scrapeCongressData(commit = "master")
congresstweets <- get_timeline(congress$twitter[1], n = 3200)

for (i in 1:537) {
  username <- congress$twitter[i]
  print(username)
  party <- congress$party[i]
  print(party)
  timeline <- tryCatch(get_timeline(username, n = 3200),
                       error = function(error_message) {
                         message(error_message)
                         return(NA)
                       })
  timeline$party <- party
  print(timeline$party)
  congresstweets <- rbind.fill(congresstweets, timeline)
}



