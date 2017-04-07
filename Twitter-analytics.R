#### 1. Install packages & Executing library)
install.packages("twitteR")
install.packages("ROAuth")
install.packages("sentiment")
install.packages("devtools")
install.packages("plyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("geom_bar")
library("plyr")
library(twitteR)
library(ROAuth)
library(RCurl)
library(bitops)
library(sentiment)
library(devtools)
require(devtools)
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
require(sentiment)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(geom_bar)
ls("package:sentiment")


#### Gaining OAuth; for this step, you need to obtain Twitter Account and Auth
## Download the curl Cert and save it at your default R folder
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

##Set constant requestURL
requestURL <- "https://api.twitter.com/oauth/request_token"


## Set constant accessURL
accessURL <- "https://api.twitter.com/oauth/access_token"


##Set constant authURL
authURL <- "https://api.twitter.com/oauth/authorize"

###Replace with your own keys from twitter developer account
consumerKey="CXAKAaZE1R5rnP3dBGT9VKg6T"
consumerSecret="T0GkGtyJ2dVFLTGb5TgDL6d4yJqC2su05RVJPLF51ZOnTYL1XX"
accesstoken="2355104119-KEi5gQMdyeOmnSR8jY6VMWL3PRdn71ewpPclzIv"
accesstokensecret="uoHawJsjNCk9I1OI07gbGnUx0A8fNKxWiCoYCa6CtTSGW"
setup_twitter_oauth(consumerKey, consumerSecret, accesstoken, accesstokensecret)


##Creating the authorization object by calling function OAuthFactory
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=requestURL,accessURL=accessURL,
                             authURL=authURL)


### Asking for access
twitCred$handshake(cainfo="cacert.pem")
#twitCred$handshake(system.file("CurlSSL", "cacert.pem", package = "RCurl"))


### Asking for access
twitCred$handshake(cainfo="cacert.pem")
#twitCred$handshake(system.file("CurlSSL", "cacert.pem", package = "RCurl"))

#registerTwitterOAuth(twitCred)

#### 4. gaining OAuth; start Function Test for tui tweets gethering =====
s<-searchTwitter("@Thomsonholidays", n=10000)
s<-searchTwitter("#sensatori", n=1000)

#### 5. transform from List to DataFrame =====
s.df=twListToDF(s)

#### 6. view DataFrame =====
View(s.df)

#### 7. down load as csv file =====
write.csv(s.df, file="C:/WebandSocial/tweetstui.df.csv", row.names=F)

query <- "spain,italy,#spain,#portugal,$Spain,#Portugal,Bulgaria,Croatia,Cyprus,Greece,Iceland,italy,Lapland, Malta, Montenegro, Turkey,
Cape Verde, Mauritius, Morocco, Tunisia, India, Sri Lanka, Thailand, United Arab Emirates, Costa Rica, Mexico, Aruba, Barbados, 
Cuba, Dominican Republic,Jamaica,#Bulgaria,#Croatia,#Cyprus,#Greece,#Iceland,#Italy,#Lapland, #Malta,#Montenegro,#Turkey,#CapeVerde,
#Egypt,#Mauritius,#Morocco,#Tunisia,#India,#Sri Lanka, #Thailand, #Costa Rica, #Mexico, #Aruba, #Barbados, #Cuba, #Dominican Republic, 
#Jamaica,Sensatori,#Sensatori,Platinum,#Platinum,Scene,#Scene,Sensimar,#Sensimar,Couples,#Couples,Gold,#Gold,Weddings,#Weddings, 
Family Life, #Family Life, Robinson, #Robinson, #Villas With Pools, #Barcelona, #Dubronvik, #Sorrento, #Cuba, #Puerto Limon, 
#YourBigDay, #Wedding Wednesdays, #NotforMe"

query <- unlist(strsplit(query,","))
tweets = list()
for(i in 1:length(query)){
  result<-searchTwitter(query[i],n=1500,geocode='29.3454657,47.9969453,80mi')
  tweets <- c(tweets,result)
  tweets <- unique(tweets)
}
print(tweets)
s.tweetdf=twListToDF(tweets)
View(s.tweetdf)
write.csv(s.df, file="C:/WebandSocial/tweetstui.df.csv", row.names=F)
some_ref = sapply(result, function(x) x$getText())

#Remove Url's
some_ref = gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", query)
print(some_ref)

#Remove digits
some_ref = gsub("[0-9]+", "", query)

#Remove punctuations
some_ref = gsub("[?.;!¡¿·']", "", query)

#Convert to lower case
some_ref = sapply(some_ref, try.error)
print(some_ref)

# # classify emotion
class_emo = classify_emotion(some_ref, algorithm="bayes", prior=1.0)
print(class_emo)

#get emotion best fit
emotion = class_emo[,1]

# # substitute NA's by "unknown"
emotion[is.na(emotion)] = "joy"
print(emotion)

# # classify polarity
class_pol = classify_polarity(s, algorithm="bayes")
print(class_pol)

# # get polarity best fit
polarity = class_pol[,4]
print(polarity)

## data frame with results
sent_df = as.data.frame(some_ref, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
print(sent_df)

## sort data frame
sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
print(sent_df)

## Create a placeholder for the file
file<-NULL
## Check if tweets.csv exists
(file.exists("tweetstui.df.csv")){file<- read.csv("tweetstui.df.csv")}
df <- do.call("rbind", lapply(some_ref, as.data.frame))
df<-rbind(df,file)

## Remove duplicates
df <- df[!duplicated(df[c("id")]),]

## plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  labs(title = "Sentiment Analysis of Tweets about Thomson\n(classification by emotion)",
       plot.title = element_text(size=12))
View(df)

# Save
write.csv(df,file="C:/WebandSocial/tweetsafterquery.df.csv",row.names=FALSE)