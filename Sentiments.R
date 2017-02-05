#Importing all required libraries
require(twitteR)
require(RCurl)
require(stringr)

#Establishing a connection using credentials
setup_twitter_oauth(consumer_key = "###", consumer_secret = "###", access_token = "###", access_secret = "###")

#Extracting tweets based on keyword
tweets <- searchTwitter("#Demonetisation", n =3000,lang = "en")
df <- twListToDF(tweets)

#Loading Emoticons list
a <- read.csv("emDict.csv", sep = ";", stringsAsFactors = FALSE)

score.sentiment <- function(txt, pos.words, neg.words){

scores <- lapply(txt,function(txt, pos.words, neg.words){
  
  #Converting emoticons to words they represent
  i <- iconv(txt, "latin1", "ASCII", "byte")
  u <-unlist(strsplit(i,"\\s+"))
  e <- match(u,a[[4]])
  u <-replace(u,which(!is.na(e)), a[na.omit(e), 1])
  u <- toString(u)
  txt <- gsub(",", "",u)
  
  #Cleaning tweets to get actionable data
  txt <- tolower(txt)
  txt <- gsub("rt","", txt)
  txt <- gsub("http\\S+\\s*","",txt)
  txt <- gsub(" $", "", txt)
  txt <- gsub("@\\w+", "", txt)
  txt <- gsub(":)", " happy ", txt)
  txt <- gsub(";\\)|:P", " hiliarious ", txt)
  txt <- txt <- gsub(":\\(|:'", " sad ", txt)
  txt <- gsub("[[:punct:]]", "", txt)
  
  words <- unlist(strsplit(txt,"\\s+"))
  
  #Calculating sentiment score
  pos.matches <- match(words, pos.words)
  neg.matches <- match(words, neg.words)
  
  pos.matches <- !is.na(pos.matches)
  neg.matches <- !is.na(neg.matches)
  
  score = sum(pos.matches) - sum(neg.matches)
  
  score
},pos.words, neg.words)

Em_sc <- as.numeric(unlist(scores))

score.df = data.frame(Em_sc, text = txt)
score.df
}

#Loading negative and positive word list
pos.words <- scan("C:/Users/Parul Khare/Desktop/Sentiment wordlist/positive-words.txt", what = "character", comment.char=";")
neg.words <- scan("C:/Users/Parul Khare/Desktop/Sentiment wordlist/negative-words.txt", what = "character", comment.char=";")

#Calculating final sentiment score and visualising
final <- score.sentiment(df$text, pos.words, neg.words)
table(final$Em_sc)
par(mar = c(4,4,1,1))
hist(final$Em_sc, col = "red", xlab = "Emotion_score", ylab = "Frequency of Emotion score", main = "Emotion Score")











