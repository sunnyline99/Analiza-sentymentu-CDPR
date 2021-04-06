library(graphics)
library(purrr)
library(stringr)
library(tm)
library(syuzhet)
library(dplyr)
library(wordcloud)
library(rtweet)

#Czyszczenie tweetow + zamiana emotek na tekst przez wgranie z pliku + analiza emotek

Dane <- read.csv("ścieżka dostępu do danych z Twittera\\Old_data")

Dane$text <- gsub("[<>]"," ", Dane$text)
Dane$text <- gsub("000","",Dane$text)
Dane$text <- gsub("\\+","!",Dane$text)
Dane$text <- gsub("U!","",Dane$text)

Emotikony_db <- read.csv("Ścieżka dostępu do danych z emoji\\emoji_df.csv")
Emotikony_sentyment <- read.csv("ścieżka dostępu do danych z sentymentem emoji\\Emoji_Sentiment_Data.csv")
Emotikony_sentyment$codepoints <- sprintf("%x", Emotikony_sentyment$Unicode.codepoint)

Emotikony <- merge(x = Emotikony_db, y = Emotikony_sentyment, by = "codepoints", all.y = TRUE)
Emotikony <- Emotikony[,c(1,8,9,10,11,12)]
colnames(Emotikony)<-c("codepoints","Ocurrences","position","negative","neutral","positive")

Emotikony$roznica<-Emotikony$positive-Emotikony$negative

Emotikony$sentyment <-((Emotikony$roznica- mean(Emotikony$roznica))/(sd(Emotikony$roznica)))
Emotikony$sentyment_uni <-(Emotikony$roznica- min(Emotikony$roznica))/(max(Emotikony$roznica)-min(Emotikony$roznica))

Emotikony$sentyment <- ifelse(Emotikony$sentyment>=1,1,Emotikony$sentyment)
Emotikony$sentyment<- ifelse(Emotikony$sentyment<=-1,-1,Emotikony$sentyment)

Emotikony$codepoints <- toupper(Emotikony$codepoints)

z <- c()
matrix_emotikony <- c()


for(i in Emotikony$codepoints){

 z <- as.data.frame(str_count(Dane$text,i))
 z <- z * Emotikony$sentyment[Emotikony$codepoints==i]
 matrix_emotikony <- as.data.frame(append(matrix_emotikony,z))
  
}

Emotikony_sentiment_sum_mat <- as.matrix(matrix_emotikony)
Emotikony_sentiment_sum <- rowSums(Emotikony_sentiment_sum_mat)

Emotikony_sentiment_sum <- ifelse(Emotikony_sentiment_sum >=3,3,Emotikony_sentiment_sum)
Emotikony_sentiment_sum <- ifelse(Emotikony_sentiment_sum <=-3,-3,Emotikony_sentiment_sum)
Emotikony_sentiment_sum <- as.data.frame(Emotikony_sentiment_sum)

twitterCorpus <- Corpus(VectorSource(Dane$text))
inspect(twitterCorpus[1:10])

twitterCorpus <- tm_map(twitterCorpus, content_transformer(tolower))
twitterCorpus <- tm_map(twitterCorpus, removeWords, stopwords("en"))
twitterCorpus <- tm_map(twitterCorpus, removeNumbers)

## funkcje do czyszczenia linkow z tweetoW

removeURL <- function(x) gsub("http[[:alnum:]]*","",x)
twitterCorpus <- tm_map(twitterCorpus, content_transformer(removeURL))

removeURL_2 <- function(x) gsub("edua[[:alnum:]]*","",x)
twitterCorpus <- tm_map(twitterCorpus, content_transformer(removeURL_2))

## usuwanie znakow spoza ASCII np. cudzyslowie

removeNonAscii <- function(x) textclean::replace_non_ascii(x)
twitterCorpus <- tm_map(twitterCorpus, content_transformer(removeNonAscii))

## reczne usuwanie slow dotyczacych emotek itp.

twitterCorpus <- tm_map(twitterCorpus, removeWords,c("amp","ufef","ufeft","uufefuufefuufef","uufef","s","uffuffuufef"))

## usuwanie withespace czyli podwojnych spacji i wolnych miejsc

twitterCorpus <- tm_map(twitterCorpus, stripWhitespace)
twitterCorpus <- tm_map(twitterCorpus, removePunctuation)

## wrzucenie obrobionego tekstu do starej bazy danych

Dane[,1] <- twitterCorpus$content

## wazenie tweetow, jesli powyzej 10 polubien to mocniejszy wplyw albo jesli powyzej 5 retweetow


waga_favourite_count <- ifelse(Dane$favoriteCount>=10, 2.5 ,1)
waga_favourite_count <- as.data.frame(waga_favourite_count)
waga_retweets <- ifelse(Dane$retweetCount >=5, 3 ,1)
waga_retweets <- as.data.frame(waga_retweets)

## wordcloud

dtm <- TermDocumentMatrix(twitterCorpus)
dtm2 <- removeSparseTerms(dtm, sparse = 0.999)
matrix <- as.matrix(dtm2) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) 

wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, 
          rot.per=0.15,colors=brewer.pal(8, "Dark2"))

## emocjonalnosc wszystkich wypowiedzi w ca³ym badanym okresie

emotions <- get_nrc_sentiment(Dane$text)

emotions_waga <-emotions

emotions_waga$anger<-emotions_waga$anger*waga_favourite_count*waga_retweets
emotions_waga$anticipation<-emotions_waga$anticipation*waga_favourite_count*waga_retweets
emotions_waga$disgust<-emotions_waga$disgust*waga_favourite_count*waga_retweets
emotions_waga$fear<-emotions_waga$fear*waga_favourite_count*waga_retweets
emotions_waga$joy<-emotions_waga$joy*waga_favourite_count*waga_retweets
emotions_waga$sadness<-emotions_waga$sadness*waga_favourite_count*waga_retweets
emotions_waga$surprise<-emotions_waga$surprise*waga_favourite_count*waga_retweets
emotions_waga$trust<-emotions_waga$trust*waga_favourite_count*waga_retweets
emotions_waga$negative<-emotions_waga$negative*waga_favourite_count*waga_retweets
emotions_waga$positive<-emotions_waga$positive*waga_favourite_count*waga_retweets


freqs <- colSums(emotions_waga)
ylim <- c(0, 1.1*max(freqs))

xx <- barplot(freqs, cex.names=.7,
        col = rainbow(10),
        main = "Ocena emocjonalnosci wszystkich tweetow",
        ylim = ylim)

text(x = xx, y = freqs, label = freqs, pos = 3, cex = 0.8, col = "red")

## wazenie sentymentu

waga_favourite_count_sent <- ifelse(Dane$favoriteCount>=10, 1.2 ,1)
waga_favourite_count_sent <- as.data.frame(waga_favourite_count_sent)
waga_retweets_sent <- ifelse(Dane$retweetCount >=5, 1.3 ,1)
waga_retweets_sent <- as.data.frame(waga_retweets_sent)


## punktacja pozytywnosci wypowiedzi roznymi sposobami i dolaczenie do bazy danych

sentiment_syuzhet <- get_sentiment(Dane$text, method = "syuzhet") * waga_favourite_count_sent * waga_retweets_sent + Emotikony$sentyment
sentiment_bing <- get_sentiment(Dane$text, method = "bing") * waga_favourite_count_sent * waga_retweets_sent + Emotikony$sentyment
sentiment_afinn <- get_sentiment(Dane$text, method = "afinn") * waga_favourite_count_sent * waga_retweets_sent + Emotikony$sentyment
sentiment_nrc <- get_sentiment(Dane$text, method = "nrc") * waga_favourite_count_sent * waga_retweets_sent+ Emotikony$sentyment

Dane <- bind_cols(Dane,data.frame(sentiment_syuzhet),data.frame(sentiment_bing),data.frame(sentiment_afinn),data.frame(sentiment_nrc))

colnames(Dane)[6:9] <- c("sentiment_syuzhet","sentiment_bing","sentiment_afinn","sentiment_nrc")

## korelacje pomiedzy roznymi (zrobiæ pokaz jaka korelacja z emotkami i bez)

sentiments <- Dane[,c(6:9)]
cor(sentiments)

## for loop zeby wyciagnac sentyment z kazdego dnia

inds <- seq(as.Date(min(Dane$created)),as.Date(max(Dane$created)), by = "day")
inds <- as.Date(inds, "%Y-%m-%d")
inds <-as.list(inds)

suma_s<-c()
suma_b<-c()
suma_a<-c()
suma_n<-c()
liczba_tweetow <-c()



for (i in inds){
  x<- as.Date(i)
  
  
  s<-Dane$sentiment_syuzhet[Dane$created==x]
  liczba_tweetow <- append(liczba_tweetow, length(s))
  sum_s<-sum(s)
  suma_s<-append(suma_s,sum_s)
  
  b<-Dane$sentiment_bing[Dane$created==x]
  sum_b<-sum(b)
  suma_b<-append(suma_b,sum_b)
  
  a<-Dane$sentiment_afinn[Dane$created==x]
  sum_a<-sum(a)
  suma_a<-append(suma_a,sum_a)
  
  n<-Dane$sentiment_nrc[Dane$created==x]
  sum_n<-sum(n)
  suma_n<-append(suma_n,sum_n)
  
}



suma_s
suma_b
suma_a
suma_n

inds <- seq(as.Date(min(Dane$created)),as.Date(max(Dane$created)), by = "day")
inds <- as.Date(inds)
inds <- as.data.frame.Date(inds)

suma_s<-as.data.frame(suma_s/liczba_tweetow)*10
suma_b<-as.data.frame(suma_b/liczba_tweetow)*10
suma_a<-as.data.frame(suma_a/liczba_tweetow)*10
suma_n<-as.data.frame(suma_n/liczba_tweetow)*10

sentiment_ts_data <- cbind(inds,suma_s,suma_b,suma_a,suma_n)
sentiment_ts_data$inds <- as.Date(sentiment_ts_data$inds, "%Y-%m-%d")

sentiment_ts_data <- sentiment_ts_data[-c(1:5),]

inds

plot(sentiment_ts_data$inds,sentiment_ts_data$suma_s, type = "l", col ="red", ylim = c(-1,10), lwd = 2, 
     ylab = "Pozytywnoœæ wypowiedzi",
     xlab = "Data",
     xaxt="n")
axis.Date(1, at=seq(min(sentiment_ts_data$inds), max(sentiment_ts_data$inds), "days"))

lines(sentiment_ts_data$inds,sentiment_ts_data$suma_b, type = "l",col="green",lwd = 2)
lines(sentiment_ts_data$inds,sentiment_ts_data$suma_a, type = "l",col="blue",lwd = 2)
lines(sentiment_ts_data$inds,sentiment_ts_data$suma_n, type = "l",col="black",lwd = 2)
lines(sentiment_ts_data$inds,lag(not$notowania/150,n=2),col ="orange",lwd = 2)

legend("topright", legend = c("syuzhet","bing","afinn","nrc"), lty = 1, lwd = 2,col = c("red","green","blue","black"))

## analiza klastrowa

d <- dist(sentiments)

groups <- hclust(d,method = "ward.D")
plot(groups, hang = -1)

cut <- cutree(groups, k =4)
newMat <- bind_cols(sentiments, data.frame(cut), Dane$screenName, Dane$text,Dane$favoriteCount)

table(newMat$cut)

notowania <- as.data.frame(c(NA,NA,240.3,241.3,228.3,235,232,NA,NA,232,223,210.2,221,226.5,NA,NA,229.9,229.5,231,220.7,223.2, NA,NA,217,220.7,217.1,208.1,212,NA,NA,239.7,217.9,190.5,194.58,NA,NA))

not <- cbind(sentiment_ts_data[,c(2:5)],notowania)
colnames(not)<-c("s","b","a","n","notowania")
colnames(not)
cor(not)
cor.test(not$a,not$notowania)

ccf(not$n,not$notowania,lag.max = 5,type = "correlation", plot = T, na.action = na.pass)
cor.test(not$notowania,lag(not$n, n =2))

wzor <- lm(not$notowania~lag(not$n, n =2))
prognoza <- wzor$coefficients[1] +wzor$coefficients[2]*lag(not$n, n =2)

tab <- cbind(prognoza,notowania$`c(NA, NA, 240.3, 241.3, 228.3, 235, 232, NA, NA, 232, 223, 210.2, 221, 226.5, NA, NA, 229.9, 229.5, 231, 220.7, 223.2, NA, NA, 217, 220.7, 217.1, 208.1, 212, NA, NA, 239.7, 217.9, 190.5, 194.58, NA, NA)`)
