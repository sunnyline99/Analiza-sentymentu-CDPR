## W³¹czenie funkcji z poni¿szych bibliotek 
## Jeœli nie zosta³a zainstalowana jedna z bibliotek nale¿y u¿yæ funkcji install.packages(*nazwa_biblioteki*)

library(graphics)
library(purrr)
library(stringr)
library(tm)
library(syuzhet)
library(dplyr)
library(wordcloud)
library(plyr)


#Czyszczenie tweetow + zamiana emotek na tekst przez wgranie z pliku + analiza emotek

Dane <- read.csv("C:\\Users\\48799\\Desktop\\RStudio\\Projekt_licencjat\\Old_data")

Dane$text <- gsub("[<>]"," ", Dane$text)
Dane$text <- gsub("000","",Dane$text)
Dane$text <- gsub("\\+","!",Dane$text)
Dane$text <- gsub("U!","",Dane$text)

Emotikony_db <- read.csv("C:\\Users\\48799\\Desktop\\RStudio\\Projekt_licencjat\\emoji_df.csv")
Emotikony_sentyment <- read.csv("C:\\Users\\48799\\Desktop\\RStudio\\Projekt_licencjat\\Emoji_Sentiment_Data.csv")
Emotikony_sentyment$codepoints <- sprintf("%x", Emotikony_sentyment$Unicode.codepoint)

Emotikony <- merge(x = Emotikony_db, y = Emotikony_sentyment, by = "codepoints", all.y = TRUE)
Emotikony <- Emotikony[,c(1,8,9,10,11,12)]
colnames(Emotikony)<-c("codepoints","Ocurrences","position","negative","neutral","positive")

Emotikony$roznica<-Emotikony$positive-Emotikony$negative
Emotikony$sentyment <-((Emotikony$roznica- mean(Emotikony$roznica))/(sd(Emotikony$roznica)))

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

## reczne usuwanie slow dotyczacych emotikonów itp.

twitterCorpus <- tm_map(twitterCorpus, removeWords,c("amp","ufef","ufeft","uufefuufefuufef","uufef","s","uffuffuufef"))

## usuwanie withespace czyli podwojnych spacji i wolnych miejsc

twitterCorpus <- tm_map(twitterCorpus, stripWhitespace)
twitterCorpus <- tm_map(twitterCorpus, removePunctuation)

## wrzucenie obrobionego tekstu do starej bazy danych

Dane[,1] <- twitterCorpus$content

## wazenie tweetow


waga_favourite_count <- ifelse(Dane$favoriteCount>=10, 2.5 ,1)
waga_favourite_count <- as.data.frame(waga_favourite_count)
waga_retweets <- ifelse(Dane$retweetCount >=5, 3 ,1)
waga_retweets <- as.data.frame(waga_retweets)
mean(Dane$retweetCount)
mean(Dane$favoriteCount)

## tworzenie wordcloud


dtm <- TermDocumentMatrix(twitterCorpus)
dtm2 <- removeSparseTerms(dtm, sparse = 0.999)
matrix <- as.matrix(dtm2) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

search_words <- c("cyberpunkgame","cyberpunk","game","cdprojektred","cdpr","red","projekt","via","just")

for(i in search_words){
  df <- df[df$word != i,]
}


set.seed(1223) 

wordcloud(words = df$word, freq = sqrt(df$freq) , min.freq = 10,
          max.words=100, random.order=FALSE, 
          rot.per=0,colors=brewer.pal(8, "Dark2"), fixed.asp = F, scale = c(1.2,0.7))


## emocjonalnosc wszystkich wypowiedzi w ca³ym badanym okresie

emotions <- get_nrc_sentiment(Dane$text)

get_nrc_sentiment("kill loving dog")

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

xx <- barplot(freqs, cex.names=.7, names.arg = c("ANG","ANT","DIS","FEA","JOY","SAD","SUP","TRU","NEG","POS"),
        col = rainbow(10),
        main = "Ocena emocjonalnosci wszystkich tweetow",
        ylim = ylim)

text(x = xx, y = freqs, label = freqs, pos = 3, cex = 0.8, col = "red")


## ocena sentymentu czterema sposobami

sentiment_syuzhet <- get_sentiment(Dane$text, method = "syuzhet") * waga_favourite_count * waga_retweets + Emotikony$sentyment
sentiment_bing <- get_sentiment(Dane$text, method = "bing") * waga_favourite_count * waga_retweets + Emotikony$sentyment
sentiment_afinn <- get_sentiment(Dane$text, method = "afinn") * waga_favourite_count * waga_retweets + Emotikony$sentyment
sentiment_nrc <- get_sentiment(Dane$text, method = "nrc") * waga_favourite_count * waga_retweets+ Emotikony$sentyment

Dane <- bind_cols(Dane,data.frame(sentiment_syuzhet),data.frame(sentiment_bing),data.frame(sentiment_afinn),data.frame(sentiment_nrc))

colnames(Dane)[6:9] <- c("sentiment_syuzhet","sentiment_bing","sentiment_afinn","sentiment_nrc")

sentiments <- Dane[,c(6:9)]
cor(sentiments)

## for loop ¿eby wyciagnac sentyment z kazdego dnia

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


inds <- seq(as.Date(min(Dane$created)),as.Date(max(Dane$created)), by = "day")
inds <- as.Date(inds)
inds <- as.data.frame.Date(inds)

suma_s
liczba_tweetow
suma_s/liczba_tweetow

suma_s<-as.data.frame(suma_s/liczba_tweetow)*10
suma_b<-as.data.frame(suma_b/liczba_tweetow)*10
suma_a<-as.data.frame(suma_a/liczba_tweetow)*10
suma_n<-as.data.frame(suma_n/liczba_tweetow)*10

sentiment_ts_data <- cbind(inds,suma_s,suma_b,suma_a,suma_n)
sentiment_ts_data$inds <- as.Date(sentiment_ts_data$inds, "%Y-%m-%d")

sentiment_ts_data <- sentiment_ts_data[-c(1),]

## wizualizacja oceny sentymentu

plot(sentiment_ts_data$inds,sentiment_ts_data$`suma_s/liczba_tweetow`, type = "l", col ="red", ylim = c(-3,30), lwd = 2, 
     ylab = "Pozytywnoœæ wypowiedzi",
     xlab = "Data",
     xaxt="n")
axis.Date(1, at=seq(min(sentiment_ts_data$inds), max(sentiment_ts_data$inds), "days"))

lines(sentiment_ts_data$inds,sentiment_ts_data$`suma_b/liczba_tweetow`, type = "l",col="green",lwd = 2)
lines(sentiment_ts_data$inds,sentiment_ts_data$`suma_a/liczba_tweetow`, type = "l",col="blue",lwd = 2)
lines(sentiment_ts_data$inds,sentiment_ts_data$`suma_n/liczba_tweetow`, type = "l",col="black",lwd = 2)

legend("topright", legend = c("syuzhet","bing","afinn","nrc"), lty = 1, lwd = 2,col = c("red","green","blue","black"))


## Do³¹czenie notowañ oraz badanie korelacji

notowania <- as.data.frame(read.csv("C:\\Users\\48799\\Desktop\\RStudio\\Projekt_licencjat\\cdr_d.csv"))

sentiment_ts_data[,6] <- as.numeric(format.Date(sentiment_ts_data$inds,"%m%d"))
colnames(sentiment_ts_data)[6] <- c("Dat")

notowania[,7] <- as.numeric(format.Date(notowania$Data,"%m%d"))
colnames(notowania)[7] <- c("Dat")

## po³¹czenie baz danych + analiza korelacji

merge_data<- join(sentiment_ts_data,notowania, type = "left")
merge_data<- merge_data[,c(1:5,11)]
colnames(merge_data)<-c("Data","s","b","a","n","notowania")

ccf(merge_data$s,merge_data$notowania,lag.max = 10,type = "correlation", plot = T,main ="Korelacja syuzhet i notowañ CDPR", na.action = na.pass)
cor.test(merge_data$s,merge_data$notowania)

cor.test(merge_data$notowania,dplyr::lag(merge_data$n, 4))

