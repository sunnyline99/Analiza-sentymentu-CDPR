library(twitteR)
library(graphics)
library(purrr)
library(tm)
library(syuzhet)
library(dplyr)
library(wordcloud)

## Polaczenie z Twitter API oraz autoryzacja

api_key <- "klucz z API Twittera"
api_secret_key <- "sekretny klucz z API Twittera"
token_bearer <- "TOKEN_BEARER"
acces_token <- "TOKEN"
acces_token_secret <- "SEKRETNY TOKEN"
setup_twitter_oauth(api_key,api_secret_key,acces_token,acces_token_secret)
1

## Pobieranie danych z Twittera

CD_Projekt_RED_Hash <- searchTwitter("#CDProjektRED", n=2000, lang = "en")
Cyberpunk2077_Hash <- searchTwitter("#Cyberpunk2077", n=4000, lang = "en")
CDPR_Hash <- searchTwitter("#CDPR", n=500, lang = "en")
cd_projekt_red <- searchTwitter("cd projekt red", n=2000, lang = "en")
CD_Projekt_RED <- searchTwitter("CD Project Red", n=2000, lang = "en")
CDPR <- searchTwitter("CDPR", n=4000, lang = "en")

## Łączenie wcześniej zgromadzonych tweetów w jedną bazę danych w formacie data frame, usunięcie retweetów z bazy danych, usunięcie zbędnych zmiennych

tweets <- tbl_df(map_df(c(CD_Projekt_RED_Hash,Cyberpunk2077_Hash,CDPR_Hash,cd_projekt_red,CD_Projekt_RED,CDPR), as.data.frame))
tweets_without_retweets <-tweets[tweets$isRetweet==FALSE,]
tweets_without_retweets <- tweets_without_retweets[,c(1,3,5,11,12)]

## Zapisanie danych w formacie CSV

write.csv(tweets_without_retweets,file.path("ścieżka dostępu docelowego miejsca zapisu\\Twitter_raw_unique"), row.names = FALSE)

## Wczytanie danych w formacie CSV

twitter_raw_unique <- read.csv("ścieżka dostępu do pliku CSV\\Twitter_raw_unique")

## Odfiltrowanie tweetów z ostatnich 24 godzin i oznaczenie ich jako nowe + usunięcie duplikatów z bazy danych

ostatnie_24_h <- Sys.time()-(86400)
test_unique <- as.character.Date(twitter_raw_unique$created)>=as.character.Date(ostatnie_24_h)
twitter_raw_unique$test <- test_unique

nowe_tweety<- twitter_raw_unique[twitter_raw_unique$test==T,]
duplikaty_nowe <- duplicated(nowe_tweety$text)
sum(duplikaty_nowe)
nowe_tweety <- nowe_tweety[!duplikaty_nowe,]

tweets_new <- nowe_tweety

# za³adowanie bazy danych z wczeœniejszymi danymi i przerobienie formatu daty + usuniêcie od razu duplikatów

old_data <- read.csv("ścieżka dostępu do obecnej bazy danych z Twittera\\Old_data")
duplikaty_old <- duplicated(old_data$text)
old_data <- old_data[!duplikaty_old,]
row.names(old_data)<-c(1:length(old_data$text))
old_data<-na.omit(old_data)

## po³¹czenie 

tweets_new$created <- as.character(tweets_new$created)
dane_do_zapisu <- bind_rows(old_data,tweets_new)

duplikaty <- duplicated(dane_do_zapisu$text)
sum(duplikaty)
dane_do_zapisu <- dane_do_zapisu[!duplikaty,]

dane_do_zapisu <- dane_do_zapisu[order((dane_do_zapisu$created), decreasing = T),]
row.names(dane_do_zapisu)<-c(1:length(dane_do_zapisu$text))
dane_do_zapisu <- dane_do_zapisu[,c(1:5)]

write.csv(dane_do_zapisu, file.path("ścieżka dostępu do obecnej bazy danych z Twittera\\Old_data"), row.names = FALSE)

raw <- read.csv("ścieżka dostępu do obecnej bazy danych z Twittera\\Old_data")
duplikaty_raw <- duplicated(raw$text)
sum(duplikaty_raw)
raw <- raw[!duplikaty_raw,]
row.names(raw)<-c(1:length(raw$text))
raw<-na.omit(raw)

write.csv(raw, file.path("ścieżka dostępu do obecnej bazy danych z Twittera\\Old_data"), row.names = FALSE)

## Zapis danych w pdf

Dane <- read.csv("ścieżka dostępu do obecnej bazy danych z Twittera\\Old_data")
system_minus_godzina <- format(Sys.time()-7200, "%Y-%m-%d %H:00:00")
system_minus_minuta <- format(Sys.time()-7200, "%Y-%m-%d %H:59:59")
test <- as.character.Date(Dane$created)>=as.character.Date(system_minus_godzina)
test_2 <- as.character.Date(Dane$created) < as.character.Date(system_minus_minuta)
Dane$test <- test
Dane$test_2 <- test_2
Dane_godzina <- Dane[Dane$test==T,]
Dane_godzina <- Dane_godzina[Dane_godzina$test_2==T,]
## czyszczebie danych

twitterCorpus <- Corpus(VectorSource(Dane_godzina$text))
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

Dane_godzina[,1] <- twitterCorpus$content
Dane_godzina <- Dane_godzina[order((Dane_godzina$created),decreasing = F),]

waga_favourite_count_godzina <- ifelse(Dane_godzina$favoriteCount>=10, 2.5 ,1)
waga_favourite_count_godzina <- as.data.frame(waga_favourite_count_godzina)
waga_retweets_godzina <- ifelse(Dane_godzina$retweetCount >=5, 3 ,1)
waga_retweets_godzina <- as.data.frame(waga_retweets_godzina)


## wordcloud

dtm <- TermDocumentMatrix(twitterCorpus) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(125) 

wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, 
          rot.per=0.15,colors=brewer.pal(8, "Dark2"))

## emocje godzinne

emotions_godzina <- get_nrc_sentiment(Dane_godzina$text)

emotions_waga_godzina <-emotions_godzina

emotions_waga_godzina $anger<-emotions_waga_godzina $anger*waga_favourite_count_godzina*waga_retweets_godzina
emotions_waga_godzina $anticipation<-emotions_waga_godzina $anticipation*waga_favourite_count_godzina*waga_retweets_godzina
emotions_waga_godzina $disgust<-emotions_waga_godzina $disgust*waga_favourite_count_godzina*waga_retweets_godzina
emotions_waga_godzina $fear<-emotions_waga_godzina $fear*waga_favourite_count_godzina*waga_retweets_godzina
emotions_waga_godzina $joy<-emotions_waga_godzina $joy*waga_favourite_count_godzina*waga_retweets_godzina
emotions_waga_godzina $sadness<-emotions_waga_godzina $sadness*waga_favourite_count_godzina*waga_retweets_godzina
emotions_waga_godzina $surprise<-emotions_waga_godzina $surprise*waga_favourite_count_godzina*waga_retweets_godzina
emotions_waga_godzina $trust<-emotions_waga_godzina $trust*waga_favourite_count_godzina*waga_retweets_godzina
emotions_waga_godzina $negative<-emotions_waga_godzina $negative*waga_favourite_count_godzina*waga_retweets_godzina
emotions_waga_godzina $positive<-emotions_waga_godzina $positive*waga_favourite_count_godzina*waga_retweets_godzina


freqs_godzina <- colSums(emotions_waga_godzina)
ylim_godzina <- c(0, 1.2*max(freqs_godzina))

xx_godzina <- barplot(freqs_godzina, cex.names=.7,
                      col = rainbow(10),
                      main = "Ocena emocjonalnosci wszystkich tweetow",
                      ylim = ylim_godzina)

text(x = xx_godzina, y = freqs_godzina, label = freqs_godzina, pos = 3, cex = 0.8, col = "red")

sentyment_godzina <- as.data.frame(get_sentiment(Dane_godzina$text))
colnames(sentyment_godzina)<-"sentyment"

dtparts = t(as.data.frame(strsplit(Dane_godzina$created,' ')))
rowNames_godzina <- as.data.frame(dtparts[,2])
rownames(rowNames_godzina)<-c(1:length(Dane_godzina$text))
colnames(rowNames_godzina)<-"godzina"
sentyment_godzina <- cbind(rowNames_godzina,sentyment_godzina)

podzial <- strsplit(sentyment_godzina$godzina,":")
podzial <- as.data.frame(podzial)
podzial <- transpose(podzial)
minuty <- podzial[2]
minuty <- as.data.frame(minuty)
colnames(minuty)<-1:length(minuty)
minuty <- as.numeric(minuty)
minuty <- as.data.frame(minuty)

sentyment_godzina$minuty <- minuty$minuty
minuty_unique <- unique(sentyment_godzina$minuty)

suma_minuty <- c()

for (i in minuty_unique){
  
  z <- i
  
  su<-sentyment_godzina$sentyment[sentyment_godzina$minuty==z]
  sum_su<-sum(su)
  suma_minuty<-append(suma_minuty,sum_su)
  
}

suma_minuty
wykres <- as.data.frame(cbind(minuty_unique,suma_minuty))

x<-plot(wykres$minuty_unique ,wykres$suma_minuty, type = "l",lwd = 2, xlim = c(0,59), xlab = "Minuta", ylab = "sentyment",
        ylim = c(min(wykres$suma_minuty)-0.5,max(wykres$suma_minuty)+1.2))
points(wykres$minuty_unique ,wykres$suma_minuty, col = ifelse(wykres$suma_minuty>=0,"green","red"), pch = 19, cex = 1.2)
abline(h=0, lty = 2,lwd = 2, col = "red")
abline(h = mean(wykres$suma_minuty), lty= 3,lwd = 2, col = "blue")
legend(x=-2,y=max(wykres$suma_minuty)+1.2, legend = c("œrednia senymentu"), lty =3, lwd = 2,col = "blue")

## ZAPIS ZDJÊCIA

pdf("ścieżka dostępu do zapisania reportu w formacie PDF\\Raport.pdf")

set.seed(125) 

wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, 
          rot.per=0.15,colors=brewer.pal(8, "Dark2"))

xx_godzina <- barplot(freqs_godzina, cex.names=.7,
                      col = rainbow(10),
                      main = "Ocena emocjonalnosci wszystkich tweetow",
                      ylim = ylim_godzina)

text(x = xx_godzina, y = freqs_godzina, label = freqs_godzina, pos = 3, cex = 0.8, col = "red")

x<-plot(wykres$minuty_unique ,wykres$suma_minuty, type = "l",lwd = 2, xlim = c(0,59), xlab = "Minuta", ylab = "sentyment",
        ylim = c(min(wykres$suma_minuty)-0.5,max(wykres$suma_minuty)+1.2))
points(wykres$minuty_unique ,wykres$suma_minuty, col = ifelse(wykres$suma_minuty>=0,"green","red"), pch = 19, cex = 1.2)
abline(h=0, lty = 2,lwd = 2, col = "red")
abline(h = mean(wykres$suma_minuty), lty= 3,lwd = 2, col = "blue")
legend(x=-2,y=max(wykres$suma_minuty)+1.2, legend = c("œrednia senymentu"), lty =3, lwd = 2,col = "blue")

dev.off()
                                
