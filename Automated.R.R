## Włączenie funkcji z poniższych bibliotek 
## Jeśi nie została zainstalowana jedna z bibliotek należy użyć funkcji install.packages(*nazwa_biblioteki*)

library(twitteR)
library(graphics)
library(tm)
library(syuzhet)
library(dplyr)
library(wordcloud)
library(purrr)
library(stringr)

## Polaczenie RStudio z API Twittera

api_key <- "*klucz API ze strony twitter developer*"
api_secret_key <- "*sekretny klucz API ze strony twitter developer*"
token_bearer <- "*token ze strony twitter developer*"
acces_token <- "*acces token ze strony twitter developer*"
acces_token_secret <- "*acces secret token ze strony twitter developer*"
setup_twitter_oauth(api_key,api_secret_key,acces_token,acces_token_secret)
1

## Pobieranie danych z Twittera

CD_Projekt_RED_Hash <- searchTwitter("#CDProjektRED", n=2000, lang = "en")
Cyberpunk2077_Hash <- searchTwitter("#Cyberpunk2077", n=4000, lang = "en")
CDPR_Hash <- searchTwitter("#CDPR", n=500, lang = "en")
cd_projekt_red <- searchTwitter("cd projekt red", n=2000, lang = "en")
CD_Projekt_RED <- searchTwitter("CD Project Red", n=2000, lang = "en")
CDPR <- searchTwitter("CDPR", n=4000, lang = "en")

## Usunięcie retweetów oraz oganiczenie bazy tylko do niezbęnych kolumn + zapis danych na dysku

tweets <- tibble::as.tibble(map_df(c(CD_Projekt_RED_Hash,Cyberpunk2077_Hash,CDPR_Hash,cd_projekt_red,CD_Projekt_RED,CDPR), as.data.frame))
tweets_without_retweets <-tweets[tweets$isRetweet==FALSE,]
tweets_without_retweets <- tweets_without_retweets[,c(1,3,5,11,12)]
write.csv(tweets_without_retweets,file.path("*ścieżka dostępu do zapisania danych (twitter_raw)*"), row.names = FALSE)

## Wybór tweetów tylko z ostatnich 24 godzin + usunięcie duplikatów

twitter_raw_unique <- read.csv("*ścieżka dostępu do zapisanych danych (twitter_raw)*")
ostatnie_24_h <- Sys.time()-(86400)
test_unique <- as.character.Date(twitter_raw_unique$created)>=as.character.Date(ostatnie_24_h)
twitter_raw_unique$test <- test_unique

nowe_tweety<- twitter_raw_unique[twitter_raw_unique$test==T,]
duplikaty_nowe <- duplicated(nowe_tweety$text)
nowe_tweety <- nowe_tweety[!duplikaty_nowe,]

tweets_new <- nowe_tweety

# Załadowanie bazy danych ze starymi danymi i przerobienie formatu daty + usunięcie duplikatów

old_data <- read.csv("*ścieżka dostępu do zapisu danych (old_data)*")
duplikaty_old <- duplicated(old_data$text)
old_data <- old_data[!duplikaty_old,]
row.names(old_data)<-c(1:length(old_data$text))
old_data<-na.omit(old_data)

## Połączenie nowej i starej bazy danych + zapis bazy na dysku

tweets_new$created <- as.character(tweets_new$created)
dane_do_zapisu <- bind_rows(old_data,tweets_new)

duplikaty <- duplicated(dane_do_zapisu$text)
dane_do_zapisu <- dane_do_zapisu[!duplikaty,]

dane_do_zapisu <- dane_do_zapisu[order((dane_do_zapisu$created), decreasing = T),]
row.names(dane_do_zapisu)<-c(1:length(dane_do_zapisu$text))
dane_do_zapisu <- dane_do_zapisu[,c(1:5)]

write.csv(dane_do_zapisu, file.path("*ścieżka dostępu do zapisu danych (old_data)*"), row.names = FALSE)

raw <- read.csv("*ścieżka dostępu do zapisanych danych (old_data)*")
duplikaty_raw <- duplicated(raw$text)
raw <- raw[!duplikaty_raw,]
row.names(raw)<-c(1:length(raw$text))
raw<-na.omit(raw)

write.csv(raw, file.path("*ścieżka dostępu do zapisu danych (old_data)*"), row.names = FALSE)

## Wybór tylko ostatniej godziny z bazy danych (analiza krótkookresowa)

Dane <- read.csv("*ścieżka dostępu do zapisu danych (old_data)*")
system_minus_godzina <- format(Sys.time()-10800, "%Y-%m-%d %H:00:00")
system_minus_minuta <- format(Sys.time()-10800, "%Y-%m-%d %H:59:59")
test <- as.character.Date(Dane$created)>=as.character.Date(system_minus_godzina)
test_2 <- as.character.Date(Dane$created) < as.character.Date(system_minus_minuta)
Dane$test <- test
Dane$test_2 <- test_2
Dane_godzina <- Dane[Dane$test==T,]
Dane_godzina <- Dane_godzina[Dane_godzina$test_2==T,]


## Czyszczenie tekstu + analiza emotikonów

Dane_godzina$text <- gsub("[<>]"," ", Dane_godzina$text)
Dane_godzina$text <- gsub("000","",Dane_godzina$text)
Dane_godzina$text <- gsub("\\+","!",Dane_godzina$text)
Dane_godzina$text <- gsub("U!","",Dane_godzina$text)

Emotikony_db <- read.csv("*ścieżka dostępu do danych emoji_df.csv*")
Emotikony_sentyment <- read.csv("*ścieżka dostępu do danych Emoji_Sentiment_Data.csv*")
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
  
  z <- as.data.frame(str_count(Dane_godzina$text,i))
  z <- z * Emotikony$sentyment[Emotikony$codepoints==i]
  matrix_emotikony <- as.data.frame(append(matrix_emotikony,z))
  
}

Emotikony_sentiment_sum_mat <- as.matrix(matrix_emotikony)
Emotikony_sentiment_sum <- rowSums(Emotikony_sentiment_sum_mat)

Emotikony_sentiment_sum <- ifelse(Emotikony_sentiment_sum >=3,3,Emotikony_sentiment_sum)
Emotikony_sentiment_sum <- ifelse(Emotikony_sentiment_sum <=-3,-3,Emotikony_sentiment_sum)
Emotikony_sentiment_sum <- as.data.frame(Emotikony_sentiment_sum)

## Czyszczenie danych ze zbędnych znaków

twitterCorpus <- Corpus(VectorSource(Dane_godzina$text))
inspect(twitterCorpus[1:10])

twitterCorpus <- tm_map(twitterCorpus, content_transformer(tolower))
twitterCorpus <- tm_map(twitterCorpus, removeWords, stopwords("en"))
twitterCorpus <- tm_map(twitterCorpus, removeNumbers)

## funkcje do czyszczenia linkow z tweetoW

removeURL <- function(x) gsub("http[[:alnum:]]*","",x)
twitterCorpus <- tm_map(twitterCorpus, content_transformer(removeURL))

#removeURL_2 <- function(x) gsub("edua[[:alnum:]]*","",x)
#twitterCorpus <- tm_map(twitterCorpus, content_transformer(removeURL_2))

## usuwanie znakow spoza ASCII np. cudzysłowie

removeNonAscii <- function(x) textclean::replace_non_ascii(x)
twitterCorpus <- tm_map(twitterCorpus, content_transformer(removeNonAscii))

## reczne usuwanie słów dotyczacych emotikonów

twitterCorpus <- tm_map(twitterCorpus, removeWords,c("amp","ufef","ufeft","uufefuufefuufef","uufef","s","uffuffuufef"))

## usuwanie withespace czyli podwojnych spacji i tabulatorów

twitterCorpus <- tm_map(twitterCorpus, stripWhitespace)
twitterCorpus <- tm_map(twitterCorpus, removePunctuation)

## wrzucenie obrobionego tekstu do starej bazy danych

Dane_godzina[,1] <- twitterCorpus$content
Dane_godzina <- Dane_godzina[order((Dane_godzina$created),decreasing = F),]

## ważenie tweetów

waga_favourite_count_godzina <- ifelse(Dane_godzina$favoriteCount>=10, 2.5 ,1)
waga_favourite_count_godzina <- as.data.frame(waga_favourite_count_godzina)
waga_retweets_godzina <- ifelse(Dane_godzina$retweetCount >=5, 3 ,1)
waga_retweets_godzina <- as.data.frame(waga_retweets_godzina)


## Tworzenie wordcloud

dtm <- TermDocumentMatrix(twitterCorpus) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)
search_words <- c("cyberpunkgame","cyberpunk","game","cdprojektred","cdpr","red","projekt","via","just")

for(i in search_words){
  df <- df[df$word != i,]
}


set.seed(125) 

wordcloud(words = df$word, freq = sqrt(df$freq) , min.freq = 10,
          max.words=100, random.order=FALSE, 
          rot.per=0,colors=brewer.pal(8, "Dark2"), fixed.asp = F, scale = c(0.8,1))

## Emocjonalność godzinna

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

xx_godzina <- barplot(freqs_godzina, cex.names=.7,names.arg = c("ANG","ANT","DIS","FEA","JOY","SAD","SUP","TRU","NEG","POS"),
                      col = rainbow(10),
                      main = "Ocena emocjonalnosci wszystkich tweetow",
                      ylim = ylim_godzina)

text(x = xx_godzina, y = freqs_godzina, label = freqs_godzina, pos = 3, cex = 0.8, col = "red")

## Analiza sentymentu

sentyment_godzina <- as.data.frame(get_sentiment(Dane_godzina$text))
sentyment_godzina <- sentyment_godzina*waga_favourite_count_godzina*waga_retweets_godzina+Emotikony_sentiment_sum
colnames(sentyment_godzina)<-"sentyment"

dtparts = t(as.data.frame(strsplit(Dane_godzina$created,' ')))
rowNames_godzina <- as.data.frame(dtparts[,2])
rownames(rowNames_godzina)<-c(1:length(Dane_godzina$text))
colnames(rowNames_godzina)<-"godzina"
sentyment_godzina <- cbind(rowNames_godzina,sentyment_godzina)

podzial <- strsplit(sentyment_godzina$godzina,":")
podzial <- as.data.frame(podzial)

podzial <- t(podzial)
minuty <- podzial[,2]
minuty <- as.data.frame(minuty)
colnames(minuty)<-1:length(minuty)
minuty <- as.numeric(minuty$`1`)
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
legend(x=-2,y=max(wykres$suma_minuty)+1.2, legend = c("ednia senymentu"), lty =3, lwd = 2,col = "blue")

## Zapis PDF na dysku

pdf("*ścieżka dostępu do zapisu Raport.pdf*")

set.seed(125) 

wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, 
          rot.per=0.15,colors=brewer.pal(8, "Dark2"))

xx_godzina <- barplot(freqs_godzina, cex.names=.7,names.arg = c("ANG","ANT","DIS","FEA","JOY","SAD","SUP","TRU","NEG","POS"),
                      col = rainbow(10),
                      main = "Ocena emocjonalnosci wszystkich tweetow",
                      ylim = ylim_godzina)

text(x = xx_godzina, y = freqs_godzina, label = freqs_godzina, pos = 3, cex = 0.8, col = "red")

x<-plot(wykres$minuty_unique ,wykres$suma_minuty, type = "l",lwd = 2, xlim = c(0,59), xlab = "Minuta", ylab = "sentyment",
        ylim = c(min(wykres$suma_minuty)-0.5,max(wykres$suma_minuty)+1.2))
points(wykres$minuty_unique ,wykres$suma_minuty, col = ifelse(wykres$suma_minuty>=0,"green","red"), pch = 19, cex = 1.2)
abline(h=0, lty = 2,lwd = 2, col = "red")
abline(h = mean(wykres$suma_minuty), lty= 3,lwd = 2, col = "blue")
legend(x=-2,y=max(wykres$suma_minuty)+1.2, legend = c("ednia senymentu"), lty =3, lwd = 2,col = "blue")

dev.off()
                                
