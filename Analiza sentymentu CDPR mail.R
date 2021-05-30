## W³¹czenie funkcji z poni¿szych bibliotek 
## Jeœli nie zosta³a zainstalowana jedna z bibliotek nale¿y u¿yæ funkcji install.packages(*nazwa_biblioteki*)

library(blastula)
library(glue)
library(mime)

## jednorazowe stworzenie klucza uatoryzuj¹cego

#create_smtp_creds_key(
 # id = "gmail_a",
#  user = "analitykagospodarcza2018@gmail.com",
#  host = "smtp.gmail.com",
#  port = 465,
#  use_ssl = TRUE
#)

## stowrzenie zmiennych do za³¹czenia w tekœcie

system_minus_godzina <- format(Sys.time()-10800, "%Y-%m-%d %H:00:00")
system_minus_minuta <- format(Sys.time()-10800, "%Y-%m-%d %H:59:59")

## Stworzenie treœci maila

email_body <- glue(
"

Raport zosta³ wygenerowany {Sys.time()} 
Obejmuje zakres czasowy od {system_minus_godzina} do {system_minus_minuta}

"
)

## Stowrzenie stopki maila

email_footer <- 
"

Raport stowrzy³ Dawid Szyszko-Celiñski

"

## po³¹czenie komponentów maila


email <- compose_email(body = email_body,
                       footer = email_footer)


## dodanie za³¹cznika w formie PDF

email %>%
  add_attachment(
    file = "C:\\Users\\48799\\Desktop\\RStudio\\Projekt_licencjat\\Raport.pdf",
    content_type = mime::guess_type("C:\\Users\\48799\\Desktop\\RStudio\\Projekt_licencjat\\Raport.pdf"),
    filename = "Raport")%>%

  
## autoryzacja oraz wys³anie maila
  
smtp_send(
    from = "analitykagospodarcza2018@gmail.com",
    to = "analitykagospodarcza2018@gmail.com",
    subject = "Raport godzinny",
    credentials = creds_key(id = "gmail_a")
    
  )

  

