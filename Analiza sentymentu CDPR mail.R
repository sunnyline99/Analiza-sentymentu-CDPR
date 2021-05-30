## Włączenie funkcji z poniższych bibliotek 
## Jeśli nie została zainstalowana jedna z bibliotek należy użyć funkcji install.packages(*nazwa_biblioteki*)

library(blastula)
library(glue)
library(mime)

## jednorazowe stworzenie klucza autoryzującego

#create_smtp_creds_key(
 # id = "gmail_a",
#  user = "*mail wysyłającego*",
#  host = "smtp.gmail.com",
#  port = 465,
#  use_ssl = TRUE
#)

## stowrzenie zmiennych do załączenia w tekście

system_minus_godzina <- format(Sys.time()-10800, "%Y-%m-%d %H:00:00")
system_minus_minuta <- format(Sys.time()-10800, "%Y-%m-%d %H:59:59")

## Stworzenie treści maila

email_body <- glue(
"

Raport został wygenerowany {Sys.time()} 
Obejmuje zakres czasowy od {system_minus_godzina} do {system_minus_minuta}

"
)

## Stowrzenie stopki maila

email_footer <- 
"

Raport stowrzył XYZ

"

## połączenie komponentów maila


email <- compose_email(body = email_body,
                       footer = email_footer)


## dodanie załącznika w formie PDF

email %>%
  add_attachment(
    file = "*ścieżka dostępu do Raport.pdf*",
    content_type = mime::guess_type("*ścieżka dostępu do Raport.pdf*"),
    filename = "Raport")%>%

  
## autoryzacja oraz wysłanie maila
  
smtp_send(
    from = "*mail wysyłającego*",
    to = "*mail odbiorcy*",
    subject = "Raport godzinny",
    credentials = creds_key(id = "gmail_a")
    
  )

  

