# load the blastula package
library(blastula)
library(glue)


#create_smtp_creds_key(
 # id = "gmail",
#  user = "mail wysyłającego",
#  host = "smtp.gmail.com",
#  port = 465,
#  use_ssl = TRUE
#)

system_minus_godzina <- format(Sys.time()-7200, "%Y-%m-%d %H:00:00")
system_minus_minuta <- format(Sys.time()-7200, "%Y-%m-%d %H:59:59")

email_body <- glue(
"

Raport zostal wygenerowany {Sys.time()} 
Obejmuje zakres czasowy od {system_minus_godzina} do {system_minus_minuta}

"
)
email_footer <- 
"

Raport stowrzyl Dawid Szyszko-Celinski

"


email <- compose_email(body = email_body,
                       footer = email_footer)


email %>%
  add_attachment(
    file = "sciezka dostępu do zapisanego raportu w formacie pdf",
    content_type = mime::guess_type("sciezka dostępu do zapisanego raportu w formacie pdf"),
    filename = "Raport")%>%

smtp_send(
    from = "od kogo jest wysyłany mail",
    to = "do kogo jest wysyłany mail",
    subject = "Raport godzinny",
    credentials = creds_key(id = "gmail")
    
  )

  

