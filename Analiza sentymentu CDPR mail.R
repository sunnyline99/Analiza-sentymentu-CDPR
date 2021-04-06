# load the blastula package
library(blastula)
library(glue)


#create_smtp_creds_key(
 # id = "gmail_a",
#  user = "analitykagospodarcza2018@gmail.com",
#  host = "smtp.gmail.com",
#  port = 465,
#  use_ssl = TRUE
#)

system_minus_godzina <- format(Sys.time()-7200, "%Y-%m-%d %H:00:00")
system_minus_minuta <- format(Sys.time()-7200, "%Y-%m-%d %H:59:59")

email_body <- glue(
"

Raport zosta³ wygenerowany {Sys.time()} 
Obejmuje zakres czasowy od {system_minus_godzina} do {system_minus_minuta}

"
)
email_footer <- 
"

Raport stowrzy³ Dawid Szyszko-Celiñski

"


email <- compose_email(body = email_body,
                       footer = email_footer)


email %>%
  add_attachment(
    file = "C:\\Users\\48799\\Desktop\\RStudio\\Projekt_licencjat\\Raport.pdf",
    content_type = mime::guess_type("C:\\Users\\48799\\Desktop\\RStudio\\Projekt_licencjat\\Raport.pdf"),
    filename = "Raport")%>%

smtp_send(
    from = "analitykagospodarcza2018@gmail.com",
    to = "analitykagospodarcza2018@gmail.com",
    subject = "Raport godzinny",
    credentials = creds_key(id = "gmail_a")
    
  )

  

