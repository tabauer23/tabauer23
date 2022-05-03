library(keyring)
library(DBI)
#creates the keyring
kb <- keyring::backend_file$new() 
#unlocks the keyring
service <- rstudioapi::showPrompt("Database service", "please enter service name")
kb$keyring_unlock(keyring = paste0(Sys.info()[["nodename"]]), password = tolower(Sys.info()[["user"]]))

con <- dbConnect(odbc::odbc(), dsn = service, 
                 uid = tolower(Sys.info()[["user"]]), 
                 pwd = keyring::backend_file$new()$get(service = paste0(tolower(Sys.info()[["user"]]), Sys.info()[["nodename"]], service), 
                                                       user = tolower(Sys.info()[["user"]]), 
                                                       keyring = Sys.info()[["nodename"]]))

kb$keyring_lock(Sys.info()[["nodename"]])

# make this into a function, that the user types in the arg as the dsn they want.
