#run once.
#install.packages("keyring")
library(keyring)
#setup the keyring for everyone. 

kr_name <- Sys.info()[["nodename"]] #sets the keyring name as your username login for windows
#creates the keyring
kb <- keyring::backend_file$new() #creates a back end keyring environment (only needs to be done once each time for the user to initially create their key)

#unlocks the keyring 
kb$keyring_unlock(keyring = kr_name,
                  password = tolower(Sys.info()[["user"]]))
#set DB to whichever database you want to make a password for.
service <- rstudioapi::showPrompt("Database service", "please enter service name")

#keyring service is "username" "computername" <<DATABASE YOU WANT CONNECTED>> 
# for example, "trevor.a.bauer" "095HZ209020811L" "EDWP"
kr_service <- paste0(tolower(Sys.info()[["user"]]), Sys.info()[["nodename"]], service) #sets the "service" name (in this case EBI access name to the user and node (specific to the PC) as a concat string)
kr_username <- tolower(Sys.info()[["user"]]) #sets the user name of the key to the windows user name

# Creates the service that will be used in the DBI login portion of the code. and then sets the password to the user name logging in. 
# In the first run of this, you replace the text with your password, run and do not save the file unless its in your personal drive
# Password reset ####
#when it is time to reset password, ensure keyring is unlocked and run the set_with_value section.
kb$set_with_value(service = kr_service, 
                  username = kr_username, 
                  keyring = kr_name, 
                  password = rstudioapi::askForPassword(prompt = paste0(service," Password"))) # this is the only line that will prompt a password.t 4T Y WQHYB


#locks the keyring back up
kb$keyring_lock(kr_name)


