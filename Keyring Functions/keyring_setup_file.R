library(keyring)


kr_name <- Sys.info()[["nodename"]] #sets the keyring name as your username login for windows
kb <- keyring::backend_file$new() #creates a back end keyring environment (only needs to be done once each time for the user to initially create their key)
#creates the keyring itself in the registry called the above kr_name (in this case sysname so its not complicated)
kb$keyring_create(keyring = kr_name, 
                  password = tolower(Sys.info()[["user"]])) #creates the keyring in the registry itself, saved as kr_name and the password is again the username. 

#locks the keyring back up
kb$keyring_lock(kr_name)