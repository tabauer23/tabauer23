library(keyring)

# Set variables to be used in keyring.
kr_name <- "my_keyring"
kr_service <- "my_database"
kr_username <- "my_username"


keyring_lock(kr_name)
