
#title: "read.hospital fxn"
#author: "Will Moir, Trevor Bauer, & Carly Zimmermann"
#date: "9/2/2021"
#output: word_document

#r Function to read in each hospital excel sheet
  
# This function will read in each hospital's Excel file. Supply it with the
# name of the hospital, name of the Excel file, and year


# To test the function, run each line inside it. First set the following variables
# (which normally would be set when then function is run). These can also be set to
# any particular hospital if the function is not importing it correctly.

# Warnings - these do not necessarily mean anything went wrong.

# The following seems harmless, the correct data was read in - but double check!
# 1: In read_fun(path = path, sheet_i = sheet, limits = limits, shim = shim,  :
# Expecting numeric in B38 / R38C2: got a date

# The following occurs if there is a n/a or other text that gets read in and is not
# hardcoded to be removed in the function code. It should be set to NA and removed anyway
# to this is harmless
# Warning message:
#In evalq(as.numeric(`%`)/100, <environment>) : NAs introduced by coercion

# For any other warnings, check that the data was read in correctly

#hospital_name = "New London Hospital"
#excel_name = "newlondon"
#year = 2019

read.hospital <- function(hospital_name, excel_name, year){
  
  # MODIFICATION - update this path to the current year  
  fullpath <- str_c("R:/OCPH/EPI/BCDC-BCDS/Group/HAI Program/Antimicrobial Resistance/1. Antibiogram/", year," Antibiogram/", year," Hospital Antibiogram Data/", excel_name)     
  
  # We want to read in gram positive and gram negative organisms as separate tables,
  # since each has different column names (antibiotics).
  
  # Since hospitals might add rows (Enterococcus if they do not speciate), we cannot
  # hardcode the range of the cells that each table will be. So we can read in each sheet,
  # and identify which rows certain key cells are.
  
  # If the template sheet is changed so that rows cannot be modified, this section can
  # be removed and the cell ranges can be hardcoded into the read_excel() functions
  
  uo <- read_excel(fullpath, 
                   sheet = "Urine Only Antibiogram Form",
                   col_names = FALSE)
  uo_gno_row <- which(uo$...1 == "Gram Negative Organisms")
  uo_gpo_row <- which(uo$...1 == "Gram Positive Organisms")
  uo_last_row <- nrow(uo)
  
  nu <- read_excel(fullpath, 
                   sheet = "Non Urine Antibiogram Form",
                   col_names = FALSE)
  nu_gno_row <- which(nu$...1 == "Gram Negative Organisms")
  nu_gpo_row <- which(nu$...1 == "Gram Positive Organisms")
  nu_last_row <- nrow(nu)
  
  # check that these give the correct ranges.
  range.uogn <- paste("A", uo_gno_row + 1, ":AZ", uo_gpo_row, sep = "")
  range.uogp <- paste("A", uo_gpo_row + 1, ":AN", uo_last_row + 1, sep = "")
  range.nugn <- paste("A", nu_gno_row + 1, ":AZ", nu_gpo_row, sep = "")
  range.nugp <- paste("A", nu_gpo_row + 1, ":AP", nu_last_row + 1, sep = "")
  
  # read in the files. uogn = urine only gram negative, etc
  uogn <- read_excel(fullpath, 
                     sheet = "Urine Only Antibiogram Form",
                     range = paste("A", uo_gno_row + 1, ":AZ", uo_gpo_row, sep = ""))
  uogp <- read_excel(fullpath, 
                     sheet = "Urine Only Antibiogram Form",
                     range = paste("A", uo_gpo_row + 1, ":AN", uo_last_row + 1, sep = ""))
  nugn <- read_excel(fullpath, 
                     sheet = "Non Urine Antibiogram Form",
                     range = paste("A", nu_gno_row + 1, ":AZ", nu_gpo_row, sep = ""))
  nugp <- read_excel(fullpath, 
                     sheet = "Non Urine Antibiogram Form",
                     range = paste("A", nu_gpo_row + 1, ":AP", nu_last_row + 1, sep = ""))
  
  
  # check that all columns have expected names
  # warning() will give the warning message when the function is run
  if(all.equal(uogn.originalnames, names(uogn)) != TRUE){
    warning('Check column names and rows Urine Only Gram Negative')}
  if(all.equal(uogp.originalnames, names(uogp)) != TRUE){
    warning('Check column names and rows Urine Only Gram Positive')}
  if(all.equal(nugn.originalnames, names(nugn)) != TRUE){
    warning('Check column names and rows Non-Urine Gram Negative')}
  if(all.equal(nugp.originalnames, names(nugp)) != TRUE){
    warning('Check column names and rows Non-Urine Gram Positive')}
  
  
  # add desired names
  names(uogn) <- uogn.names
  names(uogp) <- uogp.names
  names(nugn) <- nugn.names
  names(nugp) <- nugp.names
  
  
  # we want the data to be in long format, not wide. so instead of having the antibiotics
  # as columns, we want a variable that has the antibiotics, and each row to only have one
  # result or set of results. this makes it easier to summarize and compare.
  
  uogn.long <- uogn %>%
    filter(!is.na(`Gram Negative Organisms`)) %>%
    gather(key, value, -`Gram Negative Organisms`, -`Total Number of Isolates`) %>%
    separate(key, into = c("Antibiotic", "Measure"), sep = "_") %>%
    spread(Measure, value) %>%
    filter(!`T` %in% c("0", "NT", "n/a"),
           !is.na(`T`),
           !is.na(`%`)) %>%
    mutate(Year = year,
           Hospital = hospital_name,
           Organism_Type = "Gram Negative Organisms",
           Organism = `Gram Negative Organisms`,
           Urine = "Urine Only",
           `Total Number of Isolates` = as.numeric(`Total Number of Isolates`),
           Total = as.numeric(`T`),
           Percent = as.numeric(`%`)/100,
           Susceptible = round(Total * Percent)) %>%
    select(Year, Urine, Organism_Type, Organism, Antibiotic, Hospital, 
           `Total Number of Isolates`, Susceptible, Total, Percent)
  
  uogp.long <- uogp %>%
    filter(!is.na(`Gram Positive Organisms`)) %>%
    gather(key, value, -`Gram Positive Organisms`, -`Total Number of Isolates`) %>%
    separate(key, into = c("Antibiotic", "Measure"), sep = "_") %>%
    spread(Measure, value) %>%
    filter(!`T` %in% c("0", "NT", "n/a"),
           !is.na(`T`),
           !is.na(`%`)) %>%
    mutate(Year = year,
           Hospital = hospital_name,
           Organism_Type = "Gram Positive Organisms",
           Organism = `Gram Positive Organisms`,
           Urine = "Urine Only",
           `Total Number of Isolates` = as.numeric(`Total Number of Isolates`),
           Total = as.numeric(`T`),
           Percent = as.numeric(`%`)/100,
           Susceptible = round(Total * Percent)) %>%
    select(Year, Urine, Organism_Type, Organism, Antibiotic, Hospital, 
           `Total Number of Isolates`, Susceptible, Total, Percent)
  
  nugp.long <- nugp %>%
    filter(!is.na(`Gram Positive Organisms`)) %>%
    gather(key, value, -`Gram Positive Organisms`, -`Total Number of Isolates`) %>%
    separate(key, into = c("Antibiotic", "Measure"), sep = "_") %>%
    spread(Measure, value) %>%
    filter(!`T` %in% c("0", "NT", "n/a"),
           !is.na(`T`),
           !is.na(`%`)) %>%
    mutate(Year = year,
           Hospital = hospital_name,
           Organism_Type = "Gram Positive Organisms",
           Organism = `Gram Positive Organisms`,
           Urine = "Non-Urine",
           `Total Number of Isolates` = as.numeric(`Total Number of Isolates`),
           Total = as.numeric(`T`),
           Percent = as.numeric(`%`)/100,
           Susceptible = round(Total * Percent)) %>%
    select(Year, Urine, Organism_Type, Organism, Antibiotic, Hospital, 
           `Total Number of Isolates`, Susceptible, Total, Percent)
  
  nugn.long <- nugn %>%
    filter(!is.na(`Gram Negative Organisms`)) %>%
    gather(key, value, -`Gram Negative Organisms`, -`Total Number of Isolates`) %>%
    separate(key, into = c("Antibiotic", "Measure"), sep = "_") %>%
    spread(Measure, value) %>%
    filter(!`T` %in% c("0", "NT", "n/a"),
           !is.na(`T`),
           !is.na(`%`)) %>%
    mutate(Year = year,
           Hospital = hospital_name,
           Organism_Type = "Gram Negative Organisms",
           Organism = `Gram Negative Organisms`,
           Urine = "Non-Urine",
           `Total Number of Isolates` = as.numeric(`Total Number of Isolates`),
           Total = as.numeric(`T`),
           Percent = as.numeric(`%`)/100,
           Susceptible = round(Total * Percent)) %>%
    select(Year, Urine, Organism_Type, Organism, Antibiotic, Hospital, 
           `Total Number of Isolates`, Susceptible, Total, Percent)
  
  # finally combine the 4 data sets for output.
  data <- rbind(uogn.long, uogp.long, nugp.long, nugn.long)
}


