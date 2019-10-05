# libraries -----------------------------------------------

library(tidyverse)
load("algorithm.Rdata")

# run algorithm --------------------------------------------

results <- as.list(paste0("set_", 1:50, ".csv")) %>% lapply(., function(x){
  df <- read.csv2(x, fileEncoding = "UTF-8", colClasses = "character")
  return(detect_data_type(df))
}
)

# combine results into data frame with the column name
matches_df <- data.frame()
for(i in seq_along(results)) {
  for(j in seq_along(results[[i]][["results"]][["variable"]])) {
    matches_df[i, as.character(results[[i]][["results"]][["variable"]][[j]])] <- results[[i]][["results"]][["match"]][[j]]
  }
  #fetch runtime
  matches_df[i, "runtime"] <- results[[i]][["runtime"]]
}

# load previously created plan of synthetic data creation
original <- read.csv2("overview.csv")

# check whether personal data created was also detected

combined <- cbind(original, matches_df) %>% 
  mutate(
    first_name_found = ifelse(name_format =="separate", 
                              ifelse("firstname" %in% names(.) & firstname == "first_names", TRUE, FALSE), 
                              NA),
    last_name_found = ifelse(name_format =="separate", 
                             ifelse("lastname" %in% names(.) & lastname == "last_names", TRUE, FALSE), 
                             NA),
    full_name_found = ifelse(name_format %in% c("first_last", "last_first"), 
                             ifelse("name" %in% names(.) & name %in% c("first_last", "last_first"), TRUE, FALSE), 
                             NA),
    street_found = ifelse(address_format =="all_separate", 
                          ifelse("street" %in% names(.) & street == "streetnames", TRUE, FALSE), 
                          NA),
    city_found = ifelse(address_format %in% c("all_separate", "streetname_no"), 
                        ifelse("city" %in% names(.) & city == "cities", TRUE, FALSE), 
                        NA),
    street_no_found = ifelse(address_format == "streetname_no", 
                             ifelse("street_no" %in% names(.) & street_no == "streetnames_no", TRUE, FALSE), 
                             NA),
    postcode_found = ifelse(address_format %in% c("all_separate", "streetname_no"), 
                            ifelse("postcode" %in% names(.) & postcode == "postcode", TRUE, FALSE), 
                            NA),
    address_found = ifelse(address_format == "one_string", 
                           ifelse("address" %in% names(.) & address == "address", TRUE, FALSE), 
                           NA),
    date_found = ifelse("date" %in% names(.) & date == "date", TRUE, FALSE),
    IP_found = ifelse(IP_info == TRUE, 
                      ifelse("IP" %in% names(.) & IP == "IP", TRUE, FALSE), 
                      NA),
    email_found = ifelse(email_info == TRUE, 
                         ifelse("email" %in% names(.) & email == "email", TRUE, FALSE), 
                         NA),
    IBAN_found = ifelse(IBAN_info == TRUE, 
                        ifelse("IBAN" %in% names(.) & IBAN == "IBAN", TRUE, FALSE), 
                        NA),
    credit_found = ifelse(credit_info == TRUE, 
                          ifelse("credit" %in% names(.) & credit == "creditcard", TRUE, FALSE), 
                          NA),
    taxID_found = ifelse(taxID_info == TRUE, 
                         ifelse("taxID" %in% names(.) & taxID == "taxID", TRUE, FALSE), 
                         NA),
    phone_found = ifelse(phone_info == TRUE, 
                         ifelse("phone" %in% names(.) & phone == "phone", TRUE, FALSE), 
                         NA),
    passport_found = ifelse(passport_info == TRUE, 
                            ifelse("passport" %in% names(.) & passport == "passport", TRUE, FALSE), 
                            NA),
    licenseplate_found = ifelse(licenseplate_info == TRUE, 
                                ifelse("licenseplate" %in% names(.) & licenseplate == "licenseplate", TRUE, FALSE), 
                                NA),
    sex_found = ifelse(sex_info == TRUE, 
                       ifelse("sex" %in% names(.) & sex == "sex", TRUE, FALSE), 
                       NA),
    politics_found = ifelse(politics_info == TRUE, 
                            ifelse("politics" %in% names(.) & politics == "politics", TRUE, FALSE), 
                            NA),
    health_found = ifelse(health_info == TRUE, 
                          ifelse("health" %in% names(.) & health == "health", TRUE, FALSE), 
                          NA),
    name_in_mix_found = ifelse(mix_info == TRUE, 
                               ifelse("mix" %in% names(.) & mix %in% c("first_last","last_first"), TRUE, FALSE), 
                               NA)
  )

tool_recall <- combined %>% select(ends_with("_found")) %>% map_dbl(., mean, na.rm = TRUE)
tool_recall

write.csv2(combined, file = "results.csv", row.names = FALSE)
