# load libraries -----------------------------------

library(tidyverse)


# load dictionaries --------------------------------

load("dictionaries.RData")

# load list of streetnames -------------------------
streets <- read.delim("strassen_osm.txt", header = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE)

# generate categories of data sets ---------------------

no_sets <- 50
size <- 3000
set.seed(444)

overview <- data.frame(file = paste0("set_", 1:no_sets, ".csv"),
                       name_format = sample(c("separate", "first_last", "last_first"), size = no_sets, replace = TRUE, prob = c(0.5, 0.25, 0.25)),
                       address_format = sample(c("all_separate", "streetname_no", "one_string"), size = no_sets, replace = TRUE, prob = c(0.5, 0.25, 0.25)),
                       date_format = sample(c("dd.mm.yyyy", "dd.mm.yy", "dd. mmm. yyyy", "dd. mmmm yyyy"), size = no_sets, replace = TRUE, prob = c(0.5, 0.25, 0.125, 0.125)),
                       IP_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.1, 0.9)),
                       IBAN_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.1, 0.9)),
                       email_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.6, 0.4)),
                       creditcard_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.1, 0.9)),
                       sex_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.3, 0.7)),
                       politics_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.3, 0.7)),
                       health_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.4, 0.6)),
                       taxID_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.3, 0.7)),
                       passport_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.3, 0.7)),
                       phone_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.3, 0.7)),
                       licenseplate_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.3, 0.7)),
                       
                       # add some non-personal data
                       sales_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.3, 0.7)),
                       clothing_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.3, 0.7)),
                       value_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.3, 0.7)),
                       
                       # add mixed data columns 
                       mix_info = sample(c(TRUE, FALSE), size = no_sets, replace = TRUE, prob = c(0.3, 0.7)),
                       
                       stringsAsFactors = FALSE
) %>% 
  mutate(
    # make share of empty cells variable and record it
    names_na = pmin(pmax(rnorm(no_sets, 0.3, 0.2),0), 0.9),
    address_na = pmin(pmax(rnorm(no_sets, 0.3, 0.2),0), 0.9),
    date_na = pmin(pmax(rnorm(no_sets, 0.5, 0.2),0), 0.9),
    IP_na = ifelse(IP_info== TRUE, pmin(pmax(rnorm(no_sets, 0.5, 0.2),0), 0.9), NA),
    IBAN_na = ifelse(IBAN_info== TRUE, pmin(pmax(rnorm(no_sets, 0.5, 0.2),0), 0.9), NA),
    email_na = ifelse(email_info== TRUE, pmin(pmax(rnorm(no_sets, 0.5, 0.2),0), 0.9), NA),
    creditcard_na = ifelse(creditcard_info== TRUE, pmin(pmax(rnorm(no_sets, 0.5, 0.2),0), 0.9), NA),
    sex_na = ifelse(sex_info== TRUE, pmin(pmax(rnorm(no_sets, 0.5, 0.2),0), 0.9), NA),
    politics_na = ifelse(politics_info== TRUE, pmin(pmax(rnorm(no_sets, 0.5, 0.2),0), 0.9), NA),
    health_na = ifelse(health_info== TRUE, pmin(pmax(rnorm(no_sets, 0.5, 0.2),0), 0.9), NA),
    taxID_na = ifelse(taxID_info== TRUE, pmin(pmax(rnorm(no_sets, 0.5, 0.2),0), 0.9), NA),
    passport_na = ifelse(passport_info== TRUE, pmin(pmax(rnorm(no_sets, 0.5, 0.2),0), 0.9), NA),
    phone_na = ifelse(phone_info== TRUE, pmin(pmax(rnorm(no_sets, 0.5, 0.2),0), 0.9), NA),
    licenseplate_na = ifelse(licenseplate_info== TRUE, pmin(pmax(rnorm(no_sets, 0.5, 0.2),0), 0.9), NA),
    mix_na = ifelse(mix_info== TRUE, pmin(pmax(rnorm(no_sets, 0.3, 0.2),0), 0.9), NA)
  )
                       
load("dictionaries.RData")

# create synthetic datasets ----------------------------------------------------------------------

for (i in seq_along(overview[["file"]])){
  # initialise data frame and create names
  if (overview[["name_format"]][[i]]=="separate") {
    df = data.frame(lastname = sample(last_names, size = size, replace = TRUE),
                    firstname = sample(first_names, size = size, replace = TRUE))
    df[sample(1:size, round(size * overview[["names_na"]][[i]])), "firstname"] <- NA
  } else {
    if (overview[["name_format"]][[i]]=="first_last") {
      df = data.frame(name = paste(sample(first_names, size = size, replace = TRUE), 
                                   sample(last_names, size = size, replace = TRUE), sep = " "))
    } else {
      df = data.frame(name = paste(sample(last_names, size = size, replace = TRUE), 
                                   sample(first_names, size = size, replace = TRUE), sep = ", "))
    }
  }
  # add address data
  if (overview[["address_format"]][[i]]=="all_separate") {
    df$street  <-  sample(streets[[1]], size = size, replace = TRUE)
    df$house_no <-  sample(c(1:200, NA), size = size, replace = TRUE)
    df$postcode  <-  paste0(sample(0:9, size = size, replace = TRUE),
             sample(0:9, size = size, replace = TRUE),
             sample(0:9, size = size, replace = TRUE),
             sample(0:9, size = size, replace = TRUE),
             sample(0:9, size = size, replace = TRUE))
    df$city  <-  sample(cities, size = size, replace = TRUE)
    # set some cells NA
    df[sample(1:size, round(size*overview[["address_na"]][[i]]/4)), c("house_no", "postcode")] <- NA
    df[sample(1:size, round(size*overview[["address_na"]][[i]]/2)), c("house_no", "street")] <- NA
    df[sample(1:size, round(size*overview[["address_na"]][[i]]/4)), c("house_no", "street", "postcode", "city")] <- NA
  } else {
    if (overview[["address_format"]][[i]]=="streetname_no") {
      df$street_no  <-  paste(sample(streets[[1]], size = size, replace = TRUE),
                              sample(1:200, size = size, replace = TRUE), sep = " ")
      df$postcode  <-  paste0(sample(0:9, size = size, replace = TRUE),
               sample(0:9, size = size, replace = TRUE),
               sample(0:9, size = size, replace = TRUE),
               sample(0:9, size = size, replace = TRUE),
               sample(0:9, size = size, replace = TRUE))
      df$city  <-  sample(cities, size = size, replace = TRUE)
      # set some cells NA
      df[sample(1:size, round(size*overview[["address_na"]][[i]]/4)), c("street_no", "postcode")] <- NA
      df[sample(1:size, round(size*overview[["address_na"]][[i]]/2)), c("street_no")] <- NA
      df[sample(1:size, round(size*overview[["address_na"]][[i]]/4)), c("street_no", "postcode", "city")] <- NA
    } else {
      df$address <- paste(
        paste(sample(streets[[1]], size = size, replace = TRUE),
              sample(1:200, size = size, replace = TRUE), sep = " "),
        paste(
          paste0(sample(0:9, size = size, replace = TRUE),
                 sample(0:9, size = size, replace = TRUE),
                 sample(0:9, size = size, replace = TRUE),
                 sample(0:9, size = size, replace = TRUE),
                 sample(0:9, size = size, replace = TRUE)),
          sample(cities, size = size, replace = TRUE), sep = " "),
        sep = ", ") %>% trimws(.)
      # set some cells NA
      df[sample(1:size, round(size*overview[["address_na"]][[i]])), "address"] <- NA
      }
  }
  
  # add dates 
  if (overview[["date_format"]][[i]]=="dd.mm.yyyy"){
    df$date <- paste(sample(1:31, size = size, replace = TRUE),
                     sample(1:12, size = size, replace = TRUE),
                     sample(1970:2019, size = size, replace = TRUE), sep = ".")
  } else {
    if (overview[["date_format"]][[i]]=="dd.mm.yy"){
      df$date <- paste(sample(1:31, size = size, replace = TRUE),
                       sample(1:12, size = size, replace = TRUE),
                       str_pad(sample(c(70:99,0:19), size = size, replace = TRUE),2, side = "left", pad = "0"), sep = ".")
    } else {
      if (overview[["date_format"]][[i]]=="dd. mmmm yyyy"){
        df$date <- paste(
          paste(sample(1:31, size = size, replace = TRUE),
                         sample(c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September",
                         "Oktober", "November", "Dezember"), size = size, replace = TRUE), sep = ". "),
                         sample(1970:2019, size = size, replace = TRUE), sep = " ")
      } else {
        df$date <- paste(
          paste(sample(1:31, size = size, replace = TRUE),
                sample(c("Jan.", "Feb.", "März", "Apr.", "Mai", "Jun.", "Jul.", "Aug.", "Sept.",
                         "Okt.", "Nov.", "Dez."), size = size, replace = TRUE), sep = ". "),
          sample(1970:2019, size = size, replace = TRUE), sep = " ")
      }
    }
  }
  # set some cells NA
  df[sample(1:size, round(size * overview[["date_na"]][[i]])), "date"] <- NA

  # add IP
  if (overview[["IP_info"]][[i]]==TRUE){
    df$IP <- paste(
      sample(0:255, size = size, replace = TRUE),
      sample(0:255, size = size, replace = TRUE),
      sample(0:255, size = size, replace = TRUE),
      sample(0:255, size = size, replace = TRUE), sep = "."
    )
    # set some cells NA
    df[sample(1:size, round(size* overview[["IP_na"]][[i]])), "IP"] <- NA
  }

  
  # add IBAN
  if (overview[["IBAN_info"]][[i]]==TRUE){
    df$IBAN <- paste0("DE",
      str_pad(sample(0:99, size = size, replace = TRUE), 2, side = "left", pad = "0"),
      str_pad(sample(1:9999999, size = size, replace = TRUE), 8, side = "left", pad = "0"),
      str_pad(sample(1:99999999, size = size, replace = TRUE), 10, side = "left", pad = "0")
    )
    # set some cells NA
    df[sample(1:size, round(size * overview[["IBAN_na"]][[i]])), "IBAN"] <- NA
  }
  
  # add credit card
  if (overview[["creditcard_info"]][[i]]==TRUE){
    df$creditcard <- paste0(str_pad(sample(4000:4999, size = size, replace = TRUE), 4, side = "left", pad = "0"),
      str_pad(sample(1:9999, size = size, replace = TRUE), 4, side = "left", pad = "0"),
      str_pad(sample(1:9999, size = size, replace = TRUE), 4, side = "left", pad = "0"),
      str_pad(sample(1:9999, size = size, replace = TRUE), 4, side = "left", pad = "0"))
    # set some cells NA
    df[sample(1:size, round(size * overview[["creditcard_na"]][[i]])), "creditcard"] <- NA
  }

  # add e-mail info
  if (overview[["email_info"]][[i]]==TRUE){
    if (overview[["name_format"]][[i]]=="separate"){
      df$email <- paste(
        paste(df$firstname, df$lastname, sep = "."),
        sample(c("hotmail.com", "web.de", "gmail.com", "gmx.de"), size = size, replace = TRUE), sep = "@")
    } else {
      if (overview[["name_format"]][[i]]=="first_last"){
        df$email <- paste(gsub(" ", ".", df$name),
          sample(c("hotmail.com", "web.de", "gmail.com", "gmx.de"), size = size, replace = TRUE), sep = "@")
      } else {
        if (overview[["name_format"]][[i]]=="last_first"){
          df$email <- paste(gsub(", ", ".", df$name),
                            sample(c("hotmail.com", "web.de", "gmail.com", "gmx.de"), size = size, replace = TRUE), sep = "@")
        }
      }
    }
    # set some cells NA
    df[sample(1:size, round(size * overview[["email_na"]][[i]])), "email"] <- NA
  }

  
  # add info on sexual preferences
  if (overview[["sex_info"]][[i]]==TRUE){
    df$sex <- sample(c("heterosexuell", "homosexuell", "bisexuell"), 
                     size = size, replace = TRUE, prob = c(0.85, 0.1, 0.05))
    df[sample(1:size, round(size * overview[["sex_na"]][[i]])), "sex"] <- NA
  }
  
  # add info on political preferences
  if (overview[["politics_info"]][[i]]==TRUE){
    df$politics <- sample(c("konservativ", "liberal", "links", "rechts", "grün-alternativ"), 
                     size = size, replace = TRUE, prob = c(0.5, 0.1, 0.2, 0.05, 0.15))
    df[sample(1:size, round(size * overview[["politics_na"]][[i]])), "politics"] <- NA
  }
  
  # add info on health
  if (overview[["health_info"]][[i]]==TRUE){
    df$health <- sample(c("Krebs", "Diabetes", "Rheuma", "Demenz", "Multiple Sklerose", "Bluthochdruck"), 
                          size = size, replace = TRUE)
    df[sample(1:size, round(size * overview[["health_na"]][[i]])), "health"] <- NA
  }
  
  # add tax ID
  if (overview[["taxID_info"]][[i]]==TRUE){
    dash <- sample(c("/",""), size = size, replace = TRUE)
    df$taxID <- paste0(str_pad(sample(1:999, size = size, replace = TRUE), 3, side = "left", pad = "0"),
                       dash,
                        str_pad(sample(1:9999, size = size, replace = TRUE), 4, side = "left", pad = "0"),
                       dash,
                        str_pad(sample(1:9999, size = size, replace = TRUE), 4, side = "left", pad = "0"))
    # set some cells NA
    df[sample(1:size, round(size * overview[["taxID_na"]][[i]])), "taxID"] <- NA
    rm(dash)
  }
  
  # add passport/ID numbers
  if (overview[["passport_info"]][[i]]==TRUE){
    passport_letters <- c("C","F","G","H","J","K","L","M","N","P","R","T","V","W","X","Y","Z")
    passport_characters <- c(passport_letters, 0:9)
    df$passport <- paste0(sample(passport_letters, size = size, replace = TRUE),
                          sample(passport_characters, size = size, replace = TRUE),
                          sample(passport_characters, size = size, replace = TRUE),
                          sample(passport_characters, size = size, replace = TRUE),
                          sample(passport_characters, size = size, replace = TRUE),
                          sample(passport_characters, size = size, replace = TRUE),
                          sample(passport_characters, size = size, replace = TRUE),
                          sample(passport_characters, size = size, replace = TRUE),
                          sample(passport_characters, size = size, replace = TRUE))
    # set some cells NA
    df[sample(1:size, round(size * overview[["passport_na"]][[i]])), "passport"] <- NA
    rm(passport_characters, passport_letters)
  }
  
  # license plates
  if (overview[["licenseplate_info"]][[i]]==TRUE){
    license_cities <- read.csv2("kennzeichen.csv", header = FALSE, stringsAsFactors = FALSE)[,1] %>% 
      grep("[^ ]", ., value = TRUE)
    df$licenseplate <- paste0(sample(license_cities, size = size, replace = TRUE),
                              "-",
                              sample(LETTERS, size = size, replace = TRUE),
                              sample(LETTERS, size = size, replace = TRUE),
                              str_pad(sample(0:999, size = size, replace = TRUE), 3, side = "left", pad = "0"))
    df[sample(1:size, round(size * overview[["licenseplate_na"]][[i]])), "licenseplate"] <- NA
    rm(license_cities)
  }
  
  # phone number
  if (overview[["phone_info"]][[i]]==TRUE){
    df$phone <- paste0(sample(c("+49 ", "0"), size = size, replace = TRUE),
                       sample(10:999, size = size, replace = TRUE),
                       sample(c(" ", "-"), size = size, replace = TRUE),
                       sample(10000:9999999, size = size, replace = TRUE)
                      )
    df[sample(1:size, round(size * overview[["phone_na"]][[i]])), "phone"] <- NA
  }
  
  # non-personal data --------------------------------------------------------
  if (overview[["sales_info"]][[i]]==TRUE){
    df$sales <- sample(0:200, 
                        size = size, replace = TRUE)
  }
  if (overview[["clothing_info"]][[i]]==TRUE){
    df$clothing <- sample(c("XS","S","M","L","XL","XXL"), 
                       size = size, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.3, .075, .025))
  }
  if (overview[["value_info"]][[i]]==TRUE){
    df$value <- sample(0:1000000, 
                       size = size, replace = TRUE)
  }
  #write the number of columns to the overview table
  overview[["no_col"]][[i]] <- ncol(df)
  
  # mix of names and companies -----------------------------------------------
  
  if (overview[["mix_info"]][[i]]==TRUE){
    companies <- read.csv2("companies.csv", header = FALSE, stringsAsFactors = FALSE)[[1]]
    mix_all <- c(paste(sample(first_names, size = size, replace = TRUE), 
                           sample(last_names, size = size, replace = TRUE), sep = " "),
                     paste(sample(last_names, size = size, replace = TRUE), 
                           sample(first_names, size = size, replace = TRUE), sep = ", "),
                 sample(companies, size = size, replace = TRUE))
    
    df$mix <- sample(mix_all, 
                       size = size, replace = TRUE)
    df[sample(1:size, round(size * overview[["mix_na"]][[i]])), "mix"] <- NA
    rm(mix_all)
  }

  write.csv2(df, file = overview[["file"]][[i]], row.names = FALSE, na = "", fileEncoding = "UTF-8")
}


write.csv2(overview, file = "overview.csv", row.names = FALSE)