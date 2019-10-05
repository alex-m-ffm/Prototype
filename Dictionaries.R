# load libraries --------------------------------------------------------------------------
library(stringr)
library(tidyverse)
library(readxl)
library(rvest)

# Webscrape list of last_names from the digital family dictionary of Germany http://www.namenforschung.net/dfd/woerterbuch/liste/ -----------------------------------

last_names <- read_html("http://www.namenforschung.net/dfd/woerterbuch/gesamtliste-veroeffentlichter-namenartikel/") %>% 
  html_nodes("#maincontent a") %>%
  html_text() %>% 
  .[. != ""]

#Download list of first names from Düsseldorf

boys_2018 <- read_csv2("https://opendata.duesseldorf.de/sites/default/files/Vornamen_Jungen_2018.csv")
boys_2008 <- read_csv2("https://opendata.duesseldorf.de/sites/default/files/Vornamen_Jungen_2008.csv")
girls_2018 <- read_csv2("https://opendata.duesseldorf.de/sites/default/files/Vornamen_Mädchen_2018.csv")
girls_2008 <- read_csv2("https://opendata.duesseldorf.de/sites/default/files/Vornamen_Mädchen_2008.csv")

# get some older names!

older_male <- read_html("https://de.wiktionary.org/wiki/Verzeichnis:Deutsch/Namen/die_h%C3%A4ufigsten_m%C3%A4nnlichen_Vornamen_Deutschlands") %>% 
  html_nodes("#mw-content-text li") %>%
  html_text() %>% 
  str_extract(., boundary("word"))

older_female <- read_html("https://de.wiktionary.org/wiki/Verzeichnis:Deutsch/Namen/die_h%C3%A4ufigsten_weiblichen_Vornamen_Deutschlands") %>% 
  html_nodes("#mw-content-text li") %>%
  html_text() %>% 
  str_extract(., boundary("word"))

male <- union(boys_2008$vorname, boys_2018$vorname) %>% 
  union(., older_male) %>% 
  .[!is.na(.)] %>% 
  sort()

female <- union(girls_2008$vorname, girls_2018$vorname) %>% 
  union(., older_female) %>% 
  .[!is.na(.)] %>% 
  sort()

first_names <- union(male, female)

rm(boys_2008, boys_2018, girls_2008, girls_2018, older_female, older_male)

#List of ALL municipalities from Federal Statistical Office
cities <- read.fwf("GV100AD_311217.ASC", c(2,4,2,2,2,1,2,3,4,50,50,2,2,2,11,11,11,4,5,5,2,4,1,1,2,5,3,3,4,20)) %>% 
  filter(!is.na(V8)) %>% 
  select(City = V10) %>% 
  as.matrix(.) %>% 
  str_split(., ", ", simplify = TRUE) %>%
  as.data.frame(.) %>% 
  .[,1] %>% 
  as.character(.) %>% 
  unique(.) %>% trimws(.)

#store some more complicated regular expressions -------------------------------------------------

regex_IP <- paste0("\\b(?:(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])\\.){3}",
"(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])\\b")
regex_creditcard <- paste0("^(?:4[0-9]{12}(?:[0-9]{3})?|[25][1-7][0-9]{14}|6",
"(?:011|5[0-9][0-9])[0-9]{12}|3[47][0-9]{13}|3(?:0[0-5]|[68][0-9])[0-9]{11}|(?:2131|1800|35\\d{3})\\d{11})$")
regex_IBAN <- "DE([0-9a-zA-Z]{20})"
regex_email <- "^[a-zA-Z0-9\\_\\.\\+\\-]+@[a-zA-Z0-9\\-]+\\.[a-zA-Z0-9\\-\\.]+$"
regex_taxID <- "^([0-9]{3}/?[0-9]{4}/?[0-9]{4})$"
regex_passport <- "^[CFGHJKLMNPRTVWXYZ][CFGHJKLMNPRTVWXYZ0-9]{8}$"
regex_phone <- "^((((((00|\\+)49[/ \\-]?)|0)[1-9][0-9]{1,4})[/ \\-]?)|((((00|\\+)49\\()|\\(0)[1-9][0-9]{1,4}\\)[/ \\-]?))([0-9]{1,7}([/ \\-]?[0-9]{1,5})?)$"

regex_licenseplate <- "^[A-ZÄÖÜ]{1,3}([ \\-]|\\- | \\- )[A-Z]{0,2}[ ]?[0-9]{1,4}[H]{0,1}$"

regex_streetnames <- paste0("(.*straße$)|(.*strasse$)|(.*str\\.$)|(.*weg$)|(.*allee$)|(.*platz$)|(.*berg$)",
"|(.*ring$)|(.*wall$)|(.*anlage$)|(.*ufer$)|(.*gasse$)|(.*pfad$)|(.*tor$)|(.*hof$)|(.*kamp$)|(.*feld$)|(.*burg$)")
regex_streetnames_no <- paste0("(.*straße \\d{1,3}[A-Za-z]?$)|(.*strasse \\d{1,3}[A-Za-z]?$)|(.*str\\. \\d{1,3}[A-Za-z]?$)|(.*weg \\d{1,3}[A-Za-z]?$)|(.*allee \\d{1,3}[A-Za-z]?$)", 
                               "|(.*platz \\d{1,3}[A-Za-z]?$)|(.*berg \\d{1,3}[A-Za-z]?$)|(.*ring \\d{1,3}[A-Za-z]?$)|(.*wall \\d{1,3}[A-Za-z]?$)|(.*anlage \\d{1,3}[A-Za-z]?$)|",
                               "(.*ufer \\d{1,3}[A-Za-z]?$)|(.*gasse \\d{1,3}[A-Za-z]?$)|(.*pfad \\d{1,3}[A-Za-z]?$)|(.*tor \\d{1,3}[A-Za-z]?$)|(.*hof \\d{1,3}[A-Za-z]?$)|(.*kamp \\d{1,3}[A-Za-z]?$)|",
                               "(.*feld \\d{1,3}[A-Za-z]?$)|(.*burg \\d{1,3}[A-Za-z]?$)")
regex_address <- paste0("(.*straße \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|(.*strasse \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|(.*str\\. \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|",
                        "(.*weg \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|(.*allee \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|(.*platz \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|",
                        "(.*berg \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|(.*ring \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|(.*wall \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|",
                        "(.*anlage \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|(.*ufer \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|(.*gasse \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|",
                        "(.*pfad \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|(.*tor \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|(.*hof \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|",
                        "(.*kamp \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|(.*feld \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)|(.*burg \\d{1,3}[A-Za-z]?, \\d{5} \\w{3,}$)")

#works with all German date formats
regex_date <- paste0("^\\s*(3[01]|[12][0-9]|0?[1-9])\\.(1[012]\\.|0?[1-9]\\.| Januar | Februar | März | April | Mai |",
" Juni | Juli | August | September | Oktober | November | Dezember | Jan. | Feb. | Apr. |",
" Jun. | Jul. | Aug. | Sept. | Okt. | Nov. | Dez. )((19|20)?\\d{2})\\s*$")

save(cities, first_names, male, female, last_names, regex_IP, regex_creditcard, regex_IBAN, regex_email, regex_streetnames,
     regex_streetnames_no, regex_date, regex_address, regex_taxID, regex_passport, regex_phone, regex_licenseplate,
     file = "dictionaries.RData")