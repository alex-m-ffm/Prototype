# library(tidyverse)

# load files containing full algorithm as function
# load("algorithm.RData")

# filename <- "set_9.csv"

# test <- read.csv(filename, sep = ",", header = TRUE,
#                  na.strings = "", strip.white = TRUE, colClasses = "character", fileEncoding = "UTF-8")

# test <- read.csv(filename, sep = ";", header = TRUE,
#                  na.strings = "", strip.white = TRUE, colClasses = "character", fileEncoding = "UTF-8")

detect_data_type <- function(df) {
  #measure time
  start_time <- Sys.time()

# load files containing dictionaries and regexes, topic models
load("dictionaries.RData")
load("topic_models.RData")

  #store column names of the original dataframe
  original_vars <- names(df)

  #calculate the number of provided attributes per entry
  no_attributes <- rowSums(!is.na(df))

  #create dataframe containing the success of all checks for all variables
  #the check should always be done only on those cells that are not NA, as matching NA against a dictionary or RegEx will evaluate as FALSE!!
checks <- data.frame(
  cities = map_dbl(df, function(df) mean(df[!is.na(df)] %in% cities, na.rm = TRUE)),
  first_names = map_dbl(df, function(df) mean(df[!is.na(df)] %in% first_names, na.rm = TRUE)),
  last_names = map_dbl(df, function(df) mean(df[!is.na(df)] %in% last_names, na.rm = TRUE)),
  first_last = map_dbl(df, function(df) {
    mean((str_split_fixed(df[!is.na(df)], " ", 2)[,1] %in% first_names) & ((str_split_fixed(df[!is.na(df)], " ", 2)[,2] %in% last_names)), na.rm = TRUE)
  }),
  last_first = map_dbl(df, function(df) {
    mean((str_split_fixed(df[!is.na(df)], ", ", 2)[,2] %in% first_names) & ((str_split_fixed(df[!is.na(df)], ", ", 2)[,1] %in% last_names)), na.rm = TRUE)
  }),
  streetnames = map_dbl(df, function(df) mean(grepl(regex_streetnames, df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE)),
  streetnames_no = map_dbl(df, function(df) mean(grepl(regex_streetnames_no, df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE)),
  address = map_dbl(df, function(df) mean(grepl(regex_address, df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE)),
  postcode = map_dbl(df, function(df) mean(grepl("^\\d{5}$", df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE)),
  date = map_dbl(df, function(df) mean(grepl(regex_date, df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE)),
  email = map_dbl(df, function(df) mean(grepl(regex_email, df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE)),
  IP = map_dbl(df, function(df) mean(grepl(regex_IP, df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE)),
  creditcard = map_dbl(df, function(df) mean(grepl(regex_creditcard, df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE)),
  IBAN = map_dbl(df, function(df) mean(grepl(regex_IBAN, df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE)),
  taxID = map_dbl(df, function(df) mean(grepl(regex_taxID, df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE)),
  phone = map_dbl(df, function(df) mean(grepl(regex_phone, df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE)),
  passport = map_dbl(df, function(df) mean(grepl(regex_passport, df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE)),
  licenseplate = map_dbl(df, function(df) mean(grepl(regex_licenseplate, df[!is.na(df)], ignore.case = TRUE), na.rm = TRUE))
)

detected <- vector("character", 0)
matches <- vector("character", 0)

  # Loop: Start adding dummy variables for the variables with the most successful checks and remove them from the list
  # of variables and checks still to be assigned until no check is successful anymore or the list of columns is empty.

while (nrow(checks)>0) {
  if (max(checks, na.rm = TRUE)>0.3) {
    # find check and variable with the highest success
    max_indices <- which(checks == max(checks, na.rm = TRUE), arr.ind = TRUE)
    # in case of ties use only one of them
    var <- rownames(max_indices)[1]
    top_check <- colnames(checks)[max_indices[1,2]]

    detected <- append(detected, var)
    matches <- append(matches, top_check)

    ## remove the variable from the checks dataframe until no sufficiently successful test is left
    checks <- checks[-max_indices[1], ]
    rm(var, top_check, max_indices)
  } else break
}

findings <- data.frame(variable = detected,
                       match = matches, stringsAsFactors = FALSE)

# rm(var, top_check, max_indices, detected, matches)
rm(detected, matches)

  # variables not identified yet now passed to the topic model ----------------------------------------------------
  remaining_columns <- setdiff(original_vars, findings$variable)

  # check for text
  # check_text <- test[remaining_columns] %>%
  #   map_dbl(., function(df) sum(grepl("[a-z]{2,}", df, ignore.case = TRUE))) %>%
  #   enframe(name = "variable", value = "text_found")

  # check for terms that are also part of our topic model
  check_text <- df[remaining_columns] %>%
    map_dbl(., function(df) sum(tolower(df) %in% sources_terms$term)) %>%
    enframe(name = "variable", value = "text_found") %>%
    filter(text_found > 0) %>% #filter out the columns without any matches
    .$variable # just get the column names

  text_detected <- vector("character", 0)
  topic_matches <- vector("character", 0)

  for (column in check_text) {


    # topic_matches <- df[column] %>% rename(., text = names(.)) %>%
    #   mutate(text = tolower(text)) %>%
    #   left_join(., betas_wide, by = c("text" = "term")) %>%
    #   summarise(
    #     beta_1 = mean(`1`, na.rm = TRUE),
    #     beta_2 = mean(`2`, na.rm = TRUE),
    #     beta_3 = mean(`3`, na.rm = TRUE)
    #   ) %>% which.max() %>% enframe(name = "column", value = "top_topic") %>%
    #   left_join(., topic_IDs, by = c("top_topic" = "topic")) %>%
    #   .$title %>% as.character() %>% append(topic_matches, .)
    
    topic_check <- df[column] %>% rename(., text = names(.)) %>%
      mutate(text = tolower(text)) %>%
      left_join(., betas_wide, by = c("text" = "term")) %>%
      summarise(
        beta_1 = mean(`1`, na.rm = TRUE),
        beta_2 = mean(`2`, na.rm = TRUE),
        beta_3 = mean(`3`, na.rm = TRUE)
      )
    # if the mean beta probabilities are too low delete the column
    if(topic_check$beta_1 < thresholds$threshold[1]){
      topic_check$beta_1 <- NULL
    }
    if(topic_check$beta_2 < thresholds$threshold[2]){
      topic_check$beta_2 <- NULL
    }
    if(topic_check$beta_3 < thresholds$threshold[3]){
      topic_check$beta_3 <- NULL
    }
    # if anything remains, determine the maximum value and retrieve the topic name from the topic number
    if(exists("topic_check") & length(topic_check)>0){
      text_detected <- append(text_detected, column)
      topic_matches <- topic_check %>% which.max() %>% enframe(name = "column", value = "top_topic") %>%
        left_join(., topic_IDs, by = c("top_topic" = "topic")) %>%
        .$title %>% as.character() %>% append(topic_matches, .)
    }
  }

  model_outcome <- data.frame(variable = text_detected,
                              match = topic_matches, stringsAsFactors = FALSE)

  report <- rbind(findings, model_outcome)
  
  #measure runtime
  runtime <- as.numeric(Sys.time() - start_time)
  
  return(list(results = report, 
              runtime = runtime))
}

# detect_data_type(test)