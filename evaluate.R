# libraries -----------------------------------------------

library(tidyverse)
load("algorithm.Rdata")

# run algorithm --------------------------------------------

results <- as.list(paste0("set_", 1:50, ".csv")) %>% lapply(., function(x){
  df <- read.csv2(x, na.strings = "", strip.white = TRUE, 
                  fileEncoding = "UTF-8", colClasses = "character")
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

matches_df <- cbind(file = original$file, matches_df)

# proper accuracy calculation ------------------------------------------------
accuracy <- data.frame()
for (i in seq_along(original$file)) {
  #names 
  #separate
  if(original[["name_format"]][[i]]=="separate") {
    # first names
    if(matches_df[["firstname"]][[i]]=="first_names") {
      accuracy[i,"firstname"] <- "TP"
    } else {
      accuracy[i, "firstname"] <- "FN"
    }
    # last names
    if(matches_df[["lastname"]][[i]]=="last_names") {
      accuracy[i,"lastname"] <- "TP"
    } else {
      accuracy[i, "lastname"] <- "FN"
    }
    # errors in case full names are found - except in case the mix column is there
    if(("first_last" %in% results[[i]][["results"]][["match"]] | "last_first" %in% results[[i]][["results"]][["match"]]) & !("mix" %in% results[[i]][["results"]][["variable"]])){
      accuracy[i, "name"] <- "FP"
    } else {
      accuracy[i, "name"] <- "TN"
    }
    
  } else { #names combined
    if("first_names" %in% results[[i]][["results"]][["match"]]) {
      accuracy[i, "firstname"] <- "FP"
    } else {
      accuracy[i, "firstname"] <- "TN"
    }
    if("last_names" %in% results[[i]][["results"]][["match"]]) {
      accuracy[i, "lastname"] <- "FP"
    } else {
      accuracy[i, "lastname"] <- "TN"
    }
    #case of full names 
    if(matches_df[["name"]][[i]] %in% c("first_last", "last_first")) {
      accuracy[i,"name"] <- "TP"
    } else {
      accuracy[i, "name"] <- "FN"
    }
  }
  #dates always exist
  if(!is.na(matches_df[["date"]][[i]]) & matches_df[["date"]][[i]]=="date") {
    accuracy[i,"date"] <- "TP"
  } else {
    accuracy[i, "date"] <- "FN"
  }
  
  #addresses
  #separate
  if(original[["address_format"]][[i]]=="all_separate") {
    # street names
    if(matches_df[["street"]][[i]]=="streetnames") {
      accuracy[i,"street"] <- "TP"
    } else {
      accuracy[i, "street"] <- "FN"
    }
    # post codes
    if(matches_df[["postcode"]][[i]]=="postcode") {
      accuracy[i,"postcode"] <- "TP"
    } else {
      accuracy[i, "postcode"] <- "FN"
    }
    # cities
    if(matches_df[["city"]][[i]]=="cities") {
      accuracy[i,"city"] <- "TP"
    } else {
      accuracy[i, "city"] <- "FN"
    }
    # wrong outcomes for combined fields
    if("streetnames_no" %in% results[[i]][["results"]][["match"]]) {
      accuracy[i, "street_no"] <- "FP"
    } else {
      accuracy[i, "street_no"] <- "TN"
    }
    if("address" %in% results[[i]][["results"]][["match"]]) {
      accuracy[i, "address"] <- "FP"
    } else {
      accuracy[i, "address"] <- "TN"
    }
  } else {
    if(original[["address_format"]][[i]]=="streetname_no"){
      if(!is.na(matches_df[["street_no"]][[i]]) & matches_df[["street_no"]][[i]]=="streetnames_no") {
        #streetname number combination
        accuracy[i,"street_no"] <- "TP"
      } else {
        accuracy[i, "street_no"] <- "FN"
      }
      # post codes
      if(matches_df[["postcode"]][[i]]=="postcode") {
        accuracy[i,"postcode"] <- "TP"
      } else {
        accuracy[i, "postcode"] <- "FN"
      }
      # cities
      if(matches_df[["city"]][[i]]=="cities") {
        accuracy[i,"city"] <- "TP"
      } else {
        accuracy[i, "city"] <- "FN"
      }
      # full address errors
      if("address" %in% results[[i]][["results"]][["match"]]) {
        accuracy[i, "address"] <- "FP"
      } else {
        accuracy[i, "address"] <- "TN"
      }
      if("streetnames" %in% results[[i]][["results"]][["match"]]) {
        accuracy[i, "street"] <- "FP"
      } else {
        accuracy[i, "street"] <- "TN"
      }
    } else {
      if("streetnames_no" %in% results[[i]][["results"]][["match"]]) {
        accuracy[i, "street_no"] <- "FP"
      } else {
        accuracy[i, "street_no"] <- "TN"
      }
      if("streetnames" %in% results[[i]][["results"]][["match"]]) {
        accuracy[i, "street"] <- "FP"
      } else {
        accuracy[i, "street"] <- "TN"
      }
      if("postcode" %in% results[[i]][["results"]][["match"]]) {
        accuracy[i, "postcode"] <- "FP"
      } else {
        accuracy[i, "postcode"] <- "TN"
      }
      if("cities" %in% results[[i]][["results"]][["match"]]) {
        accuracy[i, "city"] <- "FP"
      } else {
        accuracy[i, "city"] <- "TN"
      }
      # address
      if(matches_df[["address"]][[i]]=="address") {
        accuracy[i,"address"] <- "TP"
      } else {
        accuracy[i, "address"] <- "FN"
      }
    }
  }

  #all other columns except mix
  for (var in c("IP", "IBAN", "email", "creditcard", "sex", 
                "politics", "health", "taxID", "passport", "phone", "licenseplate")){
    info <- paste(var, "info", sep = "_")
    if(original[[info]][[i]]==TRUE){
      if(!is.na(matches_df[[var]][[i]]) & matches_df[[var]][[i]]==var) {
        accuracy[i,var] <- "TP"
      } else {
        accuracy[i, var] <- "FN"
      }
    } else {
      if(var %in% results[[i]][["results"]][["match"]]){
        accuracy[i,var] <- "FP"
      } else {
        accuracy[i, var] <- "TN"
      }
    }
  }
  
  #names in mix columns 
  if(original[["mix_info"]][[i]]==TRUE){
    if(!is.na(matches_df[["mix"]][[i]]) & matches_df[["mix"]][[i]] %in% c("first_last", "last_first")) {
      accuracy[i, "mix"] <- "TP"
    } else {
      accuracy[i, "mix"] <- "FN"
    }
  }
}

accuracy <- cbind(file = original$file, accuracy)

#compute rates

accuracy_metrics <- accuracy %>% 
  pivot_longer(., names(.)[2:length(names(.))], names_to = "variable") %>% 
  group_by(variable, value) %>% 
  tally() %>% 
  filter(!is.na(value)) %>% 
  pivot_wider(., id_cols = "variable", names_from = "value", values_from = "n") %>% 
  mutate(
    TN = ifelse(is.na(TN), 0, TN),
    TP = ifelse(is.na(TP), 0, TP),
    FN = ifelse("FN" %in% names(.), ifelse(!is.na(FN), FN, 0), 0),
    FP = ifelse("FP" %in% names(.), ifelse(!is.na(FF), FP, 0), 0),
    recall = TP / (TP + FN),
    accuracy = (TP + TN)/(TP + TN + FN + FP)
  )
  
chart1 <- accuracy_metrics %>% 
  ggplot(., aes(variable, accuracy)) +
  geom_col() +
  coord_flip()

chart1
ggsave("accuracy.png", width = 8, units = "cm")

chart2 <- accuracy_metrics %>% 
  ggplot(., aes(variable, recall)) +
  geom_col() +
  coord_flip()

chart2
ggsave("recall.png", width = 8, units = "cm")

write.csv2(matches_df, file = "results.csv", row.names = FALSE)
write.csv2(accuracy, file = "accuracy.csv", row.names = FALSE)
