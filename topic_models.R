# load necessary libraries -------------------------------------------------------------------
library(stringr)
library(tidyverse)
library(readxl)
library(rvest)
library(gutenbergr)
library(topicmodels)
library(tidytext)
library(SnowballC)
library(tm)
library(ggplot2)

# define function to read text from wikipedia pages ------------------------------------------
read_wiki_paras <- function(url) {
  read_html(url) %>% 
    html_nodes("p") %>% # use only text paragraphs
    html_text %>% 
    paste(., collapse = " \n ") # collapse into single string
}

# collect wikipedia pages for the three topics -----------------------------------------------

wiki_urls_sex <- paste0("https://de.wikipedia.org/wiki/", 
                        c("Sexuelle_Orientierung", "Sexualpr%C3%A4ferenz", "Sexualit%C3%A4t",
                   "Geschlechtsidentit%C3%A4t", "Heterosexualit%C3%A4t", "Homosexualit%C3%A4t",
                   "Sexualkontakt", "Erotik", "Bisexualit%C3%A4t", "Polysexualit%C3%A4t",
                   "Pansexualit%C3%A4t", "Intersexualit%C3%A4t", "Transgender", "Transsexualit%C3%A4t",
                   "Transvestitismus", "Schwul", "Lesbisch", "Asexualit%C3%A4t"))

wiki_urls_politics <- paste0("https://de.wikipedia.org/wiki/",
                             c("Politische_Ideologie",  "Konservatismus", "Sozialismus", "Liberalismus",
                        "Rechtspopulismus", "Wirtschaftsethik", "Nachhaltigkeit", "Geschichte_der_Parteien_in_Deutschland",
                        "B%C3%BCndnis_90/Die_Gr%C3%BCnen", "Sozialdemokratische_Partei_Deutschlands",
                        "Christlich_Demokratische_Union_Deutschlands",  "Freie_Demokratische_Partei",
                        "Alternative_f%C3%BCr_Deutschland", "Die_Linke",  "Piratenpartei",
                        "Nationaldemokratische_Partei_Deutschlands", "Die_Republikaner",
                        "Partei_f%C3%BCr_Arbeit,_Rechtsstaat,_Tierschutz,_Elitenf%C3%B6rderung_und_basisdemokratische_Initiative",
                        "Westdeutsche_Studentenbewegung_der_1960er_Jahre", "Wende_und_friedliche_Revolution_in_der_DDR",
                        "Bundestagswahl_2017",  "Bundestagswahl_2013",  "Bundestagswahl_2009",  "Bundestagswahl_2005",
                        "Bundestagswahl_2002",  "Bundestagswahl_1998",  "Pegida"))

wiki_urls_health <- paste0("https://de.wikipedia.org/wiki/",
                           c("Krankheit", "Medizintheorie",
                      "Internationale_statistische_Klassifikation_der_Krankheiten_und_verwandter_Gesundheitsprobleme",
                      "Medizin",  "Trauma_(Medizin)", "Therapie", "Behinderung", "Diagnose", "Krankheitspr%C3%A4vention",
                      "Heilkunde", "Gesundheit", "M%C3%BCttergesundheit", "M%C3%A4nnergesundheit",  "Rehabilitation",
                      "Anatomie", "Humanbiologie",  "Epidemiologie",  "Augenheilkunde", "Hals-Nasen-Ohren-Heilkunde",
                      "Pneumologie", "Psychiatrie", "Pharmazie", "Radiologie", "Geriatrie", "Palliativmedizin",
                      "Physiotherapie", "Hom%C3%B6opathie", "Krebs_(Medizin)", "Diabetes_mellitus", "Myokardinfarkt",
                      "Kardiologie", "Depression",  "Asthma_bronchiale",  "Chronische_obstruktive_Lungenerkrankung",
                      "Koronare_Herzkrankheit", "Infektion", "Schlaganfall",  "Leberzirrhose", "Allergie",
                      "Essst%C3%B6rung",  "Burn-out", "Chronisches_Schmerzsyndrom", "Bandscheibenvorfall",
                      "Rheuma", "Multiple_Sklerose", "Demenz", "Alzheimer-Krankheit", "Parkinson-Krankheit"))

#create a vector of texts ---------------------------------------------------------------------
sex_wiki <- map_chr(wiki_urls_sex, read_wiki_paras) %>% 
  enframe(., name = "document", value = "text") %>% 
  mutate(document = paste("sex", document, sep = "_"))

politics_wiki <- map_chr(wiki_urls_politics, read_wiki_paras)%>% 
  enframe(., name = "document", value = "text")%>% 
  mutate(document = paste("politics", document, sep = "_"))

health_wiki <- map_chr(wiki_urls_health, read_wiki_paras)%>% 
  enframe(., name = "document", value = "text")%>% 
  mutate(document = paste("health", document, sep = "_"))

#add the three texts on sexuality by freud from project gutenberg ------------------------------
freud <- gutenberg_download(39938) %>%
  .[45:4359,] #remove intro and final descriptions

# re-format to UTF
freud <- freud %>% mutate(text = iconv(freud$text, "ISO-8859-1", "utf8")) %>%
  .$text %>% paste(., collapse = " ")%>% # paste into one string
  enframe(., name = "document", value = "text")%>%
  mutate(document = paste("sex", length(sex_wiki$document)+1, sep = "_"))

# combine documents to one dataframe -----------------------------------------------------------

all_sources <- rbind(sex_wiki, politics_wiki, health_wiki, freud)
# all_sources <- rbind(sex_wiki, politics_wiki, health_wiki)

my_stopwords <- c("jedoch", "dass", "wurde", "wurden", "worden", "mehr", "sei", "seien", "beispielsweise", "siehe",
                  "häufig", "ein", "zwei", "drei", "vier", "fünf", "sechs", "sieben", "acht", "neun", "zehn",
                  "seit", "führen", "bzw", "oft", "gibt", "gab", "sowie", "dabei", "etwa", "kam", "kommt",
                  "konnte", "ab", "jahr", "jahre", "jahren", "eher", "allerdings", "gegenüber", "ebenso",
                  "immer", "schon", "etwa", "seinen", "beispiel", "viele", "insbesondere", "beim", "erst", "verschiedene",
                  "bereits", "bezeichnet", "begriff", "begriffe", "erstmals", "deren", "neben", "heute", 	"welt", 
                  "denen", "meist", "meisten", "aufgrund", "davon", "möglich", "erste", "ersten", "gilt", "laut",
                  "sehen", "of", "sowohl", "weitere", "januar", "februar", "märz", "april", "mai", "juni", "juli",
                  "august", "september", "oktober", "november", "dezember", "besonders", "je", "deshalb", "vielen",
                  "eigenen", "trat", "zudem", "bedeutung", "bezeichnung", "lassen", "führt", "ebenfalls", "kommen",
                  "and", "dagegen", "beiden", "innerhalb", "dafür", "all", "wer", "the", "dadurch", "liegt", "sollen",
                  "mittels", "prozent",	"zusammen", "wegen", "finden", "teil", "zusätzlich", "deutlich", "wobei",
                  "weniger", "sieht", "statt", "später", "obwohl", "findet", "selten", "teilweise"
                  ) %>% 
  enframe(value = "word")

by_word <- unnest_tokens(all_sources, word, text)%>%
  anti_join(enframe(stopwords(kind = "de"), value = "word"), by = "word") %>%
  anti_join(my_stopwords, by = "word") %>%
  filter(nchar(word)>1) %>%  # filter out words that are just one character (e.g. having remained from a split of "z.B.")
  filter(grepl("\\d", word) == FALSE) %>% # filter out numbers (such as for years) %>% 
  # mutate(word = stemDocument(word, language = "german")) %>% 
  count(document, word, sort = TRUE) %>%
  ungroup()

#create document-term matrix
dtm <- by_word %>%
  cast_dtm(document, word, n)

#fit LDA model with 3 topics
sources_lda <- LDA(dtm, k = 3, method = "Gibbs", control = list(seed = 1234)) 

sources_gamma <- tidy(sources_lda, matrix = "gamma")%>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)%>% #Separate one column into multiple columns.
  mutate(
    title = reorder(title, gamma * topic)
  )

sources_gamma %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

#identify the topic numbers for the specific sources
topic_IDs <- sources_gamma %>% 
  group_by(title, topic) %>% 
  summarise(gamma = median(gamma)) %>% 
  top_n(., 1, gamma)

#store the terms and their probabilities in tidy dataframe
sources_terms <- tidy(sources_lda, matrix = "beta")

# get the betas and spread the table
betas_wide <- sources_terms %>%
  spread(key = topic, value = beta)

# find top terms ----------------------------------------------------------------------------------------

top_terms <- tidy(sources_lda, matrix = "beta") %>%
  group_by(topic) %>%
  arrange(topic, desc(beta)) %>%
  #get the top num_words PER topic
  slice(seq_len(100)) %>%
  arrange(topic, beta) %>%
  #row is required for the word_chart() function
  mutate(row = row_number()) %>%
  ungroup() %>%
  #add the word Topic to the topic labels
  mutate(topic = paste("Topic", topic, sep = " "))

# find top_terms that are most specific to one topic
top_terms_ratio <- sources_terms %>% 
  mutate(topic = paste0("topic_", topic)) %>% 
  spread(key = topic, value = beta) %>% 
  mutate(
    ratio_1 = topic_1 * 2 / (topic_2 + topic_3),
    ratio_2 = topic_2 * 2 / (topic_1 + topic_3),
    ratio_3 = topic_3 * 2 / (topic_1 + topic_2)
  ) 

#define minimum threshold
thresholds <- group_by(sources_terms, topic) %>% 
  summarise(threshold = quantile(beta, 0.5))

# save objects needed in the algorithm
save(betas_wide, topic_IDs, sources_terms, thresholds, 
     file = "topic_models.RData")
