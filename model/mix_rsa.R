setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(jsonlite)
library(rwebppl)
library(stringr)

source("./inferenceHelpers.R")
source("./BDA_vizhelpers.R")

print(Sys.setenv(NODE_OPTIONS = "--max-old-space-size=16384"))
Sys.getenv("NODE_OPTIONS")

training_items <- c( "Charley_H", "Charley_L","Frank_H", "Frank_L","Danny_H", "Danny_L", "Emily_H", "Emily_L","Emma_H", "Emma_L", "Grace_H", "Grace_L", "Isabella_H","Isabella_L","Jackson_H","Jackson_L", "Jayden_H","Jayden_L","Jon_H","Jon_L","Josh_H","Josh_L","Josie_H","Josie_L","Julian_H", "Julian_L","Mary_H","Mary_L","Mia_H","Mia_L","Olivia_H", "Olivia_L")
testing_items <- c("Owen_H", "Owen_L", "Sophia_H", "Sophia_L", "Tony_H", "Tony_L", "Zoe_H", "Zoe_L")
## TO DO: add the two items (each has two versions, so four in total) to the test set

# 1. threshold_mix ----
## DATA----
# Read the pre-processed empirical data
df <- read.csv("./bda_data.csv") %>% 
  # convert the belief ratings (about the embedded content) to belief about p
  mutate(speaker_response = ifelse(polarity=="neg", 1-speaker_response, speaker_response),
         ah_response = ifelse(polarity=="neg", 1-ah_response, ah_response)) %>%
  # discretize the ratings
  mutate(speaker_response_bin = ifelse(speaker_response == 1, 10, trunc(speaker_response * 10) + 1)) %>%
  filter(predicate %in% c("Polar", "think", "know"),
         item %in% training_items)

# Create a script that can be used in the WebPPL interpeter (for testing)
# dataHeader = sprintf("var df = %s", toJSON(head(df)))
# write_file(paste(dataHeader, thresholdMixScript, sep = "\n"), file = "testScript.txt")

## INFERENCE ----
model <- makeModel("threshold_mix.txt", "threshold_mix")

thresholdMixScript <- wrapInference(model,"threshold_mix", 1000, 5, 500) 

thresholdMixPosteriors <- webppl(thresholdMixScript, data = df, data_var = "df", random_seed = 1024)

# saveRDS(thresholdMixPosteriors, "./thresholdMixPosteriors_negCost_seed1024-full.RDS")

# plot the posterior
graphPosteriors(thresholdMixPosteriors) + ggtitle("Posteriors")

ggsave("graphs/threshold_mix/params.pdf", width = 6, height = 2, units = "in")

# ggplot(thresholdMixPosteriors %>%
#          filter(!Parameter %in% c("alpha","bareCost")) %>%
#          pivot_wider(names_from = Parameter, values_from = value) %>% 
#          group_by(negCost, embedCost) %>% 
#          summarize(count = n()), 
#   aes(x = negCost, y = embedCost)) +
#   theme_bw() +
#   geom_tile(aes(fill = count))

# graphJointPosteriors(thresholdMixPosteriors)


## PREDICTIVES ----
df <- read.csv("./bda_data.csv") %>% 
  filter(predicate %in% c("think", "know", "Polar"))

df_collapsed <- df %>% 
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(speaker_response)) %>% 
  mutate(polarity = case_when(polarity=="pos" ~ "Embedded: p",
                              polarity=="neg" ~ "Embedded: not p",
                              TRUE ~ "Polar"))

# read the posteriors instead of rerunning
thresholdMixPosteriors <- readRDS("results/threshold_mix/thresholdMixPosteriors_negCost_seed1024-full.RDS")

thresholdMixEstimates <- getEstimates(thresholdMixPosteriors) 

# thresholdMixEstimates <- data.frame(alpha=3, embedCost =2, negCost=-1)

thresholdMixPredictionScript <- wrapPrediction(model, "threshold_mix", thresholdMixEstimates)

thresholdMixPredictives <- webppl(thresholdMixPredictionScript, data = unique(df %>%  select(utterance, item)), data_var = "df")

thresholdMixPredictives <- thresholdMixPredictives %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              str_detect(utterance, "BARE") ~ "Polar",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         prob = case_when(str_detect(utterance, "doesnt") ~ 1 - prob,
                          TRUE ~ prob), # convert model prediction to about the embedded content 
         # polarity = fct_relevel(polarity, "Polar", "pos", "neg" ),
         predicate = fct_relevel(predicate, "Polar", "think", "know")) %>%
  mutate(polarity = case_when(polarity=="pos" ~ "Embedded: p",
                              polarity=="neg" ~ "Embedded: not p",
                              TRUE ~ "Polar"),
         polarity = fct_relevel(polarity, rev)) %>% 
  select(-utterance)

# save the model predictions
temp <- thresholdMixPredictives %>%
  # filter(polarity == "Embedded: p") %>%
  mutate(prior_type = ifelse(grepl("_L", item), "low_prior", "high_prior"),
         item = str_extract(item, "[^_]+"),
         model = "RSA")
write.csv(temp, "RSA_results.csv")

# graphPredictives(thresholdMixPredictives, df_collapsed)
graphPredictives_test(thresholdMixPredictives, df_collapsed)

ggsave("graphs/threshold_mix/thresholdMix-full_predictive-emprical_combined.pdf", width = 8, height = 4, units = "in")
