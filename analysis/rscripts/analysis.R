library(lme4)
library(dplyr)
library(emmeans)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(RColorBrewer)


theme_set(theme_bw())
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 


# 1. Data ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")

## prior data ----
prior.gpt4o.data <- read.csv("../../data/prior/prior_generate_system_gpt-4o.csv", header=TRUE) %>% 
  na.omit()
prior.gpt4.data <- read.csv("../../data/prior/prior_generate_system_gpt-4.csv", header=TRUE)
prior.gpt35.data <- read.csv("../../data/prior/prior_generate_system_gpt-3.5-turbo.csv", header=TRUE)
prior.human.data <- read.csv("../../data/prior/prior_means_human.csv", header=TRUE) %>% 
  rename(embedded_content = eventItem,
         rating = Mean) %>% 
  mutate(item = gsub("([A-Za-z]+).*", "\\1", embedded_content),
         embedded = "p")

prior.all.data <- bind_rows(lst(prior.gpt4.data, prior.gpt4o.data, prior.gpt35.data, prior.human.data), .id="model")
prior.all.model.data <- bind_rows(lst(prior.gpt4.data, prior.gpt4o.data, prior.gpt35.data), .id="model")

prior_p <- prior.all.data %>% 
  filter(embedded == "p") %>%
  mutate(model = case_when(model == "prior.gpt4.data" ~ "gpt-4",
                           model == "prior.gpt4o.data" ~ "gpt-4o",
                           model == "prior.gpt35.data" ~ "gpt-3.5-turbo",
                           model == "prior.human.data" ~ "human")) %>% 
  select(model,prior_type, item, embedded_content, prior, rating, distribution) %>% 
  mutate(embedded_content = fct_relevel(embedded_content, c("Isabella ate a steak on Sunday",
                                                            "Josh learned to ride a bike yesterday",
                                                            "Emily bought a car yesterday",
                                                            "Mia drank 2 cocktails last night",
                                                            "Julian dances salsa",
                                                            "Sophia got a tattoo",
                                                            "Olivia sleeps until noon",
                                                            "Frank got a cat",
                                                            "Emma studied on Saturday morning",
                                                            "Jayden rented a car",
                                                            "Danny ate the last cupcake",
                                                            "Josie went on vacation to France",
                                                            "Zoe calculated the tip",
                                                            "Owen shoveled snow last winter",
                                                            "Tony had a drink last night",
                                                            "Jon walks to work",
                                                            "Jackson ran 10 miles",
                                                            "Grace visited her sister",
                                                            "Charley speaks Spanish",
                                                            "Mary is pregnant")))


## projection data ----
projection.gpt4o.data <- read.csv("../../data/projection/projection_generate_system_gpt-4o.csv", header=TRUE) %>% 
  na.omit()
projection.gpt4.data <- read.csv("../../data/projection/projection_generate_system_gpt-4.csv", header=TRUE)
# additional run on gpt4 because of the unpredictable results
# projection.gpt4.data_og <- read.csv("../../data/projection/projection_generate_system_gpt-4-og.csv", header=TRUE)
projection.gpt35.data <- read.csv("../../data/projection/projection_generate_system_gpt-3.5-turbo.csv", header=TRUE)

projection.all.model.data <- bind_rows(lst(projection.gpt4.data, projection.gpt4o.data, projection.gpt35.data, projection.gpt4.data_og), .id="model")

projection_p <- projection.all.model.data %>% 
  filter(embedded_type == "p") %>%
  mutate(model = case_when(model == "projection.gpt4.data" ~ "gpt-4",
                           model == "projection.gpt4o.data" ~ "gpt-4o",
                           model == "projection.gpt35.data" ~ "gpt-3.5-turbo",
                           model == "projection.gpt4.data_og" ~ "gpt-4og")) %>%
  select(model,verb,prior_type, item, prior, rating)

projection_not_p <- projection.all.model.data %>% 
  filter(embedded_type == "not_p") %>%
  mutate(model = case_when(model == "projection.gpt4.data" ~ "gpt-4",
                           model == "projection.gpt4o.data" ~ "gpt-4o",
                           model == "projection.gpt35.data" ~ "gpt-3.5-turbo")) %>% 
  select(model,verb,prior_type, item, prior, rating)

### projection: embedded p, both facts ----
projection_p_mean <- projection_p %>% 
  group_by(verb, model, prior_type) |>
  summarize(mean_rating = mean(rating),
            ci_low = ci.low(rating), # confidence interval
            ci_high = ci.high(rating)) |> 
  ungroup() |>
  mutate(YMin = mean_rating - ci_low,
         YMax = mean_rating + ci_high,
         verb = fct_reorder(verb, .x=mean_rating))
         # verb = reorder_within(verb, mean_rating, model))

### projection: embedded not p, both facts ----
projection_not_p_mean <- projection_not_p %>% 
  group_by(verb, model, prior_type) |>
  summarize(mean_rating = mean(rating),
            ci_low = ci.low(rating), # confidence interval
            ci_high = ci.high(rating)) |> 
  ungroup() |>
  mutate(YMin = mean_rating - ci_low,
         YMax = mean_rating + ci_high,
         verb = fct_reorder(verb, .x=mean_rating))

### projection: embedded p and not p together ----
projection_all_mean <- bind_rows(lst(projection_p_mean,projection_not_p_mean), .id="embedded_type") %>% 
  mutate(embedded_type = ifelse(embedded_type == "projection_p_mean", "p", "not p")) %>% 
         # prior_type = ifelse(prior_type == "high_prior", "low_prior")) %>% # flip the prior for not p
  mutate(embedded_type = fct_relevel(embedded_type, c("p", "not p")))

### projection: human (Degen & Tonhauser, 2021, exp2b) ----
projection.human.data <- read.csv("../../data/projection/projection_prior_projectivity.csv", 
                             header=TRUE)
human_exclusion <- read.csv("../../data/projection/projection_prior_excluded.csv", 
                            header=TRUE)
projection.human.data = subset(projection.human.data, !is.element(assignmentid, human_exclusion$assignmentid)) %>% 
  mutate(verb = recode(verb, be_right_that = "right", inform_Sam = "inform")) %>% 
  filter(verb!="control") %>% 
  mutate(item = gsub("([A-Za-z]+).*", "\\1", fact),
         prior_type = recode(fact_type,factH="high_prior",factL="low_prior"),
         model = "human",
         embedded_type = "p") %>% 
  rename(embedded_content = content,
         rating = response) %>% 
  select(workerid, item, prior_type, rating, verb, model, embedded_type)

### projection: models and human ----
projection.all.data <- bind_rows(lst(projection.gpt4.data, projection.gpt4o.data, projection.gpt35.data, projection.human.data), .id="model")
projection_all_p <- projection.all.data %>% 
  filter(embedded_type == "p") %>%
  mutate(model = recode(model, 
                        projection.gpt4.data = "gpt-4",
                        projection.gpt4o.data = "gpt-4o",
                        projection.gpt35.data = "gpt-3.5-turbo",
                        projection.human.data = "human")) %>% 
  select(model, prior_type, item, rating, verb)

projection_all_p_mean <- projection_all_p %>% 
  group_by(verb, model, prior_type) |>
  summarize(mean_rating = mean(rating),
            ci_low = ci.low(rating), # confidence interval
            ci_high = ci.high(rating)) |> 
  ungroup() |>
  mutate(YMin = mean_rating - ci_low,
         YMax = mean_rating + ci_high,
         verb = fct_reorder(verb, .x=mean_rating))


## projection adversarial data ----
projection.gpt4o.adv.data <- read.csv("../../data/projection/projection_generate_system_gpt-4o-adv.csv", header=TRUE) %>% 
  na.omit()
projection.gpt35.adv.data <- read.csv("../../data/projection/projection_generate_system_gpt-3.5-turbo-adv.csv", header=TRUE)

projection.model.adv.data <- bind_rows(lst(projection.gpt4o.adv.data, projection.gpt35.adv.data), .id="model")

projection_p_adv <- projection.model.adv.data %>% 
  filter(embedded_type == "p") %>%
  mutate(model = case_when(model == "projection.gpt4o.adv.data" ~ "gpt-4o",
                           model == "projection.gpt35.adv.data" ~ "gpt-3.5-turbo")) %>%
  select(model,verb,prior_type, item, prior, rating)

### projection adversarial: embedded p, question not p----
projection_p_adv_mean <- projection_p_adv %>% 
  group_by(verb, model, prior_type) |>
  summarize(mean_rating = mean(rating),
            ci_low = ci.low(rating), # confidence interval
            ci_high = ci.high(rating)) |> 
  ungroup() |>
  mutate(YMin = mean_rating - ci_low,
         YMax = mean_rating + ci_high,
         verb = fct_reorder(verb, .x=mean_rating))


### projection adversarial: embedded p, question p and not p----
# convert all to belief in p
projection_p_adv_mean_convert <- projection_p_adv %>% 
  mutate(rating = 1-rating) %>% 
  group_by(verb, model, prior_type) |>
  summarize(mean_rating = mean(rating),
            ci_low = ci.low(rating), # confidence interval
            ci_high = ci.high(rating)) |> 
  ungroup() |>
  mutate(YMin = mean_rating - ci_low,
         YMax = mean_rating + ci_high,
         verb = fct_reorder(verb, .x=mean_rating))
projection_all_p_adv_mean <- bind_rows(lst(projection_p_adv_mean_convert, projection_p_mean), 
                                       .id="question_type") %>% 
  mutate(question_type = recode(question_type, projection_p_adv_mean_convert = "not p", projection_p_mean = "p")) %>%
  filter(model %in% c("gpt-3.5-turbo", "gpt-4o"),
         verb != "polar") %>% 
  mutate(question_type = fct_relevel(question_type, c("p", "not p")))


# 2. Plot ----
## prior ----
human_prior_p <- subset(prior_p, model == "human")
model_prior_p <- subset(prior_p, model != "human")
model_prior_p$facet <- model_prior_p$model
human_prior_p <- merge(human_prior_p,
                       data.frame(model = "human", 
                                  facet = unique(model_prior_p$facet)))
### prior (human and models): p ----
prior_p_plot <- rbind(human_prior_p, model_prior_p) %>% 
  mutate(is_model = ifelse(model=="human", "human", "model"))

prior_embedded_p_graph <- ggplot(data = prior_p_plot,
                           mapping = aes(y = embedded_content,
                                      x = rating,
                                      shape = is_model)) +
  geom_point(aes(fill = prior_type),size=4,alpha=0.6,
             position = position_jitter(w=0, h=0.17)) +
  geom_vline(xintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  labs(x = "Likelihood of p",
       y = "Embedded content",
       color = "prior",
       shape = "human or model") +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  facet_grid(. ~ facet) +
  scale_fill_manual(values=cbPalette,
                    labels=c("high prior", "low prior"),
                    name="Prior of p") + 
  scale_shape_manual(values=c("human"=24,"model"=21))
prior_embedded_p_graph
ggsave(prior_embedded_p_graph, file="../graphs/prior_models_facet.pdf", width=10, height=6)

## projection ----
### projection (models only): p ----
projection_embedded_p_graph <- ggplot(data = projection_p_mean,
                                 mapping = aes(x = verb,
                                               y = mean_rating,
                                               color = prior_type)) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=position_dodge(.9),
                width=.2,
                color="black") +
  geom_point(size=4,alpha=0.6) +
  geom_vline(xintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  labs(x = "verb",
       y = "Mean belief rating in p",
       color = "prior") +
  # guides(shape="none") +
  facet_grid(model~.) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.4)) +
  scale_x_reordered() +
  scale_color_manual(values=cbPalette,
                     labels=c("high prior", "low prior")) 
projection_embedded_p_graph
ggsave(projection_embedded_p_graph, file="../graphs/projection_models_p-facet.pdf", width=6, height=8)


### projection (models only): not p ----
projection_embedded_not_p_graph <- ggplot(data = projection_not_p_mean,
                                      mapping = aes(x = verb,
                                                    y = mean_rating,
                                                    color = prior_type)) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=position_dodge(.9),
                width=.2,
                color="black") +
  geom_point(size=4,alpha=0.6) +
  geom_vline(xintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  labs(x = "verb",
       y = "Mean belief rating in not p",
       color = "prior") +
  # guides(shape="none") +
  facet_grid(model~.,scales="free") +
  # scale_x_reordered() +
  scale_color_manual(values=cbPalette,
                     labels=c("high prior", "low prior"))+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
projection_embedded_not_p_graph
ggsave(projection_embedded_not_p_graph, file="../graphs/projection_models_not_p-facet.pdf", width=7, height=4)

### projection (human and models): p ----
human_projection_p_mean <- subset(projection_all_p_mean, model == "human")
model_projection_p_mean <- subset(projection_all_p_mean, model != "human")
model_projection_p_mean$facet <- model_projection_p_mean$model
human_projection_p_mean <- merge(human_projection_p_mean,
                       data.frame(model = "human", 
                                  facet = unique(model_projection_p_mean$facet)))

projection_p_plot <- rbind(human_projection_p_mean, model_projection_p_mean) %>% 
  mutate(is_model = ifelse(model=="human", "human", "model"))

projection_all_p_graph <- ggplot(data = projection_p_plot,
                                 mapping = aes(x = verb,
                                               y = mean_rating,
                                               shape = is_model)) +
  geom_point(aes(fill=prior_type),size=4,alpha=0.6) +
  labs(x = "Verb",
       color = "prior",
       shape = "human or model") +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  # coord_flip() +x
  facet_grid(model~.) +
  scale_fill_manual(values=cbPalette,
                    labels=c("high prior", "low prior"),
                    name="Prior of p") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(name="Mean belief in p", limits=c(0, 1)) + 
  scale_shape_manual(values=c("human"=24,"model"=21))
projection_all_p_graph
ggsave(projection_all_p_graph, file="../graphs/projection_model-human_facet.pdf", width=7, height=5)

projection_all_p_graph <- ggplot(data = projection_all_p_mean,
                                 mapping = aes(x = verb,
                                               y = mean_rating,
                                               shape = is_model,
                                               color = prior_type)) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=position_dodge(.9),
                width=.2,
                color="black") +
  geom_point(size=4,alpha=0.6) +
  geom_vline(xintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  labs(x = "verb",
       y = "Mean projection rating in p",
       color = "prior") +
  # guides(shape="none") +
  facet_grid(model~.) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.4)) +
  scale_x_reordered() +
  scale_color_manual(values=cbPalette,
                     labels=c("high prior", "low prior")) 
projection_all_p_graph


### projection (models only): p and not p (side-by-side) ----
projection_all_graph <- ggplot(data = projection_all_mean,
                                      mapping = aes(x = verb,
                                                    y = mean_rating,
                                                    color = prior_type)) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=position_dodge(.9),
                width=.2,
                color="black") +
  geom_point(size=4,alpha=0.6) +
  geom_vline(xintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  labs(x = "verb",
       y = "Mean belief rating in embedded content",
       color = "Prior belief for p") +
  facet_grid(model~embedded_type) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5)) +
  # facet_grid(model~.,scales="free") +
  # coord_flip() +
  scale_x_reordered() +
  scale_color_manual(values=cbPalette,
                     labels=c("high prior", "low prior")) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
projection_all_graph
ggsave(projection_all_graph, file="../graphs/projection_all-facet.pdf", width=8, height=6)

### prior ~ belief rating, line plot ----
prior_projection_p <- projection_p %>% 
  left_join(model_prior_p, by = c("model", "prior_type", "item")) %>% 
  rename(projection_rating = rating.x) %>% 
  rename(prior_rating = rating.y) %>% 
  select(model, verb, prior_type, item, projection_rating, prior_rating)

prior_projection_graph <- ggplot(data = prior_projection_p,
       aes(x=prior_rating,
           y=projection_rating,
           color=model)) +
  geom_point(alpha=0.4) +
  geom_smooth(method = "lm", fullrange=T) +
  facet_grid(. ~ model) +
  scale_x_continuous(name="Rating of prior belief in the embedded content", 
                     limits=c(0,1),
                     breaks=c(0,0.2,0.4,0.6,0.8,1)) + 
  scale_y_continuous(name="Mean attitude holder belief\nin the embedded content", limits=c(0,1)) + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  scale_colour_brewer(palette = "Set2")
prior_projection_graph

prior_projection_verb_graph <- ggplot(data = prior_projection_p %>% 
                                        filter(verb != "polar"),
                                 aes(x=prior_rating,
                                     y=projection_rating,
                                     color=model)) +
  geom_point(alpha=0.4) +
  geom_smooth(method = "lm", fullrange=T) +
  facet_wrap( ~ verb) +
  scale_x_continuous(name="Rating of prior belief in p", 
                     limits=c(0,1),
                     breaks=c(0,0.2,0.4,0.6,0.8,1)) + 
  scale_y_continuous(name="Mean attitude holder belief in p", limits=c(0,1)) + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  scale_colour_brewer(palette = "Set2")
prior_projection_verb_graph
ggsave(prior_projection_verb_graph, file="../graphs/projection_prior-verb.pdf", width=10, height=8)


## projection adversarial ----
### projection (models only): embedded p, question not p ----
projection_embedded_p_adv_graph <- ggplot(data = projection_p_adv_mean,
                                      mapping = aes(x = verb,
                                                    y = mean_rating,
                                                    color = prior_type)) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=position_dodge(.9),
                width=.2,
                color="black") +
  geom_point(size=4,alpha=0.6) +
  geom_vline(xintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  labs(x = "Verb",
       y = "Mean belief rating in not p",
       color = "Prior belief in p") +
  facet_grid(model~.) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.4)) +
  scale_x_reordered() +
  scale_color_manual(values=cbPalette,
                     labels=c("high prior", "low prior")) 
projection_embedded_p_adv_graph
ggsave(projection_embedded_p_adv_graph, file="../graphs/projection_models_p_adv-facet.pdf", width=8, height=6)


### projection (models only): embedded p, question p and not p ----
projection_all_p_adv_graph <- ggplot(data = projection_all_p_adv_mean,
                                          mapping = aes(x = verb,
                                                        y = mean_rating,
                                                        color = prior_type)) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=position_dodge(.9),
                width=.2,
                color="black") +
  geom_point(size=4,alpha=0.6) +
  geom_vline(xintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  labs(x = "Verb",
       y = "Mean belief rating in p",
       color = "Prior belief in p") +
  facet_grid(model~question_type) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.4)) +
  scale_x_reordered() +
  scale_color_manual(values=cbPalette,
                     labels=c("high prior", "low prior")) 
projection_all_p_adv_graph
ggsave(projection_all_p_adv_graph, file="../graphs/projection_models_all_p_adv-facet.pdf", width=9, height=6)



#### this is from human data
# additional helper functions/lists for graphs
dodge = position_dodge(.9)

utterance_label <- list("MC"="Filler",
                        "polar"="Polar",
                        "pos"="Embedded: p",
                        "neg"="Embedded: not p")
utterance_labeller <- function(variable,value){
  return(utterance_label[value])
}

predicate_label <- list("MC"="Filler",
                        "simple"="Polar",
                        "think"="think",
                        "know"="know",
                        "say"="say",
                        "confirm"="confirm",
                        "inform"="inform")
predicate_labeller <- function(variable,value){
  return(predicate_label[value])
}

content_label <- list("not_p"="Belief content: not p",
                      "p"="Belief content: p")
content_labeller <- function(variable,value){
  return(content_label[value])
}


# 3. Analysis  ----
## prior ----
### gpt3.5-turbo
prior_p_gpt35_analysis_data <- prior.all.data %>% 
  select(model, rating, item, prior_type) %>% 
  filter(model %in% c("prior.gpt35.data", "prior.human.data")) %>% 
  mutate(model = ifelse(model == "prior.gpt35.data", "prior_p_gpt35", "prior_p_human"))

prior_gpt35_base <- lmer(rating ~ model + (1 + model|item),
                         prior_p_gpt35_analysis_data)
summary(prior_gpt35_base)

prior_gpt35_condition <- lmer(rating ~ model * prior_type + (1 + model * prior_type|item),
                              prior_p_gpt35_analysis_data)
summary(prior_gpt35_condition)

### gpt4
#### data
prior_p_gpt4_analysis_data <- prior.all.data %>% 
  select(model, rating, item, prior_type) %>% 
  filter(model %in% c("prior.gpt4.data", "prior.human.data")) %>% 
  mutate(model = ifelse(model == "prior.gpt4.data", "prior_p_gpt4", "prior_p_human"))
#### analysis
prior_gpt4_base <- lmer(rating ~ model + (1 + model|item),
                         prior_p_gpt4_analysis_data)
summary(prior_gpt4_base)

prior_gpt4_condition <- lmer(rating ~ model * prior_type + (1 + model * prior_type|item),
                             prior_p_gpt4_analysis_data)
summary(prior_gpt4_condition)

### gpt4o
prior_p_gpt4o_analysis_data <- prior.all.data %>% 
  select(model, rating, item, prior_type) %>% 
  filter(model %in% c("prior.gpt4o.data", "prior.human.data")) %>% 
  mutate(model = ifelse(model == "prior.gpt4o.data", "prior_p_gpt4", "prior_p_human"))
prior_gpt4o_base <- lmer(rating ~ model + (1 + model|item),
                        prior_p_gpt4o_analysis_data)
summary(prior_gpt4o_base)

prior_gpt4o_condition <- lmer(rating ~ model * prior_type + (1 + model * prior_type|item),
                             prior_p_gpt4o_analysis_data)
summary(prior_gpt4o_condition)

## projectivity ----
