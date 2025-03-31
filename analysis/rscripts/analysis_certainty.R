library(dplyr)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
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

# reorder based on human ratings
prior_all <- prior.all.data %>% 
  mutate(model = case_when(model == "prior.gpt4.data" ~ "gpt-4",
                           model == "prior.gpt4o.data" ~ "gpt-4o",
                           model == "prior.gpt35.data" ~ "gpt-3.5-turbo",
                           model == "prior.human.data" ~ "human")) %>% 
  rename(embedded_type = embedded) %>% 
  select(model,prior_type, item, embedded_type,embedded_content, prior, rating, distribution, YMin, YMax) %>% 
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
                                                            "Mary is pregnant")),
         # c(“Mary”,“Charley”,“Grace”,“Jackson”,“Jon”,“Tony”,“Owen”, “Zoe”,“Josie”,“Danny”,“Jayden”,“Emma”,“Frank”,“Olivia”, “Sophia”,“Julian”,“Mia”,“Emily”,“Josh”,“Isabella”
         item = fct_relevel(item, c("Isabella", "Josh", "Emily", "Mia", "Julian", "Sophia", "Olivia", "Frank", "Emma", "Jayden", "Danny", "Josie", "Zoe", "Owen", "Tony", "Jon", "Jackson","Grace","Charley", "Mary")))

prior_all_p <- prior_all %>% 
  filter(embedded_type == "p")

prior_all_not_p <- prior_all %>% 
  filter(embedded_type == "not_p")

### projection: human (Degen & Tonhauser, 2021, exp1) ----
projection.human.data <- read.csv("../../data/projection/projection_prior.csv", header=TRUE)
projection.human.data <- projection.human.data %>% 
  mutate(verb = recode(short_trigger, be_annoyed = "annoyed", be_right = "right")) %>% 
  filter(short_trigger!="MC") %>% 
  mutate(item = content,
         prior_type = recode(prior_type,factH="high_prior",factL="low_prior"),
         model = "human",
         embedded_type = "p") %>% 
  rename(embedded_content = eventItem,
         projection_rating = projective,
         prior_rating = prior) %>% 
  select(workerid, item, prior_type, projection_rating, verb, model, embedded_type, prior_rating)

## alternative with certainty prompt ----
projection_certainty.gpt4.data <- read.csv("../../data/projection/projection_generate_system_gpt-4-certainty.csv", header=TRUE) %>%
  mutate(embedded_type=if_else(verb=="polar","polar", embedded_type))
projection_certainty.gpt4o.data <- read.csv("../../data/projection/projection_generate_system_gpt-4o-certainty.csv", header=TRUE) %>%
  mutate(embedded_type=if_else(verb=="polar","polar", embedded_type))
projection_certainty.gpt35.data <- read.csv("../../data/projection/projection_generate_system_gpt-3.5-turbo-certainty.csv", header=TRUE) %>%
  mutate(embedded_type=if_else(verb=="polar","polar", embedded_type))

projection_certainty.all.model.data <- bind_rows(lst(projection_certainty.gpt4.data, projection_certainty.gpt4o.data, projection_certainty.gpt35.data), .id="model")

projection_certainty_model_all <- projection_certainty.all.model.data %>% 
  mutate(model = case_when(model == "projection_certainty.gpt4.data" ~ "gpt-4",
                           model == "projection_certainty.gpt4o.data" ~ "gpt-4o",
                           model == "projection_certainty.gpt35.data" ~ "gpt-3.5-turbo")) %>%
  rename(projection_rating = rating) %>% 
  select(model,verb,prior_type, item, prior, projection_rating,embedded_type)

projection_certainty_model_p <- projection_certainty_model_all %>% 
  filter(embedded_type == "p") 

projection_certainty_model_not_p <- projection_certainty_model_all %>% 
  filter(embedded_type == "not_p") 

projection_certainty_model_prior <- projection_certainty_model_all %>% 
  # polar should get the prior rating as p
  mutate(embedded_type=if_else(embedded_type=="polar","p",embedded_type)) %>% 
  left_join(prior_all, by = c("model", "prior_type", "item","embedded_type")) %>%
  rename(prior_rating=rating)

### projection certainty: embedded p, both facts ----
projection_certainty_model_p_mean <- projection_certainty_model_p %>% 
  group_by(verb, model, prior_type) %>% 
  summarize(mean_rating = mean(projection_rating),
            ci_low = ci.low(projection_rating), # confidence interval
            ci_high = ci.high(projection_rating)) %>% 
  ungroup() %>% 
  mutate(YMin = mean_rating - ci_low,
         YMax = mean_rating + ci_high,
         verb = fct_reorder(verb, .x=mean_rating))

### projection certainty: embedded not p, both facts ----
projection_certainty_model_not_p_mean <- projection_certainty_model_not_p %>% 
  group_by(verb, model, prior_type) %>% 
  summarize(mean_rating = mean(projection_rating),
            ci_low = ci.low(projection_rating), # confidence interval
            ci_high = ci.high(projection_rating)) %>% 
  ungroup() %>% 
  mutate(YMin = mean_rating - ci_low,
         YMax = mean_rating + ci_high,
         verb = fct_reorder(verb, .x=mean_rating))

### projection certainty: embedded p and not p together ----
projection_certainty_all_mean <- bind_rows(lst(projection_certainty_model_p_mean,projection_certainty_model_not_p_mean), .id="embedded_type") %>% 
  mutate(embedded_type = ifelse(embedded_type == "projection_certainty_model_p_mean", "p", "not p")) %>% 
  mutate(embedded_type = fct_relevel(embedded_type, c("p", "not p")))

### projection certainty: models and human ----
projection_certainty.model.human.data <- bind_rows(lst(projection_certainty_model_prior, projection.human.data), .id="data")

projection_certainty_model_human <- projection_certainty.model.human.data %>%
  mutate(item = str_to_title(item)) %>% 
  select(model, prior_type, item, projection_rating, verb,embedded_type, prior_rating)

projection_certainty_model_human_p <- projection_certainty_model_human %>% 
  filter(embedded_type == "p")

projection_certainty_model_human_p_mean <- projection_certainty_model_human_p %>% 
  group_by(verb, model, prior_type) %>% 
  summarize(mean_rating = mean(projection_rating),
            ci_low = ci.low(projection_rating), # confidence interval
            ci_high = ci.high(projection_rating)) %>% 
  ungroup() %>% 
  mutate(YMin = mean_rating - ci_low,
         YMax = mean_rating + ci_high,
         verb = fct_reorder(verb, .x=mean_rating))

## RSA data ----
rsa_data  <- read.csv("../../data/projection/RSA_results.csv", 
                      header=TRUE) %>% 
  left_join(prior.human.data,by=c("item","prior_type")) %>% 
  rename(projection_rating = prob,
         prior_rating = rating,
         verb = predicate,
         embedded_type=polarity) %>% 
  mutate(verb = if_else(verb == "Polar", "polar", verb),
         embedded_type = case_when(embedded_type == "Embedded: p" ~ "p",
                                   embedded_type == "Embedded: not p"~ "not_p",
                                   embedded_type == "Polar"~ "polar",
                                   TRUE ~ "error")) %>% 
  select(c("projection_rating", "item", "prior_rating", "verb", "prior_type","model","embedded_type"))

rsa_p <- rsa_data %>% 
  filter(embedded_type %in% c("p","polar"))

rsa_not_p <- rsa_data %>% 
  filter(embedded_type == "not_p")

### projection certainty: models, human, and RSA ----
prior_projection_certainty_p_rsa <- bind_rows(rsa_p, projection_certainty_model_human_p)

projection_certainty_all_item_mean <- prior_projection_certainty_p_rsa %>% 
  filter(verb %in% c("think", "know", "polar")) %>% 
  group_by(verb, model, item, prior_type) %>% 
  summarize(mean_rating = mean(projection_rating),
            ci_low = ci.low(projection_rating), # confidence interval
            ci_high = ci.high(projection_rating)) %>% 
  ungroup() %>% 
  mutate(YMin = mean_rating - ci_low,
         YMax = mean_rating + ci_high,
         verb = fct_reorder(verb, .x=mean_rating))

# include prior rating
projection_certainty_prior_item_mean <- prior_projection_certainty_p_rsa %>% 
  filter(verb %in% c("think", "know", "polar")) %>% 
  group_by(verb, model, item, prior_type, prior_rating) %>% 
  summarize(mean_rating = mean(projection_rating),
            ci_low = ci.low(projection_rating), # confidence interval
            ci_high = ci.high(projection_rating)) %>% 
  ungroup() %>% 
  mutate(YMin = mean_rating - ci_low,
         YMax = mean_rating + ci_high,
         verb = fct_reorder(verb, .x=mean_rating)) %>% 
  rename(projection_rating = mean_rating)

# 2. Plot ----
## prior ----
human_prior_p <- subset(prior_all_p, model == "human")
model_prior_p <- subset(prior_all_p, model != "human")
model_prior_p$facet <- model_prior_p$model
human_prior_p <- merge(human_prior_p,
                       data.frame(model = "human",
                                  facet = unique(model_prior_p$facet)))
### prior (human and models): p ----
prior_p_plot <- rbind(human_prior_p, model_prior_p) %>% 
  mutate(is_model = ifelse(model=="human", "human", "model"))

prior_embedded_p_graph <- ggplot(data = prior_p_plot,
                                 mapping = aes(x = item,
                                               y = rating,
                                               shape = is_model)) +
  geom_point(aes(fill = prior_type),size=4,alpha=0.6) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=position_dodge(width=.9),
                width=.2) +
  geom_vline(xintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  coord_flip() +
  labs(y = "Prior belief ratings of p",
       x = "Embedded content",
       color = "prior",
       shape = "human or model") +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  facet_grid(.~model) +
  scale_fill_manual(values=cbPalette,
                    labels=c("high prior", "low prior"),
                    name="Prior of p") + 
  scale_shape_manual(values=c("human"=24,"model"=21)) + 
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        # axis.text.x = element_text(size = 10),
        axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.4, size=10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "top")
prior_embedded_p_graph
# ggsave(prior_embedded_p_graph, file="../graphs/prior_models_facet.pdf", width=7, height=7)
ggsave(prior_embedded_p_graph, file="../graphs/prior_models_facet_1.pdf", width=10, height=4)

## projection ----
### projection certainty (models only): p ----
projection_certainty_embedded_p_graph <- ggplot(data = projection_certainty_model_p_mean,
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
projection_certainty_embedded_p_graph
ggsave(projection_certainty_embedded_p_graph, file="../graphs/projection_models_p_certainty-facet.pdf", width=6, height=8)


### projection certainty (models only): not p ----
projection_certainty_embedded_not_p_graph <- ggplot(data = projection_certainty_model_not_p_mean,
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
  facet_grid(model~.) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.4)) +
  scale_x_reordered() +
  scale_color_manual(values=cbPalette,
                     labels=c("high prior", "low prior")) 
projection_certainty_embedded_not_p_graph
ggsave(projection_certainty_embedded_not_p_graph, file="../graphs/projection_models_not_p_certainty-facet.pdf", width=7, height=4)


### projection certainty (human and models): p ----
human_projection_certainty_p_mean <- subset(projection_certainty_model_human_p_mean, model == "human")
model_projection_certainty_p_mean <- subset(projection_certainty_model_human_p_mean, model != "human")
model_projection_certainty_p_mean$facet <- model_projection_certainty_p_mean$model
human_projection_certainty_p_mean <- merge(human_projection_certainty_p_mean,
                                           data.frame(model = "human", 
                                                      facet = unique(model_projection_certainty_p_mean$facet)))

projection_certainty_p_plot <- rbind(human_projection_certainty_p_mean, model_projection_certainty_p_mean) %>% 
  mutate(is_model = ifelse(model=="human", "human", "model"))

projection_certainty_all_p_graph <- ggplot(data = projection_certainty_p_plot %>% 
                                             filter(verb!="polar"),
                                           mapping = aes(x = verb,
                                                         y = mean_rating,
                                                         shape = is_model)) +
  geom_point(aes(fill=prior_type),size=3,alpha=0.6) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                # position=position_dodge(.9)
                width=.2) +
  labs(x = "Verb",
       color = "prior",
       shape = "human or model") +
  # theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.4)) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  coord_flip() +
  # facet_wrap(model~.) +
  facet_grid(.~model)+
  scale_fill_manual(values=cbPalette,
                    labels=c("high prior", "low prior"),
                    name="Prior of p") +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        # axis.text.x = element_text(size = 10),
        axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.4, size=10),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14)) +
  scale_y_continuous(name="Mean certainty ratings of p", limits=c(0,1)) + 
  scale_shape_manual(values=c("human"=24,"model"=21))
projection_certainty_all_p_graph
# ggsave(projection_certainty_all_p_graph, file="../graphs/projection_certainty_model-human_facet.pdf", width=7, height=7)
ggsave(projection_certainty_all_p_graph, file="../graphs/projection_certainty_model-human_facet_1.pdf", width=10, height=4)

# facet in columns
projection_certainty_all_p_graph_col <- ggplot(data = projection_certainty_p_plot,
                                               mapping = aes(x = verb,
                                                             y = mean_rating,
                                                             shape = is_model)) +
  geom_point(aes(fill=prior_type),size=4,alpha=0.6) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=position_dodge(.9),
                width=.2,
                color="black") +
  geom_vline(xintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  labs(x = "verb",
       y = "Mean projection rating in p",
       color = "prior",
       shape = "human or model") +
  # guides(shape="none") +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  facet_grid(model~.) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.4)) +
  scale_x_reordered() +
  scale_fill_manual(values=cbPalette,
                    labels=c("high prior", "low prior"),
                    name="Prior of p") +
  scale_shape_manual(values=c("human"=24,"model"=21))
projection_certainty_all_p_graph_col

### prior ~ projection certainty by verb (models, human) ----
projection_certainty_model_prior_p <- projection_certainty_model_prior %>% 
  filter(embedded_type == "p") %>% 
  select(model, verb, prior_type, item, projection_rating, prior_rating)

prior_projection_certainty_human_verb_graph <- ggplot(data = projection_certainty_model_human_p %>% 
                                                        filter(verb != "polar"),
                                                      aes(x=prior_rating,
                                                          y=projection_rating,
                                                          color=model)) +
  geom_point(data = projection_certainty_model_prior_p %>% 
               filter(model!="human") %>% 
               filter(verb!="polar"), alpha=0.4) +
  geom_smooth(method = "lm", fullrange=T) +
  facet_wrap( ~ verb) +
  scale_x_continuous(name="Rating of prior belief in p", 
                     limits=c(0,1),
                     breaks=c(0,0.2,0.4,0.6,0.8,1)) + 
  scale_y_continuous(name="Mean certainty rating of p", limits=c(0,1)) + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.text=element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 12)) +
  scale_colour_brewer(palette = "Set2")
prior_projection_certainty_human_verb_graph
ggsave(prior_projection_certainty_human_verb_graph, file="../graphs/projection_prior_certainty_human-verb_1.pdf", width=10, height=8)

### prior ~ projection certainty, think and know (models, humans, RSA) ----
# prior_projection_certainty_p <- projection_certainty_model_prior %>% 
#   filter(embedded_type == "p") %>% 
#   select(model, verb, prior_type, item, projection_rating, prior_rating)
prior_projection_certainty_rsa_graph <- ggplot(data = prior_projection_certainty_p_rsa %>% 
                                                      filter(verb %in% c("think", "know")),
                                                    aes(x=prior_rating,
                                                        y=projection_rating,
                                                        color=model)) +
  geom_point(data = projection_certainty_prior_item_mean %>% 
               # filter(model!="human") %>% 
               filter(verb %in% c("think", "know")), alpha=0.4) +
  geom_smooth(method = "lm", fullrange=T) +
  scale_x_continuous(name="Rating of prior belief in p", 
                     limits=c(0,1),
                     breaks=c(0,0.2,0.4,0.6,0.8,1)) + 
  scale_y_continuous(name="Mean certainty ratings of p", limits=c(0,1)) + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "top") +
  scale_colour_brewer(palette = "Set2") 
prior_projection_certainty_rsa_graph
ggsave(prior_projection_certainty_rsa_graph, file="../graphs/projection_prior_certainty_rsa-all_legend.pdf", width=6, height=4)

### prior ~ projection certainty, facet by verb (models, humans, RSA) ----
# no polar because it is not included in Degen & Tonhauser 2021
prior_projection_certainty_verb_rsa_graph <- ggplot(data = prior_projection_certainty_p_rsa %>% 
                                                      filter(verb %in% c("think", "know")),
                                                    aes(x=prior_rating,
                                                        y=projection_rating,
                                                        color=model)) +
  geom_point(data = projection_certainty_prior_item_mean %>% 
               # filter(model!="human") %>% 
               filter(verb %in% c("think", "know")), alpha=0.4) +
  geom_smooth(method = "lm", fullrange=T) +
  facet_wrap( ~ verb) +
  scale_x_continuous(name="Rating of prior belief in p", 
                     limits=c(0,1),
                     breaks=c(0,0.2,0.4,0.6,0.8,1)) + 
  scale_y_continuous(name="Mean certainty ratings of p", limits=c(0,1)) + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "top") +
  scale_colour_brewer(palette = "Set2")
prior_projection_certainty_verb_rsa_graph
ggsave(prior_projection_certainty_verb_rsa_graph, file="../graphs/projection_prior_certainty_rsa-verb_1.pdf", width=6, height=4)


# 3. Analysis  ----
## prior ----
### gpt3.5-turbo ----
# prior.all.data$prior_type = relevel(as.factor(as.character(prior.all.data$prior_type)), ref="low_prior")
# contrasts(prior.all.data$prior_type) = c(1,0)

#### data ----
prior_p_gpt35_analysis_data <- prior.all.data %>%
  filter(embedded == "p") %>% 
  select(model, rating, item, prior_type) %>%
  filter(model %in% c("prior.gpt35.data", "prior.human.data")) %>%
  mutate(model = ifelse(model == "prior.gpt35.data", "prior_p_gpt35", "prior_p_human")) %>% 
  pivot_wider(names_from = model, values_from = rating) %>% 
  mutate(prior_type = fct_relevel(prior_type, ref="low_prior"))

#### analysis ----
##### main effect ----
prior_gpt35 <- lmer(prior_p_gpt35 ~ prior_type + (1|item),
                    prior_p_gpt35_analysis_data)
summary(prior_gpt35)

##### human vs. model ----
prior_gpt35_base <- lmer(prior_p_human ~ prior_p_gpt35 + (1|item),
                         prior_p_gpt35_analysis_data)
summary(prior_gpt35_base)

prior_gpt35_condition <- lmer(prior_p_human ~ prior_p_gpt35 + prior_type + (1|item),
                              prior_p_gpt35_analysis_data)
summary(prior_gpt35_condition)
anova(prior_gpt35_base, prior_gpt35_condition)


### gpt4 ----
#### data ----
prior_p_gpt4_analysis_data <- prior.all.data %>% 
  filter(embedded == "p") %>% 
  select(model, rating, item, prior_type) %>% 
  filter(model %in% c("prior.gpt4.data", "prior.human.data")) %>% 
  mutate(model = ifelse(model == "prior.gpt4.data", "prior_p_gpt4", "prior_p_human")) %>% 
  pivot_wider(names_from = model, values_from = rating) %>% 
  mutate(prior_type = fct_relevel(prior_type, ref="low_prior"))

#### analysis ----
##### main effect ----
prior_gpt4 <- lmer(prior_p_gpt4 ~ prior_type + (1|item),
                   prior_p_gpt4_analysis_data)
summary(prior_gpt4)

##### human vs. model ----
prior_gpt4_base <- lmer(prior_p_human ~ prior_p_gpt4 + (1|item),
                        prior_p_gpt4_analysis_data)
summary(prior_gpt4_base)

prior_gpt4_condition <- lmer(prior_p_human ~ prior_p_gpt4 + prior_type + (1|item),
                             prior_p_gpt4_analysis_data)
summary(prior_gpt4_condition)
anova(prior_gpt4_base, prior_gpt4_condition)

### gpt4o ----
#### data ----
prior_p_gpt4o_analysis_data <- prior.all.data %>% 
  filter(embedded == "p") %>% 
  select(model, rating, item, prior_type) %>% 
  filter(model %in% c("prior.gpt4o.data", "prior.human.data")) %>% 
  mutate(model = ifelse(model == "prior.gpt4o.data", "prior_p_gpt4o", "prior_p_human")) %>% 
  pivot_wider(names_from = model, values_from = rating) %>% 
  mutate(prior_type = fct_relevel(prior_type, ref="low_prior"))

#### analysis ----
##### main effect ----
prior_gpt4o <- lmer(prior_p_gpt4o ~ prior_type + (1|item),
                    prior_p_gpt4o_analysis_data)
summary(prior_gpt4o)

##### human vs. model ----
prior_gpt4o_base <- lmer(prior_p_human ~ prior_p_gpt4o + (1 |item),
                         prior_p_gpt4o_analysis_data)
summary(prior_gpt4o_base)

prior_gpt4o_condition <- lmer(prior_p_human ~ prior_p_gpt4o + prior_type + (1|item),
                              prior_p_gpt4o_analysis_data)
summary(prior_gpt4o_condition)
anova(prior_gpt4o_base, prior_gpt4o_condition)

## projection ----
### rsa and models (certainty) vs. human analysis ----
projection_all_model_pivot <- projection_certainty_model_prior %>% 
  bind_rows(rsa_data) %>% 
  filter(embedded_type=="p") %>%
  filter(verb %in% c("think", "know")) %>%
  select(model, verb, prior_type, embedded_type, item, projection_rating) %>% 
  pivot_wider(names_from=model, values_from=projection_rating)

rsa_analysis <- projection_all_model_pivot %>% 
  merge(projection.human.data %>% 
          mutate(item=str_to_title(item)) %>% 
          filter(verb %in% c("think", "know"))) %>% 
  select(-model) %>% 
  rename(human=projection_rating)

#### predicting each datapoint ----
model_rsa_analysis <- rsa_analysis %>% 
  rename(gpt4="gpt-4", gpt35="gpt-3.5-turbo", gpt4o="gpt-4o") %>% 
  mutate(RSA=scale(RSA,center=TRUE),
         gpt4=scale(gpt4,center=TRUE),
         gpt4o=scale(gpt4o,center=TRUE),
         gpt35=scale(gpt35,center=TRUE)) %>% 
  drop_na(RSA) %>%  # two items do not have RSA predictions
  mutate(prior_type = relevel(as.factor(prior_type), ref="low_prior") )

##### rsa ----
rsa_base <- lmer(human ~ RSA + (1|item) + (1|workerid),
                 model_rsa_analysis)
summary(rsa_base)
AIC(rsa_base)

# by verb AIC
rsa_think_base <- lmer(human ~ RSA + (1|item),
                 model_rsa_analysis %>%
                   filter(verb=="think"))
summary(rsa_think_base)
AIC(rsa_think_base)

rsa_know_base <- lmer(human ~ RSA + (1|item),
                       model_rsa_analysis %>%
                         filter(verb=="know"))
summary(rsa_know_base)
AIC(rsa_know_base)

# in the origional study, they use the prior type (high vs. low) instead of 
# the numerical values
# rsa_prior <- lmer(human ~ RSA + prior_rating + (1|item) + (1|workerid),
#                   model_rsa_analysis)
# rsa_prior <- lmer(human ~ RSA + prior_type + (1+prior_type|item) + (1|workerid),
#                   model_rsa_analysis)
# random effect structure should be the same as the base model for model comparison
rsa_prior <- lmer(human ~ RSA + prior_type + (1|item) + (1|workerid),
                  model_rsa_analysis)
summary(rsa_prior)

##### gpt4o ----
gpt4o_base <- lmer(human ~ gpt4o + (1|item) + (1|workerid),
                   model_rsa_analysis)
summary(gpt4o_base)
AIC(gpt4o_base)

# by verb AIC
gpt4o_think_base <- lmer(human ~ gpt4o + (1|item),
                   model_rsa_analysis %>% 
                     filter(verb=="think"))
summary(gpt4o_think_base)
AIC(gpt4o_think_base)

gpt4o_know_base <- lmer(human ~ gpt4o + (1|item),
                         model_rsa_analysis %>% 
                           filter(verb=="know"))
summary(gpt4o_know_base)
AIC(gpt4o_know_base)

gpt4o_rsa <- lmer(human ~ gpt4o + RSA + (1|item)+ (1|workerid),
                  model_rsa_analysis)
summary(gpt4o_rsa)
# comparing having or not having RSA as a factor
anova(gpt4o_base, gpt4o_rsa)
# comparing having or not having LLM as a factor
anova(rsa_base, gpt4o_rsa)

# gpt4o_prior <- lmer(human ~ gpt4o + prior_rating + (1|item)+ (1|workerid),
#                     model_rsa_analysis)
# gpt4o_prior <- lmer(human ~ gpt4o + prior_type + (1+prior_type|item)+ (1|workerid),
#                     model_rsa_analysis)
# random effect structure should stay the same as the base model for model comparison
gpt4o_prior <- lmer(human ~ gpt4o + prior_type + (1|item)+ (1|workerid),
                    model_rsa_analysis)
summary(gpt4o_prior)
# comparing RSA and prior as a factor
anova(gpt4o_prior, gpt4o_rsa)

gpt4o_prior_rsa <- lmer(human ~ gpt4o + RSA + prior_type + (1+prior_type|item)+ (1|workerid),
                        model_rsa_analysis)
summary(gpt4o_prior_rsa)

##### gpt4 ----
gpt4_base <- lmer(human ~ gpt4 + (1|item)+ (1|workerid),
                  model_rsa_analysis)
summary(gpt4_base)
AIC(gpt4_base)

gpt4_think_base <- lmer(human ~ gpt4 + (1|item),
                        model_rsa_analysis %>% 
                          filter(verb=="think"))
summary(gpt4_think_base)
AIC(gpt4_think_base)

gpt4_know_base <- lmer(human ~ gpt4 + (1|item),
                        model_rsa_analysis %>% 
                          filter(verb=="know"))
summary(gpt4_know_base)
AIC(gpt4_know_base)

gpt4_rsa <- lmer(human ~ gpt4 + RSA + (1|item)+ (1|workerid),
                 model_rsa_analysis)
summary(gpt4_rsa)

# comparing having or not having RSA as a factor
anova(gpt4_base, gpt4_rsa)
# comparing having or not having LLM as a factor
anova(rsa_base, gpt4_rsa)

# gpt4_prior <- lmer(human ~ gpt4 + prior_rating + (1|item)+ (1|workerid),
#                    model_rsa_analysis)
# gpt4_prior <- lmer(human ~ gpt4 + prior_type + (1+prior_type|item)+ (1|workerid),
#                    model_rsa_analysis)
# random effect structure should be the same as the base model for model comparison
gpt4_prior <- lmer(human ~ gpt4 + prior_type + (1|item)+ (1|workerid),
                    model_rsa_analysis)
summary(gpt4_prior)

gpt4_prior_rsa <- lmer(human ~ gpt4 + RSA + prior_type + (1+prior_type|item)+ (1|workerid),
                       model_rsa_analysis)
summary(gpt4_prior_rsa)

##### gpt3.5-turbo ----
gpt35_base <- lmer(human ~ gpt35 + (1|item)+ (1|workerid),
                   model_rsa_analysis)
summary(gpt35_base)
AIC(gpt35_base)

gpt35_think_base <- lmer(human ~ gpt35 + (1|item),
                   model_rsa_analysis %>% 
                     filter(verb=="think"))
summary(gpt35_think_base)
AIC(gpt35_think_base)

gpt35_know_base <- lmer(human ~ gpt35 + (1|item),
                         model_rsa_analysis %>% 
                           filter(verb=="know"))
summary(gpt35_know_base)
AIC(gpt35_know_base)

gpt35_rsa <- lmer(human ~ gpt35 + RSA + (1|item)+ (1|workerid),
                  model_rsa_analysis)
summary(gpt35_rsa)
anova(gpt35_base, gpt35_rsa)
# comparing having or not having LLM as a factor
anova(rsa_base, gpt35_rsa)

# gpt35_prior <- lmer(human ~ gpt35 + prior_rating + (1|item)+ (1|workerid),
#                     model_rsa_analysis)
# gpt35_prior <- lmer(human ~ gpt35 + prior_type + (1+prior_type|item)+ (1|workerid),
#                     model_rsa_analysis)
# the random effect structure should be the same as the base model for model comparison
gpt35_prior <- lmer(human ~ gpt35 + prior_type + (1|item)+ (1|workerid),
                    model_rsa_analysis)
summary(gpt35_prior)

gpt35_prior_rsa <- lmer(human ~ gpt35 + RSA+ prior_type + (1+prior_type|item)+ (1|workerid),
                        model_rsa_analysis)
summary(gpt35_prior_rsa)

### exploratory by-verb analysis ----
prior_projection_p_rsa_analysis <- prior_projection_certainty_p_rsa %>% 
  filter(verb %in% c("know","think")) %>% 
  mutate(model=relevel(as.factor(model),ref="human"),
         centered_prior_rating=scale(prior_rating, scale=FALSE))

#### both verbs ----
# projection_prior_rsa <- lmer(projection_rating ~ model + centered_prior_rating + verb + (1+model| item),
#                              prior_projection_p_rsa_analysis)
# summary(projection_prior_rsa)


#### think ----
projection_rsa_think <- lmer(projection_rating ~ model + centered_prior_rating + (1| item),
                             prior_projection_p_rsa_analysis %>% filter(verb=="think"))
summary(projection_rsa_think)

projection_prior_rsa_think <- lmer(projection_rating ~ model + prior_type + (1+prior_type| item),
                                   prior_projection_p_rsa_analysis %>% 
                                     filter(verb=="think"))
summary(projection_prior_rsa_think)

#### know ----
projection_rsa_know <- lmer(projection_rating ~ model + centered_prior_rating + (1| item),
                            prior_projection_p_rsa_analysis %>% filter(verb=="know"))
summary(projection_rsa_know)

projection_prior_rsa_know <- lmer(projection_rating ~ model + prior_type + (1+prior_type| item),
                                  prior_projection_p_rsa_analysis %>% 
                                    filter(verb=="know"))
summary(projection_prior_rsa_know)

# (schematized plots of modulating factors, for slides) ----
prior <- c("German", "Cuban")
prior_rating <- c(0.4, 0.61)
prior_schema.df <- data.frame(prior=prior,rating=prior_rating)
prior_schema.df$prior <- factor(prior_schema.df$prior, levels = rev(levels(factor(prior_schema.df$prior))))

prior_schema <- ggplot(prior_schema.df,
                       aes(x=prior,
                           y=prior_rating,
                           fill=prior))+
  geom_bar(stat = "identity",width = 0.5,color="black")+
  geom_hline(yintercept=0.5,linetype='dashed', alpha=0.6,color="grey")+
  scale_fill_manual(values=c("#56B4E9","#E69F00"),
                    guide="none")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="", y="") +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14))
prior_schema
ggsave(prior_schema, file="../../../prior_schema.pdf", width=4, height=4)


predicate <- c("think", "know")
predicate_rating <- c(0.37, 0.84)
predicate_schema.df <- data.frame(predicate=predicate,rating=predicate_rating)
predicate_schema.df$predicate <- factor(predicate_schema.df$predicate, levels = rev(levels(factor(predicate_schema.df$predicate))))

predicate_schema <- ggplot(predicate_schema.df,
                       aes(x=predicate,
                           y=predicate_rating,
                           fill=predicate))+
  geom_bar(stat = "identity",width = 0.5,color="black")+
  geom_hline(yintercept=0.5,linetype='dashed', alpha=0.6,color="grey")+
  scale_fill_manual(values=c("#009E73", "#F0E442"),
                    guide="none")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="", y="") +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14))
predicate_schema
ggsave(predicate_schema, file="../../../predicate_schema.pdf", width=4, height=4)

qud <- c("JULIAN", "KNOW")
qud_rating <- c(0.27, 0.67)
qud_schema.df <- data.frame(qud=qud,rating=qud_rating)

qud_schema <- ggplot(qud_schema.df,
                     aes(x=qud,
                         y=qud_rating,
                         alpha=qud))+
  geom_bar(stat = "identity",width = 0.5,color="black")+
  geom_hline(yintercept=0.5,linetype='dashed', alpha=0.6,color="grey")+
  scale_alpha_manual(values=c(0.9, 0.4),
                    guide="none")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="", y="") +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14))
qud_schema
ggsave(qud_schema, file="../../../qud_schema.pdf", width=4, height=4)
