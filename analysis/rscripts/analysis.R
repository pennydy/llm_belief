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


###### Data ######
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")

# prior data
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


### projection data
projection.gpt4o.data <- read.csv("../../data/projection/projection_generate_system_gpt-4o.csv", header=TRUE) %>% 
  na.omit()
projection.gpt4.data <- read.csv("../../data/projection/projection_generate_system_gpt-4.csv", header=TRUE)
projection.gpt35.data <- read.csv("../../data/projection/projection_generate_system_gpt-3.5-turbo.csv", header=TRUE)

projection.all.model.data <- bind_rows(lst(projection.gpt4.data, projection.gpt4o.data, projection.gpt35.data), .id="model")

projection_p <- projection.all.model.data %>% 
  filter(embedded_type == "p") %>%
  mutate(model = case_when(model == "projection.gpt4.data" ~ "gpt-4",
                           model == "projection.gpt4o.data" ~ "gpt-4o",
                           model == "projection.gpt35.data" ~ "gpt-3.5-turbo")) %>% 
  select(model,verb,prior_type, item, prior, rating)

projection_not_p <- projection.all.model.data %>% 
  filter(embedded_type == "not_p") %>%
  mutate(model = case_when(model == "projection.gpt4.data" ~ "gpt-4",
                           model == "projection.gpt4o.data" ~ "gpt-4o",
                           model == "projection.gpt35.data" ~ "gpt-3.5-turbo")) %>% 
  select(model,verb,prior_type, item, prior, rating)

#### projection: embedded p, both facts
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

#### projection: embedded not p, both facts
projection_not_p_mean <- projection_not_p %>% 
  group_by(verb, model, prior_type) |>
  summarize(mean_rating = mean(rating),
            ci_low = ci.low(rating), # confidence interval
            ci_high = ci.high(rating)) |> 
  ungroup() |>
  mutate(YMin = mean_rating - ci_low,
         YMax = mean_rating + ci_high,
         verb = fct_reorder(verb, .x=mean_rating))

#### projection: embedded p and not p together
projection_all_mean <- bind_rows(lst(projection_p_mean,projection_not_p_mean), .id="embedded_type") %>% 
  mutate(embedded_type = ifelse(embedded_type == "projection_p_mean", "p", "not p")) %>% 
  mutate(embedded_type = fct_relevel(embedded_type, c("p", "not p")))



#### prior and projection
prior_projection_gpt35 <- rbind(prior.gpt35.data, projection.gpt35.data)

###### Plot ######
# prior
human_prior_p <- subset(prior_p, model == "human")
model_prior_p <- subset(prior_p, model != "human")
model_prior_p$facet <- model_prior_p$model
human_prior_p <- merge(human_prior_p,
                       data.frame(model = "human", 
                                  facet = unique(model_prior_p$facet)))
prior_p_plot <- rbind(human_prior_p, model_prior_p) %>% 
  mutate(is_model = ifelse(model=="human", "human", "model"))

prior_embedded_p_graph <- ggplot(data = prior_p_plot,
                           mapping = aes(y = embedded_content,
                                      x = rating,
                                      shape = is_model,
                                      color = prior_type)) +
  # geom_point(data=prior_p %>% 
  #              filter(model=="human"),
  #            mapping=aes(y=embedded_content,
  #                      x=rating,
  #                      color=prior_type),
  #            shape=17,size=4) +
  geom_point(size=4,alpha=0.6,position = position_jitter(w=0, h=0.17)) +
  geom_vline(xintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  labs(x = "Likelihood of p",
       y = "Embedded content",
       color = "prior",
       shape = "human or model") +
  # guides(shape="none") +
  facet_grid(. ~ facet) +
  scale_color_manual(values=cbPalette,
                    labels=c("high prior", "low prior"))
prior_embedded_p_graph
ggsave(prior_embedded_p_graph, file="../graphs/prior_models_facet.pdf", width=7, height=4)

# projection: p
##TODO: rank the belief rating (similar to the og pattern)
##TODO: add human data -> which file
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


## projection: not p
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


# projection: p and not p (side-by-side)
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

## prior ~ belief rating, line plot
prior_projection_p <- projection_p %>% 
  left_join(model_prior_p, by = c("model", "prior_type", "item")) %>% 
  rename(projection_rating = rating.x) %>% 
  rename(prior_rating = rating.y) %>% 
  select(model, verb, prior_type, item, projection_rating, prior_rating)


prior_projection_graph <- ggplot(data = prior_projection_p,
       aes(x=prior_rating,
           y=projection_rating)) +
  geom_point(alpha=0.4) +
  geom_smooth(method = "lm", fullrange=T) +
  # geom_point(data=combine_summary |> 
  #              filter(predicate %in% c("know","think","say","inform")), 
  #            aes(x=combined_prior,
  #                y=mean_ah_rating,
  #                fill=predicate),
  #            shape=21,size=2,color="black",stroke=1) +
  # geom_errorbar(data=combine_summary |> 
  #                 filter(predicate %in% c("know","think","say","inform")),
  #               aes(x=combined_prior,ymin=ah_YMin, ymax=ah_YMax),
  #               width=0.05,
  #               color="black") +
  facet_grid(. ~ model) +
  scale_x_continuous(name="Rating of prior belief in the embedded content", 
                     limits=c(0,1),
                     breaks=c(0,0.2,0.4,0.6,0.8,1)) + 
  scale_y_continuous(name="Mean attitude holder belief\nin the embedded content", limits=c(0,1)) + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
prior_projection_graph

prior_projection_verb_graph <- ggplot(data = prior_projection_p %>% 
                                        filter(verb != "polar"),
                                 aes(x=prior_rating,
                                     y=projection_rating,
                                     color=model)) +
  geom_point(alpha=0.4) +
  geom_smooth(method = "lm", fullrange=T) +
  # geom_point(data=combine_summary |> 
  #              filter(predicate %in% c("know","think","say","inform")), 
  #            aes(x=combined_prior,
  #                y=mean_ah_rating,
  #                fill=predicate),
  #            shape=21,size=2,color="black",stroke=1) +
  # geom_errorbar(data=combine_summary |> 
  #                 filter(predicate %in% c("know","think","say","inform")),
  #               aes(x=combined_prior,ymin=ah_YMin, ymax=ah_YMax),
  #               width=0.05,
  #               color="black") +
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


###### Graph ######
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