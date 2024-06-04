library(lme4)
library(dplyr)
library(emmeans)
library(tidyverse)


theme_set(theme_bw())
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 


###### Data ######
# get the data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")
gpt4o.data <- read.csv("../../output_generate_system_gpt-4o.csv", header=TRUE) %>% 
  na.omit()
gpt4.data <- read.csv("../../output_generate_system_gpt-4.csv", header=TRUE)
gpt35.data <- read.csv("../../output_generate_system_gpt-3.5-turbo.csv", header=TRUE)
human.data <- read.csv("../../data/2_listenerSpeakerAH-trials.csv", header=TRUE)


embedded_p <- bind_rows(lst(gpt4.data, gpt4o.data,gpt35.data), .id="model")

embedded_p <- embedded_p %>% 
  filter(embedded == "p") %>%
  mutate(model = case_when(model == "gpt4.data" ~ "gpt-4",
                           model == "gpt4o.data" ~ "gpt-4o",
                           model == "gpt35.data" ~ "gpt-3.5-turbo")) %>% 
  select(model,prior_type, item, embedded_content, prior, rating, distribution) 
  mutate(item = fct_relevel(item, c("Mary","Charley","Grace","Jackson","Jon","Tony","Owen",
                                    "Zoe","Josie","Danny","Jayden","Emma","Frank","Olivia",
                                    "Sophia","Julian","Mia","Emily","Josh","Isabella")))

embedded_p_graph <- ggplot(data = embedded_p,
                        mapping = aes(y = embedded_content,
                                      x = rating,
                                      shape = model,
                                      color = prior_type)) +
  geom_point(size=4,alpha=0.6,position = position_jitter(w=0, h=0.17)) +
  geom_vline(xintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  labs(x = "Likelihood of p",
       y = "Embedded content",
       color = "prior") +
  scale_color_manual(values=cbPalette,
                    labels=c("high prior", "low prior"))
embedded_p_graph

####### scripts from human participants analysis


# exclude bot check and select only the relevant columns
df.data.clean <- df.data |>
  filter(slide_number_in_experiment != 1) |>
  select(workerid, belief_response, certainty_response, content, prior, trigger, trigger_class)

# calculate the ratings for the alternative content and specify the belief
# of the certainty rating
df.data.all <- df.data.clean |>
  mutate(utterance_type = gsub("[a-z]+_", "\\1", trigger)) |> # either p (pos) or not p (neg) 
  mutate(predicate = gsub("(.*)_[a-z]+", "\\1", trigger)) |>
  mutate(not_p = ifelse(utterance_type == "neg", 
                        belief_response,
                        1 - belief_response),
         p = ifelse(utterance_type == "neg",
                    1 - belief_response,
                    belief_response)) |>
  mutate(certainty_content = case_when(
    belief_response > 0.5 & utterance_type == "neg" ~ "not_p",
    belief_response > 0.5 & utterance_type != "neg" ~ "p",
    belief_response <= 0.5 & utterance_type == "neg" ~ "p",
    belief_response <= 0.5 & utterance_type != "neg" ~ "not_p")) 

df.data.all <- df.data.all %>% 
  pivot_longer(cols = c(p, not_p),
               names_to = "belief_content",
               values_to = "belief_rating") |>
  # to reorder the predicate and utterance_type for graph
  mutate(predicate = fct_relevel(predicate, "MC","simple","think","know","say","confirm"),
         utterance_type = fct_relevel(utterance_type, "MC","polar","pos", "neg"))

## Data exclusion
group_mc_mean <- mean(df.data.all$belief_response[df.data.all$trigger == "MC"])
group_mc_sd <- sd(df.data.all$belief_response[df.data.all$trigger == "MC"])
exclusion_criteria <- group_mc_mean + 2*group_mc_sd
excludeid <- df.data.all |>
  filter(trigger == "MC") |>
  group_by(workerid) |>
  summarise(mean_belief_response = mean(belief_response)) |>
  mutate(exclude = ifelse(mean_belief_response >= exclusion_criteria, "yes", "no")) |>
  filter(exclude == "yes") |>
  select(workerid)
# workerid 59 did not respond to the native language question
excludeid <- excludeid$workerid
excludeid <- c(59)
df.data.summary <- df.data.all |>
  filter(!workerid %in% excludeid)

length(unique(df.data.summary$workerid))

table(df.data.summary$content, df.data.summary$predicate)


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

## BELIEF RATINGS
# summary of belief ratings
belief_summary <- df.data.summary |>
  group_by(predicate, belief_content, utterance_type) |>
  summarize(mean_belief_rating = mean(belief_rating),
            ci_low = ci.low(belief_rating), # confidence interval
            ci_high = ci.high(belief_rating)) |> 
  ungroup() |>
  mutate(YMin = mean_belief_rating - ci_low,
         YMax = mean_belief_rating + ci_high)
belief_summary
write.csv(belief_summary, "../results/belief_summary.csv" , row.names = FALSE)


# by the type of embedded clause (pos, neg, or simple polar)
belief_by_p <- ggplot(data = belief_summary |>
                        filter(belief_content == "p"),
                      mapping = aes(x = predicate,
                                    y = mean_belief_rating,
                                    fill = predicate)) +
  geom_bar(stat="identity",
           position=dodge) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=dodge,
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(. ~ utterance_type,
             labeller = utterance_labeller, scales="free_x") +
  labs(x = "Predicate",
       y = "Mean belief in p",
       fill = "Predicate") +
  scale_fill_manual(values=cbPalette,
                    labels=c("Filler", "Polar", "think", "know", "say", "confirm", "inform")) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank())
belief_by_p
ggsave(belief_by_p, file="../graphs/belief_by_p.pdf", width=6, height=3)

# by predicate
belief_by_predicate <- ggplot(data = belief_summary |>
                        filter(belief_content == "p") |>
                        mutate(utterance_type = ifelse(as.character(utterance_type) %in% c("MC","polar"), "pos", as.character(utterance_type))),
                      mapping = aes(x = belief_content,
                                    y = mean_belief_rating,
                                    alpha = utterance_type,
                                    fill = predicate)) +
  geom_bar(stat="identity",
           position=dodge,
           color="black") +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=dodge,
                width=.2,
                color="black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  labs(y = "Mean belief in p") +
  scale_alpha_discrete(range=c(.3,.9), # include both p and not p (more opaque). 
                       labels=c("not p", "p"),
                       name="Embedded\ncontent") +
  scale_fill_manual(values=cbPalette,
                    labels=c("Filler", "Polar", "think", "know", "say", "confirm", "inform"),
                    guide="none") +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank()) 
belief_by_predicate
ggsave(belief_by_predicate, file="../graphs/belief_by_predicate.pdf", width=5, height=3)

# by content and prior
belief_prior_summary <- df.data.summary |>
  group_by(predicate, belief_content, utterance_type, content, prior) |>
  summarize(mean_belief_rating = mean(belief_rating),
            count = n())|> 
  ungroup()

count_content <- df.data.summary |>
  filter(utterance_type != "MC") |>
  mutate(utterance_type = ifelse(as.character(utterance_type) == "polar", "pos", as.character(utterance_type))) |>
  group_by(utterance_type, content, prior) |>
  summarize(count = n()) |>
  mutate(weighted_prior = prior*count) |>
  group_by(utterance_type) |>
  mutate(weighted_prior_all = sum(weighted_prior) / sum(count),
         unweighted_prior_all = sum(prior) / 12)

write.csv(belief_prior_summary, "../results/belief_prior_summary.csv" , row.names = FALSE)


belief_prior_by_predicate <- ggplot(data = belief_prior_summary |>
                                     filter(belief_content == "p",
                                            predicate != "MC") |>
                                     mutate(utterance_type = ifelse(as.character(utterance_type) %in% c("MC","polar"), "pos", as.character(utterance_type))),
                                   mapping = aes(x = prior,
                                                 y = mean_belief_rating,
                                                 color = predicate)) +
  geom_point(aes(size=count)) +
  geom_smooth(method="lm") +
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_color_manual(values=cbPalette[2:7],
                    labels=c("Polar", "think", "know", "say", "confirm", "inform"),guide="none") +
  scale_size(range = c(0.5, 2),
             name = "Number of\nratings") +
  scale_y_continuous(limits = c(0,1)) + 
  scale_x_continuous(guide = guide_axis(angle = 45)) +
  labs(x = "Prior belief in p",
       y = "Mean belief in p")
belief_prior_by_predicate
ggsave(belief_prior_by_predicate, file="../graphs/belief_prior_by_predicate.pdf", width=6, height=3)

## CERTAINTY RATING
# certainty ratings (added by judith)
certainty_summary <- df.data.summary |>
  # penny, in the following line we're grouping by certainty_content -- this only makes sense if we first exclude all rows where the belief value was inferred instead of given, so we only retain the rows where participants actually gave a certainty rating for the content of that row. eg, if utterance was "pos", belief response > .5, we want to keep the row where certainty_content is "p", but not where it's "not p". similarly, if utterance was "neg", belief response < .5,  we only want to keep the row where certainty_content is "not p", etc.
  # that makes sense. The certainty_content column only keeps track of the content to which the participant gave a certainty rating. So for a given trail, the two belief_contents (one given, one inferred) map a to the same certainty_content. To get the belief rating for that content, we can just include rows that have the same certainty_content and belief_content value.
  filter(certainty_content == belief_content) |>
  group_by(predicate, certainty_content, utterance_type) |>
  summarize(mean_certainty_rating = mean(certainty_response),
            ci_low=ci.low(certainty_response),
            ci_high=ci.high(certainty_response),
            count = n()) |>
  ungroup() |>
  mutate(YMin = mean_certainty_rating - ci_low,
         YMax = mean_certainty_rating + ci_high) |>
  relocate(count, .after = utterance_type)
certainty_summary
write.csv(certainty_summary, "../results/certainty_summary_1.csv" , row.names = FALSE)

marginalized_certainty <- df.data.summary |>
  filter(certainty_content == belief_content,
         predicate != "MC") |>
  group_by(predicate, utterance_type) |>
  summarize(mean_certainty_rating = mean(certainty_response),
            ci_low=ci.low(certainty_response),
            ci_high=ci.high(certainty_response)) |>
  ungroup() |>
  mutate(YMin = mean_certainty_rating - ci_low,
         YMax = mean_certainty_rating + ci_high)
marginalized_certainty
write.csv(marginalized_certainty, "../results/marginal_certainty_summary.csv" , row.names = FALSE)

# by the type of embedded clause (pos, neg, or simple polar)
certainty_by_p <- ggplot(data = certainty_summary,
                      mapping = aes(x = certainty_content,
                                    y = mean_certainty_rating,
                                    fill = predicate)) +
  geom_bar(stat="identity", 
           position=dodge) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=dodge,
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(. ~ utterance_type,
             labeller = utterance_labeller, scales="free_x") +
  labs(x = "Rated content",
       y = "Mean certainty of the rated content",
       fill = "Predicate") +
  # scale_alpha_discrete(range=c(.3,.9),name="Embedded\ncontent") +
  scale_fill_manual(values=cbPalette,
                    labels=c("Filler", "Polar", "think", "know", "say", "confirm", "inform")) # add guide="none" later
certainty_by_p
ggsave(certainty_by_p, file="../graphs/certainty_by_p.pdf", width=6, height=3)


# by predicate
certainty_by_predicate <- ggplot(data = certainty_summary |>
                                   # filter(!utterance_type %in% c("MC")) |>
                                   mutate(utterance_type = ifelse(as.character(utterance_type) %in% c("MC","polar"), "pos", as.character(utterance_type))),
                                 mapping = aes(x = certainty_content,
                                               y = mean_certainty_rating,
                                               alpha = utterance_type,
                                               color = predicate)) +
  geom_point(aes(size=count),
             stat="identity",
             position=dodge,
             show.legend = FALSE) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=dodge,
                width=.2,
                color="black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_alpha_discrete(range=c(.3,.8), # include both p and not p (more opaque). 
                       labels=c("not p", "p"),
                       name="Embedded\ncontent") +
  scale_color_manual(values=cbPalette,
                    labels=c("Filler", "Polar", "think", "know", "say", "confirm", "inform"),
                    guide="none") +
  labs(x = "Rated content",
       y = "Mean certainty of the rated content") +
  scale_y_continuous(limits = c(0,1))
  # +scale_x_discrete(guide = guide_axis(angle = 45)) 
certainty_by_predicate
ggsave(certainty_by_predicate, file="../graphs/certainty_by_predicate.pdf", width=6, height=3)

## BELIEF AND CERTAINTY
overall_certainty_belief_summary <- df.data.summary |>
  filter(certainty_content == belief_content) |>
  group_by(predicate) |>
  summarize(mean_certainty_rating = mean(certainty_response),
            mean_belief_rating = mean(belief_rating),
            certainty_ci_low = ci.low(certainty_response),
            certainty_ci_high = ci.high(certainty_response),
            belief_ci_low = ci.low(belief_rating),
            belief_ci_high = ci.high(belief_rating),
            count = n()) |>
  ungroup() |>
  mutate(certainty_YMin = mean_certainty_rating - certainty_ci_low,
         certainty_YMax = mean_certainty_rating + certainty_ci_high,
         belief_YMin = mean_belief_rating - belief_ci_low,
         belief_YMax = mean_belief_rating + belief_ci_high)

overall_certainty_belief_summary
write.csv(overall_certainty_belief_summary, "../results/overall_certainty_belief_summary.csv" , row.names = FALSE)

overall_belief_certainty <- ggplot(data = overall_certainty_belief_summary,
                                   mapping = aes(x = mean_belief_rating,
                                                 y = mean_certainty_rating,
                                                 color = predicate)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin = certainty_YMin,ymax = certainty_YMax)) + 
  geom_errorbarh(aes(xmin = belief_YMin,xmax = belief_YMax)) +
  scale_color_manual(values=cbPalette,
                     labels=c("Filler", "Polar", "think", "know", "say", "confirm", "inform")) + 
  labs(x = "Mean belief of the rated content",
       y = "Mean certainty of the rated content") 
overall_belief_certainty
ggsave(overall_belief_certainty, file="../graphs/overall_belief_certainty.pdf")

certainty_belief_summary <- df.data.summary |>
  filter(certainty_content == belief_content)

certainty_belief_all <- ggplot(data = certainty_belief_summary,
                                      mapping = aes(x = belief_rating,
                                                    y = certainty_response,
                                                    color = predicate)) +
  geom_point(alpha=0.4) +
  geom_smooth(fullrange=TRUE,
              method = "lm") +
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_color_manual(values=cbPalette,
                     labels=c("Filler", "Polar", "think", "know", "say", "confirm", "inform"), 
                     guide="none") + 
  labs(x = "Mean belief of the rated content",
       y = "Mean certainty of the rated content") +
  scale_x_continuous(limits = c(0.5,1),
                     guide = guide_axis(angle = 90)) +
  scale_y_continuous(limits = c(0,1))
certainty_belief_all
ggsave(certainty_belief_all, file="../graphs/certainty_belief_all.pdf",width=6, height=3)

certainty_belief_by_content_summary <- certainty_belief_summary |>
  group_by(content, predicate) |>
  summarise(mean_content_belief = mean(belief_rating),
            mean_content_certainty = mean(certainty_response)) |>
  ungroup()

certainty_belief_by_content <- ggplot(data = certainty_belief_by_content_summary,
       mapping = aes(x = mean_content_belief,
                     y = mean_content_certainty,
                     color = predicate)) +
  geom_point() +
  geom_smooth(fullrange=TRUE,
              method = "lm") +
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_color_manual(values=cbPalette,
                     labels=c("Filler", "Polar", "think", "know", "say", "confirm", "inform"),
                     guide="none") +
  labs(x = "Mean belief of the content",
       y = "Mean certainty of the rated content") +
  scale_x_continuous(guide = guide_axis(angle = 90))  +
  scale_y_continuous(limits = c(0,1))
certainty_belief_by_content
ggsave(certainty_belief_by_content, file="../graphs/certainty_belief_by_content.pdf",width=6, height=3)



# certainty rating when the belief content is the same as the embedded content
certainty_summary_alt <- df.data.summary |>
  filter((certainty_content == "not_p" & utterance_type == "neg") |
           (certainty_content == "p" & utterance_type != "neg")) |>
  filter((belief_content == "not_p" & utterance_type == "neg") |
           (belief_content == "p" & utterance_type != "neg")) |>
  group_by(predicate, certainty_content, utterance_type) |>
  summarize(mean_certainty_rating = mean(certainty_response),
            ci_low=ci.low(certainty_response),
            ci_high=ci.high(certainty_response),
            count = n()) |>
  ungroup() |>
  mutate(YMin = mean_certainty_rating - ci_low,
         YMax = mean_certainty_rating + ci_high) |>
  relocate(count, .after = utterance_type)
certainty_summary_alt

certainty_combine_summary_alt <- df.data.summary |>
  filter((certainty_content == "not_p" & utterance_type == "neg") |
           (certainty_content == "p" & utterance_type != "neg")) |>
  filter((belief_content == "not_p" & utterance_type == "neg") |
           (belief_content == "p" & utterance_type != "neg")) |>
  group_by(predicate) |>
  summarize(mean_certainty_rating = mean(certainty_response),
            ci_low=ci.low(certainty_response),
            ci_high=ci.high(certainty_response),
            count = n()) |>
  ungroup() |>
  mutate(YMin = mean_certainty_rating - ci_low,
         YMax = mean_certainty_rating + ci_high) |>
  mutate(certainty_content = "same")
certainty_combine_summary_alt


certainty_combine_alt_by_predicate <- ggplot(data = certainty_combine_summary_alt,
                                 mapping = aes(x = certainty_content,
                                               y = mean_certainty_rating,
                                               color = predicate)) +
  geom_point(aes(size=count),
             stat="identity",
             position=dodge,
             show.legend = FALSE) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position="dodge2",
                width=.2,
                color="black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_color_manual(values=cbPalette,
                     labels=c("Filler", "Polar", "think", "know", "say", "confirm", "inform"),
                     guide="none") +
  labs(y = "Mean certainty of\nthe embedded content") +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank()) +
  scale_y_continuous(limits = c(0,1))
# +scale_x_discrete(guide = guide_axis(angle = 45)) 
certainty_combine_alt_by_predicate
ggsave(certainty_combine_alt_by_predicate, file="../graphs/certainty_comebine_alt_by_predicate.pdf", width=6, height=3)

certainty_alt_by_predicate <- ggplot(data = certainty_summary_alt |>
                                       # filter(!utterance_type %in% c("MC")) |>
                                       mutate(utterance_type = ifelse(as.character(utterance_type) %in% c("MC","polar"), "pos", as.character(utterance_type))),
                                     mapping = aes(x = certainty_content,
                                                   y = mean_certainty_rating,
                                                   alpha = utterance_type,
                                                   color = predicate)) +
  geom_point(aes(size=count),
             stat="identity",
             position=dodge,
             show.legend = FALSE) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position="dodge2",
                width=.2,
                color="black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_alpha_discrete(range=c(.3,.8), # include both p and not p (more opaque). 
                       labels=c("not p", "p"),
                       name="Embedded\ncontent") +
  scale_color_manual(values=cbPalette,
                     labels=c("Filler", "Polar", "think", "know", "say", "confirm", "inform"),
                     guide="none") +
  labs(x = "Rated content",
       y = "Mean certainty of the embedded content") +
  scale_y_continuous(limits = c(0,1))
# +scale_x_discrete(guide = guide_axis(angle = 45)) 
certainty_alt_by_predicate
ggsave(certainty_alt_by_predicate, file="../graphs/certainty_alt_by_predicate.pdf", width=6, height=3)


###### Analysis ######
df.belief_data <- df.data.summary |>
  filter(predicate %in% c("know", "think", "simple"),
         # trigger_class == "Critical",
         # (utterance_type == "neg" & belief_content == "not_p") |
         #   (utterance_type != "neg" & belief_content == "p"))
         belief_content == "p")
belief_model <- lmer(belief_response ~  predicate + (predicate | workerid) + (predicate | content),
                     df.belief_data)

joint_tests(belief_model)
summary(belief_model)


df.certainty_data <- df.data.summary |>
  filter(
    predicate %in% c("know", "think", "simple"),
         # trigger_class == "Critical",
         belief_content == certainty_content)

certainty_model <- lmer(certainty_response ~ predicate + (predicate | workerid) + (predicate | content),
                        df.certainty_data)
joint_tests(certainty_model)
summary(certainty_model)

cor(df.certainty_data$certainty_response, df.certainty_data$belief_rating,
    method = "spearman")

