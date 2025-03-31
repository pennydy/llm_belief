graphCostPosteriors <- function(posteriors) {
  posteriors$Parameter <- relevel(posteriors$Parameter, ref = "colorCost")
  labels = c(colorCost = "color", sizeCost = "size")
  scale_value=1
  return(ggplot(posteriors, aes(x = value)) +
           geom_histogram(aes(y=..density..),
                          data=subset(posteriors, Parameter == "colorCost"),
                          binwidth = .02, colour="black", fill="white") +
           geom_histogram(aes(y=..density..),
                          data =subset(posteriors, Parameter == "sizeCost" ),
                          binwidth = .02, colour="black", fill="white") +
           geom_density(aes(y=..density..), alpha=.5,
                        data=subset(posteriors, Parameter == "colorCost"),
                        adjust = 1, fill="#FF6666")+
           geom_density(aes(y=..density..), alpha=.5,
                        data=subset(posteriors, Parameter == "sizeCost"),
                        adjust = 1, fill="#FF6666")+
           theme_bw() +
           ylab("Density") +
           xlab("Cost") +
           # xlim(0,2) +
           facet_grid(Parameter ~. , scales = 'free', labeller=labeller(Parameter = labels)) +
           theme(panel.border = element_rect(size=.2),
                 plot.margin = unit(x = c(0.05, 0.02, 0.05, 0.05), units = "in"),
                 panel.grid = element_line(size = .4),
                 axis.line        = element_line(colour = "black", size = .2),
                 axis.ticks       = element_line(colour = "black", size = .2),
                 axis.ticks.length = unit(2, "pt"),
                 axis.text.x        = element_text(size = 6 * scale_value, colour = "black",vjust=2),
                 axis.text.y        = element_text(size = 6 * scale_value, colour = "black",margin = margin(r = 0.3)),#,hjust=-5),
                 axis.title.x       = element_text(size = 6 * scale_value, margin = margin(t = .5)),
                 axis.title.y       = element_text(size = 6 * scale_value, margin = margin(r = .5)),
                 strip.text      = element_text(size = 6 * scale_value,margin=margin(t=4,r=4,b=4,l=4,unit="pt")))) 
}

graphPosteriors <- function(posteriors) {
  return(ggplot(posteriors, aes(x = value)) +
           theme_bw() +
           facet_wrap(~Parameter, scales = "free") +
           # geom_histogram())
           # geom_density())
           geom_density(alpha = 0.05) +
           xlab("Parameter value"))
}

graphJointPosteriors <- function(posteriors) {
  return(ggplot(posteriors %>%
                  filter(Parameter!="negCost") %>%
                  pivot_wider(names_from = Parameter, values_from = value), aes(x = alpha, y = embedCost)) +
           theme_bw() +
           # facet_wrap(~Parameter, scales = "free") +
           geom_tile())
  # geom_density(alpha = 0.05))
}


graphPredictives <- function(predictives, df) {
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
  
  predictives$type = "prediction"
  df$type = "observation"
  
  predictives <- rbind(predictives, 
                       df) %>%
    spread(type, prob)
  
  correlation <- predictives %>% 
    # group_by(polarity) %>%
    summarize(r = cor(observation, prediction),
              r_squared = round(r ^ 2, digits=3)) %>% 
    ungroup()
  
  ggplot(predictives, aes(x = prediction, y = observation)) +
    # geom_point(aes(shape=polarity, color = predicate)) +
    scale_color_manual(values=cbPalette[2:4]) +
    scale_fill_manual(values=cbPalette[2:4]) +
    geom_point(aes(color = predicate),alpha=0.8) +
    # geom_smooth(method = "lm", fullrange=TRUE) + 
    theme_bw() +
    geom_text(data = correlation,
              mapping = aes(x=-Inf, y=-Inf,label = sprintf("r^2 = %f", r_squared)),
              hjust = -.1,
              vjust = -1) +
    # annotate("text", x = 0.75, y = 0, label = sprintf("r = %f", cor(predictives$prediction,
    #                                                                 predictives$observation))) +
    facet_grid(.~polarity) +
    # scale_color_manual(values=cbPalette[2:4], guide="none") +
    # scale_fill_manual(values=cbPalette[2:4], guide="none") +
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,by=.25)) + 
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=.25))
}

graphPredictives_ah <- function(predictives, df) {
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
  
  predictives$type = "prediction"
  df$type = "observation"
  
  predictives <- rbind(predictives, 
                       df) %>%
    spread(type, prob)
  
  correlation <- predictives %>% 
    # group_by(polarity) %>%
    summarize(r = cor(observation, prediction),
              r_squared = round(r ^ 2, digits=3)) %>% 
    ungroup()
  
  ggplot(predictives, aes(x = prediction, y = observation)) +
    # geom_point(aes(color = predicate,
    #                shape=polarity)) +
    geom_point(aes(color = predicate),alpha=0.8)+
    # geom_smooth(method = "lm", fullrange=TRUE) + 
    theme_bw() +
    geom_text(data = correlation,
              mapping = aes(x=-Inf, y=-Inf,label = sprintf("r^2 = %f", r_squared)),
              hjust = -.1,
              vjust = -1) +
    # annotate("text", x = 0.75, y = 0, label = sprintf("r = %f", cor(predictives$prediction,
    #                                                                 predictives$observation))) +
    facet_grid(.~polarity) +
    scale_color_manual(values=cbPalette[3:4]) +
    scale_fill_manual(values=cbPalette[3:4]) + 
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,by=.25)) + 
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=.25))
}


graphJointPosteriors <- function(posteriors) {
  return(ggplot(posteriors %>%
                  filter(Parameter!="negCost") %>%
                  pivot_wider(names_from = Parameter, values_from = value), aes(x = alpha, y = embedCost)) +
           theme_bw() +
           # facet_wrap(~Parameter, scales = "free") +
           geom_tile())
  # geom_density(alpha = 0.05))
}


graphPredictives_test <- function(predictives, df) {
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
  
  training_items <- c( "Charley_H", "Charley_L","Frank_H", "Frank_L","Danny_H", "Danny_L", "Emily_H", "Emily_L","Emma_H", "Emma_L", "Grace_H", "Grace_L", "Isabella_H","Isabella_L","Jackson_H","Jackson_L", "Jayden_H","Jayden_L","Jon_H","Jon_L","Josh_H","Josh_L","Josie_H","Josie_L","Julian_H", "Julian_L","Mary_H","Mary_L","Mia_H","Mia_L","Olivia_H", "Olivia_L")
  testing_items <- c("Owen_H", "Owen_L", "Sophia_H", "Sophia_L", "Tony_H", "Tony_L", "Zoe_H", "Zoe_L")
  
  predictives$type = "prediction"
  df$type = "observation"
  
  predictives <- rbind(predictives, 
                       df) %>%
    spread(type, prob) %>% 
    mutate(type = ifelse(item %in% training_items, "training", "testing"))
  
  correlation <- predictives %>% 
    group_by(type) %>%
    summarize(r = cor(observation, prediction),
              r_squared = round(r ^ 2, digits=3)) %>% 
    ungroup()
  all_corr <- round(cor(predictives$prediction,
                  predictives$observation)^2, digits=3)
  
  ggplot(predictives, aes(x = prediction, y = observation)) +
    # geom_point(aes(shape=polarity, color = predicate)) +
    geom_point(aes(color = predicate), alpha=0.8) +
    # geom_smooth(method = "lm", fullrange=TRUE) + 
    geom_abline(slope=1,linetype="dashed",alpha=0.4)+
    theme_bw() +
    geom_text(data = correlation,
              mapping = aes(x=-Inf, y=-Inf,label = sprintf("r^2 = %f", r_squared)),
              hjust = -.1,
              vjust = -1,
              size=3) +
    facet_grid(type~polarity) +
    # annotate("text", x = 0.75, y = 0, label = sprintf("r^2 = %f", all_corr)) +
    # labs(tag=sprintf("r = %f", cor(predictives$prediction,
    #                                        predictives$observation))) +
    scale_color_manual(values=cbPalette[2:4]) +
    scale_fill_manual(values=cbPalette[2:4]) + 
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,by=.25)) + 
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=.25)) 
    # theme(legend.position="top")
}


graphPredictives_ah_test <- function(predictives, df) {
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
  
  
  training_items <- c( "Charley_H", "Charley_L","Frank_H", "Frank_L","Danny_H", "Danny_L", "Emily_H", "Emily_L","Emma_H", "Emma_L", "Grace_H", "Grace_L", "Isabella_H","Isabella_L","Jackson_H","Jackson_L", "Jayden_H","Jayden_L","Jon_H","Jon_L","Josh_H","Josh_L","Josie_H","Josie_L","Julian_H", "Julian_L","Mary_H","Mary_L","Mia_H","Mia_L","Olivia_H", "Olivia_L")
  testing_items <- c("Owen_H", "Owen_L", "Sophia_H", "Sophia_L", "Tony_H", "Tony_L", "Zoe_H", "Zoe_L")
  
  
  predictives$type = "prediction"
  df$type = "observation"
  
  predictives <- rbind(predictives, 
                       df) %>%
    spread(type, prob) %>% 
    mutate(type = ifelse(item %in% training_items, "training", "testing"))
  
  correlation <- predictives %>% 
    group_by(type) %>%
    summarize(r = cor(observation, prediction),
              r_squared = round(r ^ 2, digits=3)) %>% 
    ungroup()
  
  ggplot(predictives, aes(x = prediction, y = observation)) +
    # geom_point(aes(color = predicate,
    #                shape=polarity)) +
    geom_point(aes(color = predicate), alpha=.8)+
    # geom_smooth(method = "lm", fullrange=TRUE) + 
    geom_abline(slope=1,linetype="dashed",alpha=0.4)+
    theme_bw() +
    geom_text(data = correlation,
              mapping = aes(x=-Inf, y=-Inf,label = sprintf("r^2 = %f", r_squared)),
              hjust = -.1,
              vjust = -1) +
    # annotate("text", x = 0.75, y = 0, label = sprintf("r = %f", cor(predictives$prediction,
    #                                                                 predictives$observation))) +
    facet_grid(type~polarity) +
    scale_color_manual(values=cbPalette[3:4]) +
    scale_fill_manual(values=cbPalette[3:4]) + 
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,by=.25)) + 
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=.25)) 
    # theme(legend.position="top")
}


graphJointPosteriors <- function(posteriors) {
  return(ggplot(posteriors %>%
                  filter(Parameter!="negCost") %>%
                  pivot_wider(names_from = Parameter, values_from = value), aes(x = alpha, y = embedCost)) +
           theme_bw() +
           # facet_wrap(~Parameter, scales = "free") +
           geom_tile())
  # geom_density(alpha = 0.05))
}

graphPredictives_cg <- function(predictives, df) {
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
  
  training_items <- c("Frank_H", "Frank_L","Danny_H", "Danny_L", "Emily_H", "Emily_L","Isabella_H","Isabella_L","Jon_H","Jon_L", "Jayden_H", "Jayden_L")
  testing_items <- c("Charley_H", "Charley_L","Emma_H", "Emma_L","Grace_H", "Grace_L", "Jackson_H","Jackson_L", "Owen_H", "Owen_L", "Sophia_H", "Sophia_L", "Tony_H", "Tony_L", "Zoe_H", "Zoe_L","Josh_H","Josh_L","Josie_H","Josie_L","Julian_H", "Julian_L","Mary_H","Mary_L","Mia_H","Mia_L","Olivia_H", "Olivia_L")
  
  predictives$type = "prediction"
  df$type = "observation"
  
  predictives <- rbind(predictives, 
                       df) %>%
    spread(type, prob) %>% 
    mutate(type = ifelse(item %in% training_items, "training", "testing"))
  
  correlation <- predictives %>% 
    group_by(type) %>%
    summarize(r = cor(observation, prediction),
              r_squared = round(r ^ 2, digits=3)) %>% 
    ungroup()
  all_corr <- round(cor(predictives$prediction,
                        predictives$observation)^2, digits=3)
  
  ggplot(predictives, aes(x = prediction, y = observation)) +
    # geom_point(aes(shape=polarity, color = predicate)) +
    geom_point(aes(color = predicate), alpha=0.8) +
    # geom_smooth(method = "lm", fullrange=TRUE) + 
    geom_abline(slope=1,linetype="dashed",alpha=0.4)+
    theme_bw() +
    geom_text(data = correlation,
              mapping = aes(x=-Inf, y=-Inf,label = sprintf("r^2 = %f", r_squared)),
              hjust = -.1,
              vjust = -1,
              size=3) +
    facet_grid(type~polarity) +
    # annotate("text", x = 0.75, y = 0, label = sprintf("r^2 = %f", all_corr)) +
    # labs(tag=sprintf("r = %f", cor(predictives$prediction,
    #                                        predictives$observation))) +
    scale_color_manual(values=cbPalette[2:4]) +
    scale_fill_manual(values=cbPalette[2:4]) + 
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,by=.25)) + 
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=.25)) 
  # theme(legend.position="top")
}


