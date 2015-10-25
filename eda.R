library(tidyr)
library(dplyr)
library(ggplot2)


d <- read.csv("data/hunt_lipo_2006.csv", skip = 13, header = TRUE)
d <- dplyr::select(d, date:M.710)
d <- gather(d, group, prob, -date)
d$date <- as.integer(d$date)
d$group <- as.factor(d$group)
d$prob <- as.numeric(d$prob)
d <- d %>% filter(date <= 1647)

# normalize each line

# plots
d %>% ggplot(aes(x = date, y = prob, color = group)) +
  geom_line() + 
  theme_bw()

p <- d %>% ggplot(aes(x = date, y = prob)) +
  geom_line() + 
  facet_grid(group ~ .) +
  scale_y_continuous(breaks=NULL) + 
  theme_bw()

g <- d %>% group_by(group) %>%
  summarize(sigma = sqrt(sum(prob * (date - sum(date * prob))^2)), 
            mu = sum(date * prob))
  
