library(ggplot2)
library(tidyverse)
library(tidymodels)
library(plotly) # N.B. Requires the Cairo package
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)

sprinter_genres <- read.csv("Sprinters.csv")
long_genres <- read.csv("Long-distance.csv")

both_genres <-
  bind_rows(
    long_genres %>% mutate(category = "Long Distance swimmers"),
    sprinter_genres %>% mutate(category = "Sprinters")
  )

test3 <- both_genres %>% 
  group_by(category) %>%
  
  
ggplot(both_genres, aes(x=Parent.Genres)) + 
  geom_bar(position = "dodge") + 
  facet_wrap(~ category)

ggplot(sprinter_genres, aes(Parent.Genres)) + 
  geom_histogram()
