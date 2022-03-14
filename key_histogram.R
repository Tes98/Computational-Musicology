library(ggplot2)
library(tidyverse)
library(tidymodels)
library(plotly) # N.B. Requires the Cairo package
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)

long_distance <- get_playlist_audio_features("","56vEaKq9VqFBlqtcXZAgn4")
sprinters <- get_playlist_audio_features("", "70wUKUusJoxhLNieGGRXpK")


total <-
  bind_rows(
    long_distance %>% mutate(category = "Long Distance swimmers"),
    sprinters %>% mutate(category = "Sprinters")
  )

key_histogram <- total %>%
  ggplot(aes(x=key, fill=category)) +
  geom_histogram(position = 'dodge', binwidth = 0.55) +
  theme_classic()

ggplotly(key_histogram)
