library(ggplot2)
library(tidyverse)
library(tidymodels)
library(plotly) # N.B. Requires the Cairo package
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)
library(purrr)
library(gghighlight)

long_dist <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "56vEaKq9VqFBlqtcXZAgn4"
  ) %>%
  add_audio_analysis()
sprinters <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "70wUKUusJoxhLNieGGRXpK"
  ) %>%
  add_audio_analysis()




jazz <-
  long_dist %>%
  mutate(Playlist = "Long-distance") %>%
  bind_rows(sprinters %>% mutate(Playlist = "Sprinters"))


p1 <- jazz %>%
  mutate(
    sections =
      map(
        sections,                                    # sections or segments
        summarise_at,
        vars(tempo, loudness, duration),             # features of interest
        list(section_mean = mean, section_sd = sd)   # aggregation functions
      )
  ) %>%
  unnest(sections) %>%
  ggplot(
    aes(
      x = tempo,
      y = tempo_section_sd,
      colour = Playlist,
      alpha = loudness
    )
  ) +
  geom_point(aes(size = duration / 60)) +
  geom_rug() +
  scale_colour_manual(values = c('#93B9D0', "#293352")) +
  theme_minimal() +
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Playlist",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  ) 
 
p1



