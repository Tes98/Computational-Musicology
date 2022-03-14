library(ggplot2)
library(tidyverse)
library(tidymodels)
library(plotly) # N.B. Requires the Cairo package
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)
library(purrr)

long_dist <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "56vEaKq9VqFBlqtcXZAgn4"
  ) %>%
  slice(1:30) %>%
  add_audio_analysis()
sprinters <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "70wUKUusJoxhLNieGGRXpK"
  ) %>%
  slice(1:30) %>%
  add_audio_analysis()
jazz <-
  long_dist %>%
  mutate(genre = "Long-Distance") %>%
  bind_rows(sprinters %>% mutate(genre = "Sprinters"))


jazz %>%
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
      colour = genre,
      alpha = loudness
    )
  ) +
  geom_point(aes(size = duration / 60)) +
  geom_rug() +
  theme_minimal() +
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Genre",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  )
