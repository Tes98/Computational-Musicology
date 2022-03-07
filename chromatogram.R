library(ggplot2)
library(tidyverse)
library(tidymodels)
library(plotly) # N.B. Requires the Cairo package
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)

u2 <-
  get_tidy_audio_analysis("1Og8U5KRel9D51h4bmznlE") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

pachelbel <-
  get_tidy_audio_analysis("1c3GkbZBnyrQ1cm4TGHFrK") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)


u2 %>%
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) %>%
  compmus_gather_chroma() %>% 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "angular") +
  theme_minimal() +
  scale_fill_viridis_c()


pachelbel %>%
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) %>%
  compmus_gather_chroma() %>% 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "angular") +
  theme_minimal() +
  scale_fill_viridis_c()


