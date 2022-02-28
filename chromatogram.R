library(ggplot2)
library(tidyverse)
library(tidymodels)
library(plotly) # N.B. Requires the Cairo package
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)

lady_gaga <-
  get_tidy_audio_analysis("1HHeOs6zRdF8Ck58easiAY") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)


lady_gaga %>%
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


monti <-
  get_tidy_audio_analysis("6lo128WdLt7xP5Ejvsq1ym") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)


monti %>%
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


