library(ggplot2)
library(tidyverse)
library(tidymodels)
library(plotly) # N.B. Requires the Cairo package
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)

## The Tallis Scholars
tallis <-
  get_tidy_audio_analysis("2J3Mmybwue0jyQ0UVMYurH") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)
## La Chapelle Royale
chapelle <-
  get_tidy_audio_analysis("4ccw2IcnFt1Jv9LqQCOYDi") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)


## Lady Gaga
gaga <-
  get_tidy_audio_analysis("1HHeOs6zRdF8Ck58easiAY") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)


## Vittorio Monti
monti <-
  get_tidy_audio_analysis("6lo128WdLt7xP5Ejvsq1ym") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

compmus_long_distance(
  gaga %>% mutate(pitches = map(pitches, compmus_normalise, "chebyshev")),
  monti %>% mutate(pitches = map(pitches, compmus_normalise, "chebyshev")),
  feature = pitches,
  method = "euclidean"
) %>%
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_equal() +
  labs(x = "Lady Gaga", y = "Vittorio Monti") +
  theme_minimal() +
  scale_fill_viridis_c(guide = NULL)


