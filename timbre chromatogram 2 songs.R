library(ggplot2)
library(tidyverse)
library(tidymodels)
library(plotly) # N.B. Requires the Cairo package
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)

#kijken naar timbre
# All by myself van Celine Dion
Eric <-
  get_tidy_audio_analysis("0AQqrtK1pULuwZUXhwaaDz") %>%
  compmus_align(bars, segments) %>% 
  select(bars) %>%
  unnest(bars) %>%
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"
      )
  )

#Rachmaninov – Piano Concerto No.2 in C minor
piano <-
  get_tidy_audio_analysis("3Q6o2IvSohjJkMgGdKqEbE") %>%
  compmus_align(bars, segments) %>% 
  select(bars) %>%
  unnest(bars) %>%
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"
      )
  )

total <-
  bind_rows(
    celine %>% mutate(category = "Eric Carmen - All by myself"),
    piano %>% mutate(category = "Rachmaninov – Piano Concerto No.2 in C minor")
  )


total %>%
  compmus_gather_timbre() %>%
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +                              
  theme_classic() +
  facet_wrap(~category)

