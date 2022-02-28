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
celine <-
  get_tidy_audio_analysis("0gsl92EMIScPGV1AU35nuD") %>%
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
    celine %>% mutate(category = "Celine Dion - All by myself"),
    piano %>% mutate(category = "Rachmaninov – Piano Concerto No.2 in C minor")
  )

plot1 <-
  total %>%
  compmus_gather_timbre() %>%
  ggplot(
    aes(
      x = basis,
      y = value,
      fill = category
    )
  ) +
  geom_boxplot(position = "dodge") +
  labs(x = "Spotify Timbre Coefficients", y = NULL) +
  theme_classic() 

plot1 %>%
  ggplotly() %>%
  layout(boxmode = "group")
