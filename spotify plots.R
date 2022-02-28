library(ggplot2)
library(tidyverse)
library(tidymodels)
library(plotly) # N.B. Requires the Cairo package
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)

gaga <-
  get_tidy_audio_analysis("1HHeOs6zRdF8Ck58easiAY") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)


monti <- get_tidy_audio_analysis(c("6lo128WdLt7xP5Ejvsq1ym")) %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

monti




