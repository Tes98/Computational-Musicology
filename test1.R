library(ggplot2)
library(tidyverse)
library(tidymodels)
library(plotly) # N.B. Requires the Cairo package
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)

#testing plots
motivation <- get_playlist_audio_features("","37i9dQZF1DXdxcBWuJkbcy")
personal <- get_playlist_audio_features("", "6XCAbThTYQGcAXguQYTLJV")

pers

total <-
  bind_rows(
    motivation %>% mutate(category = "Motivation Mix"),
    personal %>% mutate(category = "Wedstrijd voorbereiding")
  )

total %>% ggplot(aes(x = energy, color = category, fill = category)) +
  geom_histogram(position = 'dodge', binwidth = 0.1) 

total %>% ggplot(aes(y = energy, color = category)) +
  geom_boxplot()

total %>% ggplot(aes(y = danceability, color = category)) +
  geom_boxplot()

total %>% ggplot(aes(y = loudness, color = category)) +
  geom_boxplot()

# pitch chromatogram 

# Leas on life andy grammer
andy_grammer <-
  get_tidy_audio_analysis("4ufkuONjQMNR2fyXu1bO9w") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

#all I do is win dj Khaled
dj_khaled <-
  get_tidy_audio_analysis("12PNcnMsjsZ3eHm62t8hiy") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

both <- 
  bind_rows(
    andy_grammer %>% mutate(category = "Andy Grammer - Leas on life"),
    dj_khaled %>% mutate(category = "DJ Khaled - All I do is win")
  )

both %>%
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
  facet_wrap(~category)


# Self similarity matrix 

shivers <-
  get_tidy_audio_analysis("50nfwKoDiSYg8zOCREWAm5") %>% # Change URI.
  compmus_align(bars, segments) %>%                     # Change `bars`
  select(bars) %>%                                      #   in all three
  unnest(bars) %>%                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) %>%
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )

shivers %>%
  compmus_self_similarity(timbre, "euclidean") %>% 
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
  coord_fixed() +
  scale_fill_viridis_c(guide="none") +
  theme_classic() +
  labs(x = "", y = "")
