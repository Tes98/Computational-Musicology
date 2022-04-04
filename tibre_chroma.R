library(ggplot2)
library(tidyverse)
library(tidymodels)
library(plotly) # N.B. Requires the Cairo package
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)
library(scales)
library(gridExtra)
library(grid)
library(readxl)
library(ggdendro)

bzt <-
  get_tidy_audio_analysis("0nLiqZ6A27jJri2VCalIUs") %>% # Change URI.
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

timbre_plot <- bzt %>%
  compmus_self_similarity(timbre, "cosine") %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  coord_fixed() +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(title = 'Timbre', x = "Timbre", y = "")


pitch_plot <- bzt %>%
  compmus_self_similarity(pitches, "cosine") %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  coord_fixed() +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(title = 'Pitch', x = "Pitch", y = "")

timbre_plot + theme(axis.title.x.top = element_text())
pitch_plot + theme(axis.title.x.top = element_text())


fig <- subplot(pitch_plot, timbre_plot, nrows=1, titleY=TRUE, titleX=TRUE, margin = 0.05) %>%
  layout(title = "Self-Similarity Matrices")
fig
