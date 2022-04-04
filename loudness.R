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




loud <-
  long_dist %>%
  mutate(Playlist = "Long-distance") %>%
  bind_rows(sprinters %>% mutate(Playlist = "Sprinters"))


loud %>%
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
  ggplot(aes(x=loudness, y=duration, fill = Playlist)) +
  geom_violin(alpha=0.6) +
  scale_fill_manual(values = c('#93B9D0', "#295590")) +
  theme_minimal() +
  labs(title="Violin plot of Loudness",x="Loudness(dBFS)", y = "Duration (seconds)")

mean_loud <- ddply(loud, "Playlist", summarise, grp.mean=mean(loudness))
mean_loud

mean_dur <- ddply(loud, "Playlist", summarise, grp.mean=mean(duration))
mean_dur

max_dur <- ddply(loud, "Playlist", summarise, grp.mean=max(duration))
min_dur <- ddply(loud, "Playlist", summarise, grp.mean=min(duration))
max_dur
min_dur
