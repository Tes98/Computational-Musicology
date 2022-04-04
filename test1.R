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


#testing plots
long_distance <- get_playlist_audio_features("","56vEaKq9VqFBlqtcXZAgn4")
sprinters <- get_playlist_audio_features("", "70wUKUusJoxhLNieGGRXpK")


total <-
  bind_rows(
    long_distance %>% mutate(category = "Long Distance swimmers"),
    sprinters %>% mutate(category = "Sprinters")
  )


total %>% ggplot(aes(y = energy, color = category)) +
  geom_boxplot()

total %>% ggplot(aes(y = danceability, color = category)) +
  geom_boxplot()


library(scales)
p <- ggplot(total, aes(category, energy)) + 
  geom_boxplot()
ggplotly(p, tooltip = c("category"))



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





#energy/dance  boxplot

long_distance <- get_playlist_audio_features("","56vEaKq9VqFBlqtcXZAgn4")
sprinters <- get_playlist_audio_features("", "70wUKUusJoxhLNieGGRXpK")


total <-
  bind_rows(
    long_distance %>% mutate(Category = "Long-distance playlist"),
    sprinters %>% mutate(Category = "Sprinters playlist")
  )

e <- ggplot(total, aes(Category, energy)) + 
  geom_boxplot(aes(fill=Category)) +
  labs(title="Boxplot Energy",x="", y = "Energy") +
  scale_fill_manual(values = c('#93B9D0', '#295590'), guide = "none")

#ggplotly(e, tooltip = c("category"))

d <- ggplot(total, aes(Category, danceability)) + 
  geom_boxplot(aes(fill=Category)) +
  labs(title="Boxplot Danceability",x="", y = "Danceability") +
  scale_fill_manual(values = c('#93B9D0', '#295590'), guide = "none")
#ggplotly(d, tooltip = c("category"))

grid.arrange(e, d, ncol=2)
plot1 <- ggplotly(e) %>%
  layout(xaxis = list(title = ''), yaxis = list(title = 'Energy'))
plot2 <- ggplotly(d) %>%
  layout(xaxis = list(title = ''), yaxis = list(title = 'Danceability'))

fig <- subplot(plot1, plot2, nrows=1, titleY=TRUE, titleX=TRUE, margin = 0.075) %>%
  layout(title = "Boxplot Energy and Danceability")

fig
