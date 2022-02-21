library(tidyverse)
library(spotifyr)
library(ggplot2)

pop <- get_playlist_audio_features("","4vQc2Q7ERByqmRpFnD4GfI")
classical <- get_playlist_audio_features("", "6dIXmgBDBwhp0J6CKKD7Hn")
gaga <- get_track_audio_analysis(c("1HHeOs6zRdF8Ck58easiAY"))
monti <- get_track_audio_analysis(c("6lo128WdLt7xP5Ejvsq1ym"))
head(gaga)
summary(total)
summary(gaga)
summary(monti)
total <-
  bind_rows(
    pop %>% mutate(category = "Pop"),
    classical %>% mutate(category = "Classical")
  )

two_songs <- 
  bind_rows(
    gaga %>% mutate(category = "Lady Gaga"),
    monti %>% mutate(category = "Vittorio Monti")
  )
head(two_songs)
#total %>% ggplot(aes(x = valence, color = category, fill = category)) +
 # geom_histogram(position = 'dodge', binwidth = 0.1) 


total %>% ggplot(aes(x = category, y = key)) +
  geom_boxplot() 

total %>% ggplot(aes(x = category, y = timbre)) +
  geom_boxplot() 

gaga %>% ggplot(aes(x = track:length, y = timbre, color = category)) +
  geom_point() + geom_smooth() 
