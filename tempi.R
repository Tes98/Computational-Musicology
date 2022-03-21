library(ggplot2)
library(tidyverse)
library(tidymodels)
library(plotly) # N.B. Requires the Cairo package
library(protoclust)
library(heatmaply)
library(spotifyr)
library(compmus)
library(plyr)
library(cowplot)

# histogram of tempi 

long_distance <- get_playlist_audio_features("","56vEaKq9VqFBlqtcXZAgn4")
sprinters <- get_playlist_audio_features("", "70wUKUusJoxhLNieGGRXpK")


total <-
  bind_rows(
    long_distance %>% mutate(Category = "Long-distance playlist"),
    sprinters %>% mutate(Category = "Sprinters playlist")
  )


mu <- ddply(total, "Category", summarise, grp.mean=mean(tempo))
head(mu)

tempi_hist <- total %>%
  ggplot(aes(x=tempo, color=Category, fill=Category)) +
  geom_histogram(position="dodge", alpha=0.5, binwidth = 15) + 
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Category),
             linetype="dashed") +
  labs(title="Tempo histogram",x="Tempo(BPM)", y = "Count") +
  theme_classic()

ggplotly((tempi_hist))


# tempogram of Heart-shaped box
#sprinter track with the highest tempo 
nirvana <- get_tidy_audio_analysis("11LmqTE2naFULdEP94AUBa")
styles <- get_tidy_audio_analysis("6ndmKwWqMozN2tcZqzCX4K")

b <- 
  bind_rows(
  nirvana %>% mutate(Category = "Heart-Shaped Box"),
  styles %>% mutate(Category = "Remember the Name (feat. Styles of Beyond)")
)

highest_tempo <- nirvana %>%
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE) %>%
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(title="Heart-Shaped Box", x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic() 


#tempogram of Remember the Name (feat. Styles of Beyond)
# long distance track with lowest tempo


low_tempo <- styles %>%
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE) %>%
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(title="Remember the Name", x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic() 
  
plot_grid(highest_tempo, low_tempo, labels = "AUTO")
