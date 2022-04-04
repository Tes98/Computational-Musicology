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
library(dplyr)

# histogram of tempi 

long_distance <- get_playlist_audio_features("","56vEaKq9VqFBlqtcXZAgn4")
sprinters <- get_playlist_audio_features("", "70wUKUusJoxhLNieGGRXpK")


total <-
  bind_rows(
    long_distance %>% mutate(Playlist = "Long-distance playlist"),
    sprinters %>% mutate(Playlist = "Sprinters playlist")
  )


mu <- ddply(total, "Playlist", summarise, grp.mean=mean(tempo))

tempi_hist <- total %>%
  ggplot(aes(x=tempo, y=..density.., fill=Playlist)) +
  geom_histogram(alpha=0.6) + 
  geom_density(alpha=0.2, fill='white') +
  geom_vline(data=mu, aes(xintercept=grp.mean), colour = c("#93B9D0", "#295590"),
             linetype="dashed") +
  labs(title="Tempo histogram",x="Tempo(BPM)", y = "Count") +
  scale_fill_manual(values = c('#93B9D0', '#295590'), guide = "none") +
  theme_classic()

ggplotly((tempi_hist))


# tempogram of Heart-shaped box
#sprinter track with the highest tempo 
panther <- get_tidy_audio_analysis("20A08NCQknognYF4i9EyGu")
lease_on_life <- get_tidy_audio_analysis("4ufkuONjQMNR2fyXu1bO9w")


highest_tempo <- panther %>%
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) %>%
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(title="Eyes of a Panther", x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic() 


#tempogram of Remember the Name (feat. Styles of Beyond)
# long distance track with lowest tempo


low_tempo <- lease_on_life %>%
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) %>%
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(title="Lease On Life", x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic() 
  
plot_grid(highest_tempo, low_tempo, labels = "")

ggplot2.histogram(data=weight, xName='weight',
                  groupName='sex', legendPosition="top",
                  alpha=0.5, addDensity=TRUE,
                  addMeanLine=TRUE, meanLineColor="white", meanLineSize=1.5)


