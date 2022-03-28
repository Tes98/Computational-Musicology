### __tempogram__ of two songs: Heart-Shaped Box from Nirvana and Remember the name from Fort Minor

#### Heart-Shaped Box
```{r tempogram1}
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
  labs(title = "Heart-Shaped Box",x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic() 

#tempogram of Remember the Name (feat. Styles of Beyond)
# long distance track with lowest tempo

low_tempo <- styles %>%
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE) %>%
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(title = "Remember the Name", x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic() 

#ggplotly(low_tempo)

ggarrange(highest_tempo, low_tempo)

```

***
  Tempo can vary locally within a piece. A tempogram is a feature matrix which indicates the prevalence of certain tempi at each moment in time. In this section, I have made two temopgrams. One of the song with the lowest tempo and one with the highest tempo. The song with the lowest tempo came from the long-distance playlist, this is the song Remember the Name (feat. Styles of Beyond). The song with the highest tempo, Heart-Shaped Box from nirvana, came from the sprinters playlist. 

Still find it difficult to interpret these plots, so a proper explanation follows. 