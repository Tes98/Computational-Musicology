library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(spotifyr)
library(compmus)



get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit %>% 
    collect_predictions() %>% 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit %>% 
    conf_mat_resampled() %>% 
    group_by(Prediction) %>% mutate(precision = Freq / sum(Freq)) %>% 
    group_by(Truth) %>% mutate(recall = Freq / sum(Freq)) %>% 
    ungroup() %>% filter(Prediction == Truth) %>% 
    select(class = Prediction, precision, recall)
}  

# dendogram long-distance 

halloween <-
  get_playlist_audio_features(playlist_uris = "70wUKUusJoxhLNieGGRXpK") %>%
  add_audio_analysis() %>%
  mutate(
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean"
      )
  ) %>%
  mutate(pitches = map(pitches, compmus_normalise, "clr")) %>%
  mutate_at(vars(pitches, timbre), map, bind_rows) %>%
  unnest(cols = c(pitches, timbre))

halloween_juice <-
  recipe(
    track.name ~
      danceability +
      energy +
      #loudness +
      #speechiness +
      acousticness +
      instrumentalness + 
      #liveness + valence +
      tempo +
      # duration +
      # C + `C#|Db` + D + `D#|Eb` +
      # E + `F` + `F#|Gb` + G +
      #`G#|Ab` + A + `A#|Bb` + B +
      #c01 +
      c02 +
      c03 +
      #c04 + c05 + 
      c06 +
      c07 + 
      c08 + 
      #c09 + c10 + 
      c11, 
    #c12,
    data = halloween
  ) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  # step_range(all_predictors()) %>% 
  prep(halloween %>% mutate(track.name = str_trunc(track.name, 20))) %>%
  juice() %>%
  column_to_rownames("track.name")

halloween_dist <- dist(halloween_juice, method = "euclidean")

halloween_dist %>% 
  hclust(method = "complete") %>% # Try single, average, and complete.
  dendro_data() %>%
  ggdendrogram()


heatmaply(
  halloween_juice,
  hclustfun = hclust,
  hclust_method = "average",  # Change for single, average, or complete linkage.
  dist_method = "euclidean"
)
#------------------------------------------------------------------------------
# KNN

sprint <- get_playlist_audio_features("spotify", "70wUKUusJoxhLNieGGRXpK")
ld <- get_playlist_audio_features("spotify", "56vEaKq9VqFBlqtcXZAgn4")
wedstrijd <-
  bind_rows(
    sprint %>% mutate(playlist = "Sprinters playlist"), 
    ld %>% mutate(playlist = "Long-distance playlist")
  ) 

wedstrijd_features <-
  wedstrijd %>%  # For your portfolio, change this to the name of your corpus.
  add_audio_analysis() %>% 
  mutate(
    playlist = factor(playlist),
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(
        segments,
        compmus_summarise, pitches,
        method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean",
      )
  ) %>%
  mutate(pitches = map(pitches, compmus_normalise, "clr")) %>%
  mutate_at(vars(pitches, timbre), map, bind_rows) %>%
  unnest(cols = c(pitches, timbre))


wedstrijd_recipe <-
  recipe(
    playlist ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo +
      duration +
      C + `C#|Db` + D + `D#|Eb` +
      E + `F` + `F#|Gb` + G +
      `G#|Ab` + A + `A#|Bb` + B +
      c01 +
      c02 +
      c03 +
      c04 + c05 + 
      c06 +
      c07 + 
      c08 + 
      c09 + c10 + 
      c11 + 
      c12,
    data = wedstrijd_features,          # Use the same name as the previous block.
  ) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].


wedstrijd_cv <- wedstrijd_features %>% vfold_cv(5)

knn_model <-
  nearest_neighbor(neighbors = 1) %>%
  set_mode("classification") %>% 
  set_engine("kknn")
wedstrijd_knn <- 
  workflow() %>% 
  add_recipe(wedstrijd_recipe) %>% 
  add_model(knn_model) %>% 
  fit_resamples(
    wedstrijd_cv, 
    control = control_resamples(save_pred = TRUE)
  )




wedstrijd_knn %>% get_conf_mat() %>% autoplot(type = "heatmap")

wedstrijd_knn %>% get_pr()


#------------------------------------------------------------------------------
#random forest 

forest_model <-
  rand_forest() %>%
  set_mode("classification") %>% 
  set_engine("ranger", importance = "impurity")
wedstrijd_forest <- 
  workflow() %>% 
  add_recipe(wedstrijd_recipe) %>% 
  add_model(forest_model) %>% 
  fit_resamples(
    wedstrijd_cv, 
    control = control_resamples(save_pred = TRUE)
  )

wedstrijd_forest %>% get_conf_mat() %>% autoplot(type = "heatmap")
wedstrijd_forest %>% get_pr()


workflow() %>% 
  add_recipe(wedstrijd_recipe) %>% 
  add_model(forest_model) %>% 
  fit(wedstrijd_features) %>% 
  pluck("fit", "fit", "fit") %>%
  ranger::importance() %>% 
  enframe() %>% 
  mutate(name = fct_reorder(name, value)) %>% 
  ggplot(aes(name, value)) + 
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = "Importance")





wedstrijd_features %>%
  ggplot(aes(x = danceability, y = c02, colour = playlist, size = energy)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = c('#93B9D0', '#295590')) +
  labs(
    x = "Danceability",
    y = "Timbre Component 6",
    size = "Energy",
    colour = "Playlist"
  ) 

ggplotly(plot2)
