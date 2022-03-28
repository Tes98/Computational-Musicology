library(readxl)
library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(spotifyr)
library(compmus)

df_long = read_excel('genre_count_long.xlsx')
df_sprint = read_excel('genre_count_sprint.xlsx')


both_genres <-
  bind_rows(
    df_long %>% mutate(Category = "Long-distance playlist"),
    df_sprint %>% mutate(Category = "Sprinters playlist")
  )

genre_histogram <- both_genres %>%
  ggplot(aes(x=Count, y = Genre)) +
  geom_col(fill="#93B9D0", alpha = 0.8) +
  geom_text(aes(label = Percentage)) +
  xlim(0,35) +
  theme_classic() +
  facet_wrap(~Category)
  

ggplotly(genre_histogram)


