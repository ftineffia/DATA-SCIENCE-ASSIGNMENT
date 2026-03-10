# Install required packages
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)

data <- read_excel("cleaned_movielens_data.xlsx")

rating_data <- data %>% select(user_id, item_id, rating)
user_item_matrix <- rating_data %>% pivot_wider(names_from = item_id, values_from = rating, values_fill = 0)
user_item_matrix <- user_item_matrix %>% column_to_rownames(var = "user_id")

# calculate user means (excluding 0s)
user_means <- rating_data %>%
  group_by(user_id) %>%
  filter(rating > 0) %>%  # Exclude implicit 0s
  summarise(mean_rating = mean(rating))

# Join means back and normalize
normalized_data <- rating_data %>%
  left_join(user_means, by = "user_id") %>%
  mutate(normalized_rating = ifelse(rating > 0, rating - mean_rating, 0))

# Recreate the normalized user-item matrix
normalized_user_item_matrix <- normalized_data %>%
  select(user_id, item_id, normalized_rating) %>%
  pivot_wider(names_from = item_id, values_from = normalized_rating, values_fill = 0) %>%
  column_to_rownames(var = "user_id")

head(user_item_matrix)
head (normalized_user_item_matrix)

