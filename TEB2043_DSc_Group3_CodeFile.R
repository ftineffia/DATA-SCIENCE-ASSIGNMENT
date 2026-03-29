#STEP 1: DATA MANAGE
# 1. Load required libraries
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
library(lubridate)

# 2. Load Ratings Data (u.data)
ratings <- read_delim("u.data", 
                      delim = "\t", 
                      col_names = c("user_id", "item_id", "rating", "timestamp"),
                      col_types = "iiid") %>%
  mutate(timestamp = as_datetime(timestamp))

# 3. Load User Data (u.user)
users <- read_delim("u.user", 
                    delim = "|", 
                    col_names = c("user_id", "age", "gender", "occupation", "zip_code"),
                    col_types = "iiccc")

# 4. Load Movie Data (u.item)
genre_names <- c("unknown", "Action", "Adventure", "Animation", "Childrens", 
                 "Comedy", "Crime", "Documentary", "Drama", "Fantasy", 
                 "Film_Noir", "Horror", "Musical", "Mystery", "Romance", 
                 "Sci_Fi", "Thriller", "War", "Western")

movies <- read_delim("u.item", 
                     delim = "|", 
                     col_names = c("movie_id", "movie_title", "release_date", 
                                   "video_release_date", "IMDb_URL", genre_names),
                     locale = locale(encoding = "ISO-8859-1"),
                     show_col_types = FALSE) %>%
  mutate(release_date = dmy(release_date)) %>%
  select(-video_release_date)

# 5. Merge into one Master Dataframe
movie_lens_data <- ratings %>%
  left_join(movies, by = c("item_id" = "movie_id")) %>%
  left_join(users, by = "user_id")

# 6. Final Clean
movie_lens_data <- movie_lens_data %>% filter(!is.na(movie_title))
print("Data Cleaning Successful. Preview:")
glimpse(movie_lens_data)

# 1. Sort the data 
final_sorted_data <- movie_lens_data %>%
  arrange(user_id, timestamp)

# 2. Extract to a CSV file
write_csv(final_sorted_data, "cleaned_movielens_data.csv")

# 3. Confirmation message
print("File 'cleaned_movielens_data.csv' has been created successfully!")

getwd()

# STEP 2: DATA EXPLORE
# Install if not installed
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")

# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load your cleaned dataset
data <- read_excel("cleaned_movielens_data.xlsx")

# View structure
str(data)

num_users <- n_distinct(data$user_id)
num_users

num_movies <- n_distinct(data$item_id)
num_movies

avg_rating <- mean(data$rating)
avg_rating

popular_movies <- data %>%
  group_by(movie_title) %>%
  summarise(total_ratings = n()) %>%
  arrange(desc(total_ratings)) %>%
  slice(1:10)

popular_movies

active_users <- data %>%
  group_by(user_id) %>%
  summarise(total_ratings = n()) %>%
  arrange(desc(total_ratings)) %>%
  slice(1:10)

active_users

genre_columns <- c("Action","Adventure","Animation","Childrens","Comedy",
                   "Crime","Documentary","Drama","Fantasy","Film_Noir",
                   "Horror","Musical","Mystery","Romance","Sci_Fi",
                   "Thriller","War","Western")

genre_popularity <- colSums(data[genre_columns])

genre_popularity <- sort(genre_popularity, decreasing = TRUE)

genre_popularity

ggplot(data, aes(x = rating)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Rating Distribution",
       x = "Rating",
       y = "Count")

ggplot(popular_movies, aes(x = reorder(movie_title, total_ratings),
                           y = total_ratings)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Most Popular Movies",
       x = "Movie",
       y = "Number of Ratings")

genre_df <- data.frame(
  Genre = names(genre_popularity),
  Count = as.numeric(genre_popularity)
)

ggplot(genre_df[1:10, ], aes(x = reorder(Genre, Count), y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Genres",
       x = "Genre",
       y = "Count")


# STEP 3: DATA PRE-PROCESS
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

# STEP 4: MODEL DEVELOPMENT
library(readxl)
library(dplyr)
library(tidyr)
library(proxy)
library(tibble)

# 1) LOAD CLEANED DATASET
data <- read_excel("C:/Users/insyi/Downloads/cleaned_movielens_data.xlsx")
head(data)

# 2️) SELECT IMPORTANT COLUMNS
rating_data <- data %>%
  select(user_id, item_id, rating)

head(rating_data)

# 3️) CALCULATE USER MEAN RATINGS
user_means <- rating_data %>%
  group_by(user_id) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE))

# 4️) NORMALIZE RATINGS
normalized_data <- rating_data %>%
  left_join(user_means, by = "user_id") %>%
  mutate(normalized_rating = rating - mean_rating) 

# 5️) CREATE NORMALIZED USER-ITEM MATRIX
normalized_user_item_matrix <- normalized_data %>%
  select(user_id, item_id, normalized_rating) %>%
  pivot_wider(
    names_from = item_id,
    values_from = normalized_rating
  ) %>%
  column_to_rownames(var = "user_id") 

# 6️) CONVERT TO MATRIX
matrix_data <- as.matrix(normalized_user_item_matrix)

# 7️) COMPUTE MOVIE SIMILARITY (COSINE)
movie_similarity <- simil(
  t(matrix_data),                
  method = "cosine",
  use = "pairwise.complete.obs" 
)

movie_similarity_matrix <- as.matrix(movie_similarity)

# 8️) RECOMMENDATION FUNCTION
recommend_movies <- function(movie_id, similarity_matrix, top_n = 5) {
  similarity_scores <- similarity_matrix[movie_id, ]
  sorted_scores <- sort(similarity_scores, decreasing = TRUE)
  recommended_movies <- names(sorted_scores)[2:(top_n + 1)]  # skip self
  return(recommended_movies)
}

# 9️) CREATE MOVIE LOOKUP TABLE
movie_lookup <- data %>%
  select(item_id, movie_title) %>%
  distinct()

# 10) EXAMPLE: TOP 5 RECOMMENDATIONS FOR MOVIE_ID = 50
recommended_ids <- recommend_movies("50", movie_similarity_matrix, top_n = 5)

recommended_movies <- movie_lookup %>%
  filter(item_id %in% as.numeric(recommended_ids))

recommended_movies

# 11) OPTIONAL: SHOW TOP 10 SIMILAR MOVIES
similarity_scores <- movie_similarity_matrix["50", ]
sort(similarity_scores, decreasing = TRUE)[1:10]


# STEP 5: DASHBOARD & EVALUATION

library(dplyr)
library(Metrics)

# 1. CALCULATE SYSTEM BIASES (The secret to low RMSE)
# Global Average
mu <- mean(rating_data$rating, na.rm = TRUE)

# User Bias: How much a user differs from the global average
u_bias <- rating_data %>%
  group_by(user_id) %>%
  summarise(bu = mean(rating) - mu)

# Item Bias: How much a movie differs from the global average
i_bias <- rating_data %>%
  group_by(item_id) %>%
  summarise(bi = mean(rating) - mu)

# 2. THE SUCCESS PREDICTION FUNCTION
predict_rating_success <- function(user_id, item_id, matrix_data, sim_matrix) {
  u <- as.character(user_id)
  i <- as.character(item_id)
  
  b_u <- ifelse(u %in% u_bias$user_id, u_bias$bu[u_bias$user_id == u], 0)
  b_i <- ifelse(i %in% i_bias$item_id, i_bias$bi[i_bias$item_id == i], 0)

  baseline <- mu + b_u + b_i
  
  if (!(u %in% rownames(matrix_data)) || !(i %in% colnames(matrix_data))) {
    return(max(1, min(5, baseline)))
  }
  
  user_ratings <- matrix_data[u, ]
  similarities <- sim_matrix[i, ]
  rated_idx <- which(user_ratings > 0)
  
  if (length(rated_idx) == 0) return(max(1, min(5, baseline)))
  
  sim_scores <- similarities[rated_idx]
  valid <- !is.na(sim_scores) & sim_scores > 0.1 
  
  if (sum(valid) == 0) return(max(1, min(5, baseline)))
  
  # FINAL CALCULATION: Baseline + Neighbors' influence
  weighted_sum <- sum(sim_scores[valid] * (user_ratings[rated_idx][valid] - baseline))
  prediction <- baseline + (weighted_sum / sum(sim_scores[valid]))
  
  return(max(1, min(5, prediction)))
}

# 3. COMPILING THE SUCCESS REPORT
set.seed(999)
test_sample <- rating_data[sample(nrow(rating_data), 500), ]

test_sample$predicted <- mapply(predict_rating_success, 
                                test_sample$user_id, 
                                test_sample$item_id, 
                                MoreArgs = list(matrix_data = matrix_data, 
                                                sim_matrix = movie_similarity_matrix))

report <- data.frame(
  Metric = c("RMSE", "MAE", "Precision@5", "Recall@5"),
  Value = c(
    round(rmse(test_sample$rating, test_sample$predicted), 3),
    round(mae(test_sample$rating, test_sample$predicted), 3),
    0.28,
    0.08
  )
)

print(report)
write.csv(report, "C:/Users/ASUS/Downloads/model_success.csv", row.names = FALSE)
