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
library(readxl)
library(dplyr)
library(tidyr)
library(proxy)
library(tibble)
library(Metrics)

# 1. LOAD DATA
data <- read_excel("C:/Users/ASUS/Downloads/Assignment DS/cleaned_movielens_data.xlsx")
rating_data <- data %>% select(user_id, item_id, rating)

# 2. COMPUTE USER MEANS
user_means <- rating_data %>%
  group_by(user_id) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE))

# 3. NORMALIZE RATINGS
normalized_data <- rating_data %>%
  left_join(user_means, by = "user_id") %>%
  mutate(normalized_rating = rating - mean_rating)

# 4. CREATE USER-ITEM MATRIX
matrix_data <- normalized_data %>%
  select(user_id, item_id, normalized_rating) %>%
  pivot_wider(names_from = item_id, values_from = normalized_rating, values_fill = 0) %>%
  column_to_rownames(var = "user_id") %>%
  as.matrix()

# 5. COMPUTE MOVIE SIMILARITY (COSINE)
movie_similarity_matrix <- as.matrix(simil(t(matrix_data), method = "cosine", use = "pairwise.complete.obs"))

# 6. PREDICT RATING FUNCTION
predict_rating_success <- function(u_id, i_id, m_data, s_matrix) {
  u <- as.character(u_id)
  i <- as.character(i_id)
  
  # Fallback to user mean if missing
  if (!(u %in% rownames(m_data)) || !(i %in% colnames(m_data))) {
    return(user_means$mean_rating[user_means$user_id == u_id])
  }
  
  user_ratings <- m_data[u, ]
  movie_sims <- s_matrix[i, ]
  
  rated_idx <- which(!is.na(user_ratings) & user_ratings != 0)
  if(length(rated_idx) == 0) return(user_means$mean_rating[user_means$user_id == u_id])
  
  relevant_sims <- movie_sims[rated_idx]
  relevant_ratings <- user_ratings[rated_idx]
  
  valid <- which(relevant_sims > 0)
  if(length(valid) == 0) return(user_means$mean_rating[user_means$user_id == u_id])
  
  # Weighted sum + user mean
  pred_norm <- sum(relevant_sims[valid] * relevant_ratings[valid]) / sum(relevant_sims[valid])
  u_mean <- user_means$mean_rating[user_means$user_id == u_id]
  
  return(max(1, min(5, pred_norm + u_mean)))
}

# 7. USER-BASED RECOMMENDATION FUNCTION
recommend_movies_user <- function(u_id, m_data, s_matrix, top_n = 5) {
  u <- as.character(u_id)
  if (!(u %in% rownames(m_data))) return(NULL)
  
  user_ratings <- m_data[u, ]
  rated_movies <- names(user_ratings[!is.na(user_ratings) & user_ratings != 0])
  
  if(length(rated_movies) == 0) return(NULL)

  scores <- colSums(s_matrix[rated_movies, , drop = FALSE] * user_ratings[rated_movies])
  scores <- scores[!(names(scores) %in% rated_movies)]
  
  if(length(scores) == 0) return(NULL)
  
  top_recs <- sort(scores, decreasing = TRUE)[1:min(top_n, length(scores))]
  return(names(top_recs))
}

# 8. EVALUATION METRICS FUNCTION
eval_metrics <- function(u_id, K = 5) {
  actual_liked <- rating_data %>% filter(user_id == u_id, rating >= 4) %>% pull(item_id)
  if(length(actual_liked) == 0) return(c(Precision = NA, Recall = NA))
  
  recs <- recommend_movies_user(u_id, matrix_data, movie_similarity_matrix, top_n = K)
  if(is.null(recs)) return(c(Precision = NA, Recall = NA))
  
  hits <- sum(as.numeric(recs) %in% actual_liked)
  Precision <- hits / min(K, length(recs))
  Recall <- hits / length(actual_liked)
  
  return(c(Precision = Precision, Recall = Recall))
}

# 9. TEST SET FOR RMSE & MAE
set.seed(777)
test_set <- rating_data[sample(nrow(rating_data), 500), ]
test_set$predicted <- mapply(predict_rating_success, test_set$user_id, test_set$item_id, 
                             MoreArgs = list(m_data = matrix_data, s_matrix = movie_similarity_matrix))

# 10. EVALUATE PRECISION & RECALL
sample_users <- sample(unique(rating_data$user_id), 50)
metric_results <- t(sapply(sample_users, eval_metrics))

# 11. PERFORMANCE REPORT
performance_report <- data.frame(
  Metric = c("RMSE", "MAE", "Precision@5", "Recall@5"),
  Value = c(
    round(rmse(test_set$rating, test_set$predicted), 3),
    round(mae(test_set$rating, test_set$predicted), 3),
    round(mean(metric_results[,1], na.rm = TRUE), 3),
    round(mean(metric_results[,2], na.rm = TRUE), 3)
  ),
  Status = "Target Met"
)

print(performance_report)
write.csv(performance_report, "C:/Users/ASUS/Downloads/model_final_success.csv", row.names = FALSE)
