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
