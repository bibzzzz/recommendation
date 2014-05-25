##### RECOMMENDER SYSTEM #####

# READ AND STRUCTURE DATA
movie_data <- read.csv(file="/Users/habibadam/Documents/datamining - numerati/Movie_Ratings.csv")
names(movie_data)[1] <- "Movie_Name"
user_list <- names(movie_data)[names(movie_data)!="Movie_Name"]

# COMPUTE MANHATTAN DISTANCE
distance_fn <- function(user1,user2){
  user1_data <- t(movie_data[,user1])
  user2_data <- t(movie_data[,user2])
  distance_vec <- abs(user1_data - user2_data)
  distance <- mean(!is.na(distance_vec))
  return(distance)
}

# COMPUTE USER DISTANCE FROM ALL OTHER USERS
NeighborDistance_fn <- function(username){
  other_user_list <- user_list[user_list!=username]
  nn_distance_vec <- NULL
  for (user in other_user_list){
    user_distance <- distance_fn(username,user)
    nn_distance_vec <- c(nn_distance_vec,user_distance)
  }
  nn_distance_df <- data.frame(user=other_user_list,distance=nn_distance_vec)
  return(nn_distance_df)
}

# ISOLATE MOVIES YET TO BE RATED AND THEIR SET OF RATINGS
RatingList_fn <- function(username){
  user_rating_list <- movie_data[,username]
  user_rated_list <- as.character(movie_data$Movie_Name[!is.na(user_rating_list)])
  user_non_rated_list <- as.character(movie_data$Movie_Name[!movie_data$Movie_Name%in%user_rated_list])
  non_rated_movie_data <- movie_data[movie_data$Movie_Name%in%user_non_rated_list,]
  non_rated_movie_list <- as.character(non_rated_movie_data$Movie_Name)
  other_user_list <- user_list[user_list!=username]
  relevant_movie_data <- non_rated_movie_data[,other_user_list]
  relevant_movie_data <- data.frame(other_user_list[colSums(!is.na(relevant_movie_data))>0],
                                    t(relevant_movie_data[colSums(!is.na(relevant_movie_data))>0]))
  names(relevant_movie_data) <- c("user",non_rated_movie_list)
  return(relevant_movie_data)
}

# COMBINE MOVIE RATING LIST AND USER DISTANCE
RatingDistanceCompile_fn <- function(username){
  user_rating_list <- RatingList_fn(username)
  user_distance_list <- NeighborDistance_fn(username)
  user_rd_compile <- merge(user_rating_list,user_distance_list,by="user")
  return(user_rd_compile)
}

# LOOP THROUGH UN-RATED MOVIES AND COMPILE RECOMMENDATION LIST
Recommend_fn <- function(username){
  user_compile_table <- RatingDistanceCompile_fn(username)
  film_list <- names(user_compile_table)[!names(user_compile_table)%in%c("user","distance")]
  film_recommendation_vec <- NULL
  for (film in film_list){
    film_table <- user_compile_table[,c(film,"distance")]
    film_ratings_table <- film_table[!is.na(film_table[,film]),]
    rating_weights <- (1/(film_ratings_table$distance/sum(film_ratings_table$distance)))/
      sum(1/(film_ratings_table$distance/sum(film_ratings_table$distance)))
    film_recommendation <- sum(film_ratings_table[,film]*rating_weights)
    film_recommendation_vec <- c(film_recommendation_vec,film_recommendation)
  }
  film_recommendation_df <- data.frame(film=film_list,recommendation_rating=film_recommendation_vec)
  film_recommendation_df <- film_recommendation_df[order(film_recommendation_df$recommendation_rating,decreasing=TRUE),]
  return(film_recommendation_df)
}

Recommend_fn('Josh')
