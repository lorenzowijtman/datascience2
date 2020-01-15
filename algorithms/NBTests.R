# connect to spark
sc <<- spark_connect(master = "local", version="2.0.0")

trainNB <- function(posScore, negScore, limit) {
  limit <- (limit/2)
  posQ <- paste0('{"sentiment": 1, "Reviewer_Score": {"$lt" : ',posScore,'}}')
  negQ <- paste0('{"sentiment": 0, "Reviewer_Score": {"$lt" : ',negScore,'}}')
  pos <- mcon$find(posQ, fields = '{"Positive_Review": true, "Reviewer_Score": true, "sentiment": true}', limit = limit) 
  pos$`_id` <- NULL
  colnames(pos) <- c('review', 'score', 'sentiment')
  neg <- mcon$find(negQ, fields = '{"Negative_Review": true, "Reviewer_Score": true, "sentiment": true}', limit = limit)
  neg$`_id` <- NULL
  colnames(neg) <- c('review', 'score', 'sentiment')
  print(nrow(pos))
  print(nrow(neg))
  df <- rbind(pos, neg)
  copy_to(sc, df, name = "reviews_tbl", overwrite = T)
  rm(pos)
  rm(neg)
  rm(df)
  
  partitions <<- reviews_tbl %>%
    sdf_random_split(training = 0.7, test = 0.3, seed = 1011)
  
  
  pipeline <<- ml_pipeline(sc) %>%
    ft_tokenizer(input_col = "review", output_col = "raw_tokens") %>%
    ft_stop_words_remover(input_col = "raw_tokens", output_col = "tokens") %>%
    ft_count_vectorizer("tokens", "vectokens") %>%
    ft_r_formula(sentiment ~ vectokens) %>%
    ml_naive_bayes()
  
  fitted_pipeline <<- ml_fit(
    pipeline,
    partitions$training
  )

}

testNBWithData <- function() {
  
  predictions <- ml_transform(
    fitted_pipeline,
    partitions$test
  )
  
  table <- predictions %>%
    group_by(sentiment, prediction) %>%
    tally()
  
  return(predictions)
}

testNBWithOwnReview <- function(review) {
  test <- as.data.frame(c(review))
  
  colnames(test) <- c('review')
  
  my_tbl <- sdf_copy_to(sc, test, "test", overwrite = T)
  
  prediction <- ml_predict(fitted_pipeline, my_tbl)
  prediction <- as.data.frame(prediction)
  
  sentiment <- "Positive"
  
  if(prediction$prediction == 0) {
    sentiment <- "Negative"
  }
  
  return(sentiment)
}