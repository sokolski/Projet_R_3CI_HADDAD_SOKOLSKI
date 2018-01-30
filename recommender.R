library(readr)
library(dplyr)
library(data.table)

library(stringr)
library(jsonlite)

library(tidytext)

library(ggplot2)

library(recommenderlab)

####################################

# we're reading only 5 000 lines in this example

infile <- "C:/Data_R/reviews_Books.json"
review_lines <- read_lines(infile, n_max = 5000, progress = FALSE)

# Each line is a JSON object- the fastest way to process is to combine into a
# single JSON string and use fromJSON and flatten
reviews_combined <- str_c("[", str_c(review_lines, collapse = ", "), "]")

reviews <- fromJSON(reviews_combined) %>%
  flatten() %>%
  tbl_df()

review_words <- reviews %>%
  select(reviewerID, asin, overall, reviewText) %>%
  unnest_tokens(word, reviewText) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

#################   AFINN method ################


####

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

review_words_counted_afinn <- review_words %>%
  inner_join(AFINN, by = "word") %>%
  count(asin, afinn_score, sort = TRUE) %>%
  ungroup()

word_summaries <- review_words_counted_afinn %>%
  group_by(word) %>%
  summarize(books = n_distinct(reviewerID),
            reviews = n(),
            uses = sum(n),
            average_stars = mean(overall)) %>%
  ungroup()

#most positive and negative
word_summaries_filtered <- word_summaries %>%
  filter(reviews >= 200, books >= 10)

words_afinn <- word_summaries_filtered %>%
  inner_join(AFINN, by = "word")
#plot
ggplot(words_afinn, aes(afinn_score, average_stars, group = afinn_score)) +
  geom_boxplot() +
  xlab("AFINN score of word") +
  ylab("Average stars of reviews with this word")

###### plot to see score words
word_summaries_filtered %>%
  inner_join(AFINN, by = "word") %>%
  ggplot(aes(reviews, average_stars, color = afinn_score)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(reviews$overall), color = "red", lty = 2) +
  scale_color_gradient2(low = "red", high = "blue", midpoint = 0, mid = "gray") +
  labs(x = "# of reviews",
       y = "Average Stars",
       color = "AFINN")

################## BING method #################
bing <- get_sentiments("bing")

review_bing_word_counts <- review_words %>%
  inner_join(bing, by = "word") %>%
  count(asin, sentiment, sort = TRUE) %>%
  ungroup()

#plot bing method
######
review_bing_word_counts %>%
  filter(n > 10) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(asin = reorder(asin, n)) %>%
  ggplot(aes(asin, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")


########### système de recommendation ###########

######## AFINN recommender #########
data_rec_afinn<-merge (review_words, review_words_counted_afinn, by="asin")

data_rec_afinn_1<- subset(data_rec_afinn, select=c(asin, reviewerID, n))
data_rec_afinn_2 <- as.matrix(data_rec_afinn_1)
data_rec_afinn_2<- sapply(data.frame(data_rec_afinn_2),as.numeric)

data_mat_afinn <- as(data_rec_afinn_2,"realRatingMatrix")
train_afinn <- data_mtx_afinn[1:30]
r_afinn <- Recommender(train_afinn, method = "UBCF")

p_afinn <- predict(r_afinn, data_mat_afinn[3], n=10, type="ratings")
as(p_afinn, "matrix")
p_afinn
###### BING recommender ############
data_recommender_bing<-merge (review_words, bing_word_counts, by= "asin", all.x=TRUE)

data_recommender_1<- subset(data_recommender_bing,select=c(asin, reviewerID ,n))
data_recommender_2 <- as.matrix(data_recommender_1)
data_recommender_2<- sapply(data.frame(data_recommender_2),as.numeric)

data_mat_bing<- as(data_recommender_2,"realRatingMatrix")
train_bing <- data_mat_bing[1:50]
r_bing <- Recommender(train_bing, method = "UBCF")

p_bing <- predict(r_bing, data_mat, type="ratings")
as(p_bing, "matrix")
pre_bing <- predict(r_bing, data_mat[101], n = 1)
pre_bing


