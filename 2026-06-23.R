if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, tidytext, patchwork, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-06-23')

encyclicals <- tuesdata$encyclicals

rerum_novarum <- encyclicals %>%
  filter(encyclical == "Rerum Novarum")

magnifica_humanitas <- encyclicals %>%
  filter(encyclical == "Magnifica Humanitas")

#Compares the most commonly used words between the two encyclicals
rerum_novarum_tokens <- rerum_novarum %>%
  unnest_tokens(word, text, drop = TRUE) %>% 
  select(-(encyclical:sentence_count)) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 15) %>%
  mutate(word = fct_reorder(word, n),
         count = n)

rerum_novarum_tokens_plot <- rerum_novarum_tokens %>%
  ggplot(aes(x = word, y = count)) +
  geom_col() + 
  coord_flip() +
  labs(x = "Word",
       y = "Count",
       title = "Rerum Novarum")

magnifica_humanitas_tokens <- magnifica_humanitas %>%
  unnest_tokens(word, text, drop = TRUE) %>% 
  select(-(encyclical:sentence_count)) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 15) %>%
  mutate(word = fct_reorder(word, n),
         count = n)

magnifica_humanitas_tokens_plot <- magnifica_humanitas_tokens %>%
  ggplot(aes(x = word, y = count)) +
  geom_col() + 
  coord_flip() +
  labs(x = "Word",
       y = "Count",
       title = "Magnifica Humanitas")

rerum_novarum_tokens_plot / magnifica_humanitas_tokens_plot + plot_annotation("Most Commonly Used Words")

#Comparing sources of inspiration for each encylical
scripture_references <- tuesdata$scripture_references

rn_scripture_references <- scripture_references %>%
  filter(encyclical == "Rerum Novarum")
  
rn_scripture_references_count <- rn_scripture_references %>%
  count(book, sort = TRUE)

rn_scripture_references <- rn_scripture_references %>%
  select(book:testament) %>%
  distinct()

rn_scripture_references_count <- left_join(rn_scripture_references_count, rn_scripture_references, by = "book")

rn_scripture_references_count_plot <- rn_scripture_references_count %>%
  ggplot(aes(x = reorder(book, n), y = n, fill = testament)) +
  geom_col() + 
  coord_flip() +
  labs(x = "Book",
       y = "Count",
       title = "Rerum Novarum") +
  theme(legend.title = element_blank())

mh_scripture_references <- scripture_references %>%
  filter(encyclical == "Magnifica Humanitas")

mh_scripture_references_count <- mh_scripture_references %>%
  count(book, sort = TRUE)

mh_scripture_references <- mh_scripture_references %>%
  select(book:testament) %>%
  distinct()

mh_scripture_references_count <- left_join(mh_scripture_references_count, mh_scripture_references, by = "book")

mh_scripture_references_count_plot <- mh_scripture_references_count %>%
  ggplot(aes(x = reorder(book, n), y = n, fill = testament)) +
  geom_col() + 
  coord_flip() +
  labs(x = "Book",
       y = "Count",
       title = "Magnifica Humanitas") +
  theme(legend.title = element_blank())

rn_scripture_references_count_plot / mh_scripture_references_count_plot + plot_annotation("Most Common Books Cited") + plot_layout(guides = "collect")

#ChatGPT was used for the next two sections as I have little experience with machine learning and especially in R

#Machine learning prediction 
pacman::p_load(tidymodels, textrecipes, stopwords, glmnet, yardstick)

filtered_encyclicals <- encyclicals %>%
  select(pope, text) #Keeps only these two columns for ease of analysis

#Splits into training and testing sets
set.seed(123)
text_split <- initial_split(filtered_encyclicals, prop = 0.7, strata = pope)
train_data <- training(text_split)
test_data <- testing(text_split)

#Creates the textrecipe object
text_rec <- recipe(pope ~ text, data = train_data) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_tfidf(text)

#Creates the model
model <- multinom_reg(penalty = 0.01) %>%
  set_engine("glmnet")

#Creates the workflow
wf <- workflow() %>%
  add_recipe(text_rec) %>%
  add_model(model)

fit_model <- fit(wf, data = train_data)

#Evaluate on test data
pred <- predict(fit_model, test_data) %>%
  bind_cols(test_data) %>%
  mutate(prediction = as.factor(.pred_class),
         pope = as.factor(pope))

#Creates and plots a confusion matrix
pred_confusion_matrix <- conf_mat(pred, pope, prediction)

pred_confusion_matrix_plot <- autoplot(pred_confusion_matrix, type = "heatmap") +
  scale_fill_gradient(low="azure",high = "#2E86C1") +
  labs(title = "Confusion Matrix",
       x = "Actual")

pred_confusion_matrix_plot

#Finds the most similar passages
pacman::p_load(proxy, gt)

#Creates the textrecipe object, including the tfidf data
passage_rec <- recipe(~ text, data = encyclicals) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_tfidf(text) %>%
  prep() %>%
  bake(new_data = NULL)

#Joins the corresponding encyclical and text
passage_rec_encyclicals <- bind_cols(
  encyclicals %>% select(encyclical, text),
  passage_rec
)

#Extracts only the tfidf data
passage_rec_matrix <- passage_rec_encyclicals %>%
  select(starts_with("tfidf_")) %>%
  as.matrix()

#Computes cosine similarity
sim <- as.matrix(simil(
  passage_rec_matrix,
  method = "cosine"
))

rn <- which(passage_rec_encyclicals$encyclical == "Rerum Novarum")
mh <- which(passage_rec_encyclicals$encyclical == "Magnifica Humanitas")

cross_sim <- sim[rn, mh]

#Compares each passage in the two encyclicals
results <- expand.grid(
  rn = seq_along(rn),
  mh = seq_along(mh)
) %>%
  mutate(
    similarity = cross_sim[cbind(rn, mh)],
    rn_row = rn[rn],
    mh_row = mh[mh],
    rn_text = rerum_novarum$text[rn_row],
    mh_text = magnifica_humanitas$text[mh_row]
  ) %>%
  arrange(desc(similarity))

#Creates a heatmap showing the similarity between each passage
results_plot <- results %>%
  ggplot(aes(x = mh, y = rn, fill = similarity)) +
  geom_tile() +
  labs(title = "Similarity between Rerum Novarum and Magnifica Humanitas",
       x = "Magnifica Humanitas",
       y = "Rerum Novarum",
       fill = "Similarity") +
  scale_fill_gradient(low = "white", high = "black")

results_plot

#Creates a table showing the most similar passages and how similar they are
results_table <- results %>%
  slice_head(n = 10) %>%
  select(similarity, rn_text:mh_text) %>%
  gt()

results_table

