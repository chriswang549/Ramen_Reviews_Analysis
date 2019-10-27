# Learn Pipe Operators
# Library() when used just before code
# Improve Organization
# Use _ for variable names to make things more readable
# Word Tree Correlated with Ratings

library(tidyverse)

#Loading provided csv file for ramen ratings
#linear regression to predict star ratings?
ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")
view(ramen_ratings)



# get predictors
# bar graph of 4 predictors recorded


ramen_ratings_predictors %>%
  gather(category, value, -review_number, -stars) %>%
  count(category, value) %>%
  group_by(category) %>%
  top_n(20, n) %>%
  ungroup() %>%
  mutate(value = reorder_within(value, n, category)) %>%
  ggplot(aes(value, n)) +
  geom_col() +
  facet_wrap(~ category, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Categorical Predictors That May Be Relevant ",
       x = "Predictor",
       y = "Count")

ramen_ratings %>% count(variety,sort =T)
#variety may be a subset of brands? low# of ns

library(broom)
# linear regression model star ratings correlated with brand ,country , etc

lm(stars ~ brand + country + style, ramen_ratings_predictors) %>%
  tidy(conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(estimate)) %>%
  extract(term, c("category", "term"), "^([a-z]+)([A-Z].*)") %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(estimate, term, color = category)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(lty = 2, xintercept = 0) +
  facet_wrap(~ category, ncol = 1, scales = "free_y") +
  theme(legend.position = "none") +
  labs(x = "Effect on Ratings",
       y = "",
       title = "Variables That Predict Ratings",
       subtitle = "Some less common variables are lumped together")

#Modifications to the plot above
ramen_ratings_predictors <- ramen_ratings %>%
  mutate(style = fct_lump(style, 4),
         country = fct_lump(country, 12),
         brand = fct_lump(brand, 20)) %>%
  replace_na(list(style = "Other")) %>%
  mutate(brand = fct_relevel(brand, "Other"),
         country = fct_relevel(country, "Other"),
         style = fct_relevel(style, "Pack"))


library(tidytext)
# average rating of words with counts
ramen_ratings_predictors %>%
  filter(!is.na(stars)) %>%
  unnest_tokens(word, variety) %>%
  group_by(word) %>%
  summarize(avg_rating = mean(stars),
            n = n()) %>%
  arrange(desc(n))


### Web scraping for the word reviews


library(rvest)
library(janitor)
ramen_list <- read_html("https://www.theramenrater.com/resources-2/the-list/")


# We can get the data ourselves
ramen_reviews <- ramen_list %>%
  html_node("#myTable") %>%
  html_table() %>%
  tbl_df() %>%
  janitor::clean_names() %>%
  select(-t)



review_links <- read_html("https://www.theramenrater.com/resources-2/the-list/") %>% 
  html_nodes("#myTable a ")
reviews <- tibble(review_number = parse_number(html_text(review_links)),
                  link = html_attr(review_links, "href"))


page <- read_html("https://www.theramenrater.com/2019/05/23/3180-yum-yum-moo-deng/")
#Get review text from any url
get_review_text <- function(url) {
  message(url)
  
  read_html(url) %>%
    html_nodes(".entry-content > p") %>%
    html_text() %>%
    str_subset(".")
}
review_text <- reviews %>%
  head(250) %>%
  mutate(text = map(link, possibly(get_review_text, character(0), quiet = FALSE)))

## 2:23 am 10/27

library(tidytext)
review_paragraphs <- review_text %>% 
  filter(!map_lgl(text, is.null)) %>%
  unnest() %>%
  filter(str_detect(text, "Finished")) %>%
  mutate(text = str_remove(text, "Finished.*?\\. "))
#combine ratings with paragarphs
rewiew_paragraphs_combined <- review_paragraphs %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]")) %>%
  inner_join(ramen_ratings, by = "review_number")

#count of words that appear
review_words <- rewiew_paragraphs_combined %>%
  filter(!is.na(stars)) %>%
  group_by(word) %>%
  summarize(number = n(),
            reviews = n_distinct(review_number),
            avg_rating = mean(stars)) %>%
  arrange(desc(reviews))

review_words_filtered <- review_words %>%
  filter(reviews < 200, reviews >= 10)

library(widyr)
#word correlation
word_correlation <- rewiew_paragraphs_combined %>%
  semi_join(review_words_filtered, by = "word") %>%
  distinct(review_number, word) %>%
  pairwise_cor(word, review_number, sort = TRUE)

library(igraph)
library(ggraph)
set.seed(123)
#top 300 
filtered_cors <- word_correlation %>%
  head(300)

nodes <- review_words_filtered %>%
  filter(word %in% filtered_cors$item1 | word %in% filtered_cors$item2)
filtered_cors %>%
  graph_from_data_frame(vertices = nodes) %>%
  ggraph() +
  geom_edge_link() +
  geom_node_point(aes(size = reviews * 1.1)) +
  geom_node_point(aes(size = reviews, color = avg_rating)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_color_gradient2(low = "red", high = "blue", midpoint = 4) +
  theme_void() +
  labs(color = "Average rating",
       size = "# of reviews",
       title = "Network of words used together in ramen reviews",
       subtitle = "Based on 250 ramen reviews and their star ratings")
