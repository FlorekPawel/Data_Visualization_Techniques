library(stringr)
library(ggplot2)
library(dplyr)
library(ggwordcloud)

df <- read.csv("C:/PW/TWD/Projekt 2/data_csv/words_data.csv")

punctuation_regex <- "[[:punct:]]"
emoji_regex <- "[\\p{So}]"

words_df <- df[!str_detect(df$words, emoji_regex), ]

words_df <- words_df %>% 
  select(words) %>% 
  group_by(words) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


words_df <- words_df[nchar(words_df$words) >= 2, , drop = TRUE]
words_df <- words_df[!grepl(punctuation_regex, words_df$words), , drop = TRUE]
words_df <- words_df %>% 
  filter(words!="")

rownames(words_df) <- words_df$words
set.seed(1)

words_plot <- ggplot(head(words_df, 50), aes(label = words, size = count)) +
  geom_text_wordcloud_area(rm_outside = TRUE, shape = "square", color = "#FF00FF") +
  scale_size_area(max_size = 30) +
  theme_minimal() +labs(y= "", x = "")+
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"))
words_plot


emoji_df <- df[str_detect(df$words, emoji_regex), ]
emoji_df$words <- gsub("[^\\p{So}]", "", emoji_df$words, perl = TRUE)

emoji_df <- emoji_df %>% 
  select(words) %>% 
  group_by(words) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(words != "")

rownames(emoji_df) <- emoji_df$words
emoji_plot <- ggplot(head(emoji_df, 50), aes(label = words, size = count)) +
  geom_text_wordcloud(shape = "square", color = "#FF00FF") +
  scale_size_area(max_size = 50) +
  theme_minimal() +labs(y= "", x = "") +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"))
emoji_plot
