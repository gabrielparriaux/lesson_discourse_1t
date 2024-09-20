# first_teacher and lesson discourse ----

## plot-frequency-lesson-discourse ----

# lemmatized

# compute plot of the most frequent words
plot_freq_lesson_disc <- freq_lesson_disc[1:50, ] %>%
  mutate(feature = fct_reorder(feature, frequency)) %>%
  ggplot( aes(feature,frequency,label=frequency,fill=feature)) + 
  geom_point(stat="identity",  fill="#4980B8") +
  geom_text(size=3.5,hjust=1.4) +
  ggtitle("") +
  xlab("") +
  ylab("Count") +
  coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size = 7),
        plot.title = element_text(size=14, face="bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.margin = margin(0, 10, 10, 10, unit = "pt")
  )

# anticipated creation of the caption to use as a variable in the chunk option for display
plot_freq_lesson_disc_caption <- paste("Most frequent words in lesson's discourses lexicon by", first_teacher, "(lesson", this_lesson_id, ")")