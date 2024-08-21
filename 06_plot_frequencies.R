# first-teacher ----

## plot-frequency-first-teacher ----

# subset dfm for first_teacher and stat of most frequent words
dfm_first_teacher <- dfm_subset(dfm_without_stopwords, Speaker == first_teacher)
freq_first_teacher <- textstat_frequency(dfm_first_teacher)

# compute plot of the most frequent words
plot_freq_first_teacher <- freq_first_teacher[1:50, ] %>%
  mutate(feature = fct_reorder(feature, frequency)) %>%
  ggplot( aes(feature,frequency,label=frequency,fill=feature)) + 
  geom_bar(stat="identity",  fill="#4980B8") +
  geom_text(size=4,hjust=1) +
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
plot_freq_first_teacher_caption <- paste("Most frequent words in classroom discourse lexicon by", first_teacher)

## plot-frequency-first-teacher-to-class ----

# subset dfm for first_teacher with address_to class and stat of most frequent words
dfm_first_teacher_to_class <- dfm_subset(dfm_without_stopwords, To == "class" & Speaker == first_teacher)
freq_first_teacher_to_class <- textstat_frequency(dfm_first_teacher_to_class)

# compute plot of the most frequent words
plot_freq_first_teacher_to_class <- freq_first_teacher_to_class[1:50, ] %>%
  mutate(feature = fct_reorder(feature, frequency)) %>%
  ggplot( aes(feature,frequency,label=frequency,fill=feature)) + 
  geom_bar(stat="identity",  fill="#4980B8") +
  geom_text(size=4,hjust=1) +
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
plot_freq_first_teacher_by_recipient_caption <- paste("Most frequent words in", first_teacher, " classroom discourse depending on the address")

## plot-frequency-first-teacher-to-pupil ----

# subset dfm for first_teacher with address_to class and stat of most frequent words
dfm_first_teacher_to_pupil <- dfm_subset(dfm_without_stopwords, To == "pupil" & Speaker == first_teacher)
freq_first_teacher_to_pupil <- textstat_frequency(dfm_first_teacher_to_pupil)

# compute plot of the most frequent words
plot_freq_first_teacher_to_pupil <- freq_first_teacher_to_pupil[1:50, ] %>%
  mutate(feature = fct_reorder(feature, frequency)) %>%
  ggplot( aes(feature,frequency,label=frequency,fill=feature)) + 
  geom_bar(stat="identity",  fill="#4980B8") +
  geom_text(size=4,hjust=1) +
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

## plot-frequency-first-teacher-to-teacher ----

# subset dfm for first_teacher with address_to class and stat of most frequent words
dfm_first_teacher_to_teacher <- dfm_subset(dfm_without_stopwords, To == "teacher" & Speaker == first_teacher)
freq_first_teacher_to_teacher <- textstat_frequency(dfm_first_teacher_to_teacher)

# compute plot of the most frequent words
plot_freq_first_teacher_to_teacher <- freq_first_teacher_to_teacher[1:25, ] %>%
  mutate(feature = fct_reorder(feature, frequency)) %>%
  ggplot( aes(feature,frequency,label=frequency,fill=feature)) + 
  geom_bar(stat="identity",  fill="#4980B8") +
  geom_text(size=4,hjust=1) +
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

# second-teacher ----

## plot-frequency-second-teacher ----

# subset dfm for first_teacher and stat of most frequent words
dfm_second_teacher <- dfm_subset(dfm_without_stopwords, Speaker == second_teacher)
freq_second_teacher <- textstat_frequency(dfm_second_teacher)

# compute plot of the most frequent words
plot_freq_second_teacher <- freq_second_teacher[1:50, ] %>%
  mutate(feature = fct_reorder(feature, frequency)) %>%
  ggplot( aes(feature,frequency,label=frequency,fill=feature)) + 
  geom_bar(stat="identity",  fill="#4980B8") +
  geom_text(size=4,hjust=1) +
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
plot_freq_second_teacher_caption <- paste("Most frequent words in classroom discourse lexicon by", second_teacher)

## plot-frequency-second-teacher-to-class ----

# subset dfm for first_teacher with address_to class and stat of most frequent words
dfm_second_teacher_to_class <- dfm_subset(dfm_without_stopwords, To == "class" & Speaker == second_teacher)
freq_second_teacher_to_class <- textstat_frequency(dfm_second_teacher_to_class)

# compute plot of the most frequent words
plot_freq_second_teacher_to_class <- freq_second_teacher_to_class[1:50, ] %>%
  mutate(feature = fct_reorder(feature, frequency)) %>%
  ggplot( aes(feature,frequency,label=frequency,fill=feature)) + 
  geom_bar(stat="identity",  fill="#4980B8") +
  geom_text(size=4,hjust=1) +
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
plot_freq_second_teacher_by_recipient_caption <- paste("Most frequent words in", second_teacher, " classroom discourse depending on the address")

## plot-frequency-second-teacher-to-pupil ----

# subset dfm for first_teacher with address_to class and stat of most frequent words
dfm_second_teacher_to_pupil <- dfm_subset(dfm_without_stopwords, To == "pupil" & Speaker == second_teacher)
freq_second_teacher_to_pupil <- textstat_frequency(dfm_second_teacher_to_pupil)

# compute plot of the most frequent words
plot_freq_second_teacher_to_pupil <- freq_second_teacher_to_pupil[1:50, ] %>%
  mutate(feature = fct_reorder(feature, frequency)) %>%
  ggplot( aes(feature,frequency,label=frequency,fill=feature)) + 
  geom_bar(stat="identity",  fill="#4980B8") +
  geom_text(size=4,hjust=1) +
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

## plot-frequency-second-teacher-to-teacher ----

# subset dfm for first_teacher with address_to class and stat of most frequent words
dfm_second_teacher_to_teacher <- dfm_subset(dfm_without_stopwords, To == "teacher" & Speaker == second_teacher)
freq_second_teacher_to_teacher <- textstat_frequency(dfm_second_teacher_to_teacher)

# compute plot of the most frequent words
plot_freq_second_teacher_to_teacher <- freq_second_teacher_to_teacher[1:25, ] %>%
  mutate(feature = fct_reorder(feature, frequency)) %>%
  ggplot( aes(feature,frequency,label=frequency,fill=feature)) + 
  geom_bar(stat="identity",  fill="#4980B8") +
  geom_text(size=4,hjust=1) +
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

# merged-teachers ----

## plot-frequency-merged-teachers-lemmatized ----

# lemmatized

# compute plot of the most frequent words
plot_freq_main_corp_lemm <- freq_main_corp_lemm[1:50, ] %>%
  mutate(feature = fct_reorder(feature, frequency)) %>%
  ggplot( aes(feature,frequency,label=frequency,fill=feature)) + 
  geom_bar(stat="identity",  fill="#4980B8") +
  geom_text(size=4,hjust=1) +
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
plot_freq_main_corp_lemm_caption <- paste("Most frequent words in classroom discourse lexicon by", first_teacher, "and", second_teacher)