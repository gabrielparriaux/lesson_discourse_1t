# second-teacher-columns-management ----

# delete unnecessary columns
drops2 <- c("Out","Duration.y", "Commented_Text", "Thread_Resolved")
second_teacher_reduced <- second_teacher_reduced[ , !(names(second_teacher_reduced) %in% drops2)]

# rename columns after merge
names(second_teacher_reduced)[names(second_teacher_reduced) == 'Duration.x'] <- 'Duration'
names(second_teacher_reduced)[names(second_teacher_reduced) == 'Comment_1'] <- 'To'

# remove unnecessary email in To column
second_teacher_reduced$To<-gsub("name@anonymous.org: ","",as.character(second_teacher_reduced$To))

# triplicate column to separate comments
second_teacher_reduced$Comment_didactic <- second_teacher_reduced$To
second_teacher_reduced$Comment_bugs <- second_teacher_reduced$To

## To column ----

# delete all comments that do not contain adresse: in To column and replace them with NA values
second_teacher_reduced$To <- with(second_teacher_reduced, ifelse(!grepl("*ADRESSE:*", second_teacher_reduced$To), NA, second_teacher_reduced$To))

# remove other comments about didactic or bugs from the To column
# ADRESSE: comes always first in the comments, so we need to remove what comes after
# BUG ENREGISTREMENT.* selects all the text starting from BUG ENREGISTREMENT and everything that follows
second_teacher_reduced$To <- gsub("BUG ENREGISTREMENT.*","",as.character(second_teacher_reduced$To))
second_teacher_reduced$To <- gsub("DIDACTIQUE.*","",as.character(second_teacher_reduced$To))

# remove possible \n introduced by a line break when introducing the comment in Trint
first_teacher_reduced$To <- gsub("\n"," ",as.character(first_teacher_reduced$To))

## Comment_didactic column ----

# delete all comments that do not contain DIDACTIQUE: in Comment_didactic column
second_teacher_reduced$Comment_didactic <- with(second_teacher_reduced, ifelse(!grepl("*DIDACTIQUE:*", second_teacher_reduced$Comment_didactic), "", second_teacher_reduced$Comment_didactic))

# remove other comments about adresse: or bugs from the Comment_didactic column
# DIDACTIQUE: comes in the middle in the comments, so there are things to remove before and after…
# .*DIDACTIQUE selects all the text that comes before and up to DIDACTIQUE
second_teacher_reduced$Comment_didactic <-gsub(".*DIDACTIQUE","DIDACTIQUE",as.character(second_teacher_reduced$Comment_didactic))
# BUG ENREGISTREMENT.* selects all the text starting from BUG ENREGISTREMENT and everything that follows
second_teacher_reduced$Comment_didactic <- gsub("BUG ENREGISTREMENT.*","",as.character(second_teacher_reduced$Comment_didactic))

# remove possible \n introduced by a line break when introducing the comment in Trint
first_teacher_reduced$Comment_didactic <- gsub("\n"," ",as.character(first_teacher_reduced$Comment_didactic))

## Comment_bugs column ----

# delete all comments that do not contain BUG in Comment_bugs column
second_teacher_reduced$Comment_bugs <- with(second_teacher_reduced, ifelse(!grepl("*BUG*", second_teacher_reduced$Comment_bugs), "", second_teacher_reduced$Comment_bugs))

# remove other comments about adresse: or didactic from the Comment_bugs column
# BUG ENREGISTREMENT always comes last in the comments, so we need to remove all that comes before
# .*BUG selects all the text that comes before and up to BUG
second_teacher_reduced$Comment_bugs <-gsub(".*BUG","BUG",as.character(second_teacher_reduced$Comment_bugs))

# remove possible \n introduced by a line break when introducing the comment in Trint
first_teacher_reduced$Comment_bugs <- gsub("\n"," ",as.character(first_teacher_reduced$Comment_bugs))

# copy ADRESSE: values in To column when value is NA, direction down (default)
second_teacher_reduced <- second_teacher_reduced %>% fill(To)

# replace values into column to remove ADDRESS and to put in English
second_teacher_reduced$To <- with(second_teacher_reduced, ifelse(grepl("ADRESSE: classe", second_teacher_reduced$To), "class", second_teacher_reduced$To))
second_teacher_reduced$To <- with(second_teacher_reduced, ifelse(grepl("ADRESSE: élève", second_teacher_reduced$To), "pupil", second_teacher_reduced$To))
second_teacher_reduced$To <- with(second_teacher_reduced, ifelse(grepl("ADRESSE: enseignant", second_teacher_reduced$To), "teacher", second_teacher_reduced$To))

# second-teacher-discourse-tables ----

## second_teacher_to_class ----

# remove rows that are do not address whole class
second_teacher_to_class <- subset(second_teacher_reduced, To=="class")

# delete unnecessary columns
drops9 <- c("Speaker", "To", "Comment_didactic", "Comment_bugs")
second_teacher_to_class <- second_teacher_to_class[ , !(names(second_teacher_to_class) %in% drops9)]

## second_teacher_to_pupil ----

# remove rows that are do not address pupils
second_teacher_to_pupil <- subset(second_teacher_reduced, To=="pupil")

# delete unnecessary columns
second_teacher_to_pupil <- second_teacher_to_pupil[ , !(names(second_teacher_to_pupil) %in% drops9)]

## second_teacher_to_teacher ----

# remove rows that are do not address teacher
second_teacher_to_teacher <- subset(second_teacher_reduced, To=="teacher")

# delete unnecessary columns
second_teacher_to_teacher <- second_teacher_to_teacher[ , !(names(second_teacher_to_teacher) %in% drops9)]

## second_teacher_complete ----

# simple copy of second_teacher_reduced
second_teacher_complete <- second_teacher_reduced

# delete unnecessary columns
drops10 <- c("Speaker", "Comment_didactic", "Comment_bugs")
second_teacher_complete <- second_teacher_complete[ , !(names(second_teacher_complete) %in% drops10)]

## second_teacher_comments_dida ----

# remove rows where Comment_didactic is empty
second_teacher_comments_dida <- subset(second_teacher_reduced, Comment_didactic!="")

# delete unnecessary columns
drops11 <- c("Duration", "Speaker", "To", "Comment_bugs")
second_teacher_comments_dida <- second_teacher_comments_dida[ , !(names(second_teacher_comments_dida) %in% drops11)]

## second_teacher_comments_bugs ----

# remove rows where Comment_bugs is empty
second_teacher_comments_bugs <- subset(second_teacher_reduced, Comment_bugs!="")

# delete unnecessary columns
drops11b <- c("Duration", "Speaker", "To", "Comment_didactic")
second_teacher_comments_bugs <- second_teacher_comments_bugs[ , !(names(second_teacher_comments_bugs) %in% drops11b)]

# merged-teachers ----

# rbind the two tables
merged_teachers <- rbind(first_teacher_reduced, second_teacher_reduced)

# sort by In column
merged_teachers <- merged_teachers[order(merged_teachers$In, decreasing = FALSE), ]

# time-management ----

# remove digits for seconds
merged_teachers$In = substr(merged_teachers$In, 1, nchar(merged_teachers$In)-4)

# store first statement starting time in a variable
lesson_start_time <- period_to_seconds(hms(merged_teachers$In[1]))

# remove pause duration if needed (only for statements after the pause)
if (pause_yes) {
  # loop through first_teacher_reduced$In and remove pause duration for all values after pause_start_time
  merged_teachers <- mutate(merged_teachers, In = case_when(
    period_to_seconds(hms(merged_teachers$In)) > pause_start_time ~ strftime(as.POSIXct("00:00:00", format="%H:%M:%S") + (seconds_to_period(period_to_seconds(hms(merged_teachers$In)) - pause_duration_in_s)), format="%H:%M:%S"), 
    TRUE   ~ In 
  ))
}

# adjust starting time to zero and move all the timing backward accordingly
merged_teachers <- mutate(merged_teachers, In = case_when(
  TRUE   ~ strftime(as.POSIXct("00:00:00", format="%H:%M:%S") + (seconds_to_period(period_to_seconds(hms(merged_teachers$In)) - lesson_start_time)), format="%H:%M:%S")
))

# store last statement starting time in a variable
lesson_end_time <- period_to_seconds(hms(merged_teachers$In[nrow(merged_teachers)]))

# total duration of the lesson in seconds
lesson_duration <- lesson_end_time + merged_teachers$Duration[nrow(merged_teachers)]

# merged-teachers-discourse-tables ----

## merged_teachers_to_class ----

# remove rows that are do not address whole class
merged_teachers_to_class <- subset(merged_teachers, To=="class")

# delete unnecessary columns
drops6 <- c("To", "Comment_didactic", "Comment_bugs")
merged_teachers_to_class <- merged_teachers_to_class[ , !(names(merged_teachers_to_class) %in% drops6)]

## merged_teachers_to_pupil ----

# remove rows that are do not address pupils
merged_teachers_to_pupil <- subset(merged_teachers, To=="pupil")

# delete unnecessary columns
merged_teachers_to_pupil <- merged_teachers_to_pupil[ , !(names(merged_teachers_to_pupil) %in% drops6)]

## merged_teachers_to_teacher ----

# remove rows that are do not address teacher
merged_teachers_to_teacher <- subset(merged_teachers, To=="teacher")

# delete unnecessary columns
merged_teachers_to_teacher <- merged_teachers_to_teacher[ , !(names(merged_teachers_to_teacher) %in% drops6)]

## merged_teachers_complete ----

# simple copy of merged_teachers
merged_teachers_complete <- merged_teachers

# delete unnecessary columns
drops7 <- c("Comment_didactic", "Comment_bugs")
merged_teachers_complete <- merged_teachers_complete[ , !(names(merged_teachers_complete) %in% drops7)]

## merged_teachers_comments_dida ----

# remove rows where Comment_didactic is empty
merged_teachers_comments_dida <- subset(merged_teachers, Comment_didactic!="")

# delete unnecessary columns
drops8 <- c("Duration", "To", "Comment_bugs")
merged_teachers_comments_dida <- merged_teachers_comments_dida[ , !(names(merged_teachers_comments_dida) %in% drops8)]

## merged_teachers_comments_bugs ----

# remove rows where Comment_bugs is empty
merged_teachers_comments_bugs <- subset(merged_teachers, Comment_bugs!="")

# delete unnecessary columns
drops12 <- c("Duration", "To", "Comment_didactic")
merged_teachers_comments_bugs <- merged_teachers_comments_bugs[ , !(names(merged_teachers_comments_bugs) %in% drops12)]

# compute-time-slices ----

# intervals in seconds
time_interval <- 600

# number of intervals
number_of_intervals <- ceiling(lesson_duration / time_interval)

# for loop to create the time slices
time_slices <- data.frame()
for (i in 1:number_of_intervals) {
  time_slices <- rbind(time_slices, data.frame(start = (i-1)*time_interval, end = i*time_interval))
}

# for loop to rename rows
for (i in 1:nrow(time_slices)) {
  rownames(time_slices)[i] <- paste0("time[", (i-1)*10, "-", i*10, "]")
}

# assign-statements-to-time-slices ----

# for merged teachers 

# compute a new column with In value converted in seconds in merged_teachers_complete
merged_teachers_complete$In_sec <- period_to_seconds(hms(merged_teachers_complete$In))

# create an empty column for time_slice
merged_teachers_complete$time_slice = NA_character_

# loop through the dataframe to fill in the time_slice column until number_of_intervals-1
for(i in 1:(number_of_intervals-1)){
  merged_teachers_complete <- merged_teachers_complete %>%
    mutate(time_slice = case_when(
      In_sec >= time_slices[i, 1] & In_sec < time_slices[i+1, 1] ~ rownames(time_slices)[i], 
      TRUE ~ time_slice
    ))
}

# fill in the time_slice column for the last interval 
merged_teachers_complete <- merged_teachers_complete %>%
  mutate(time_slice = case_when(
    In_sec >= time_slices[number_of_intervals, 1] ~ rownames(time_slices)[number_of_intervals],
    TRUE ~ time_slice
  ))

# remove In_sec column
merged_teachers_complete <- subset(merged_teachers_complete, select = -c(In_sec))

# Here we have a complete corpus (in the form of a dataframe) with all the words in their original form

# main-corpus-creation ----

# create a corpus object with quanteda from full discourse of merged teachers
corpus_merged_teachers <- corpus(merged_teachers_complete, text_field = "Text")

# for the record, display one full text entry of the object
# as.character(corpus_first_teacher)[1]

# save summary of the corpus in a table
corpus_merged_teachers.stats <- summary(corpus_merged_teachers, n = 1000000)

# create a tokens object with the full corpus unlemmatised (used just after for statistics, and later for dispersion and word in context)
tok_merged_teachers <- tokens(corpus_merged_teachers, remove_punct = TRUE, split_hyphens = FALSE)
tok_merged_teachers <- tokens_tolower(tok_merged_teachers)

# compute-general-stats-for-lesson ----

# as the corpus has not been lemmatised and is complete, the stats we get here are for the original text complete without lemmatisation. Punctuation is removed.

# compute values for text length of first_teacher
text_length_first_teacher_to_class <- ntoken(tokens_subset(tok_merged_teachers, Speaker == first_teacher & To == "class")) %>% sum()
text_length_first_teacher_to_pupil <- ntoken(tokens_subset(tok_merged_teachers, Speaker == first_teacher & To == "pupil")) %>% sum()
text_length_first_teacher_to_teacher <- ntoken(tokens_subset(tok_merged_teachers, Speaker == first_teacher & To == "teacher")) %>% sum()
text_length_first_teacher <- ntoken(tokens_subset(tok_merged_teachers, Speaker == first_teacher)) %>% sum()

# compute values for text length of second_teacher
text_length_second_teacher_to_class <- ntoken(tokens_subset(tok_merged_teachers, Speaker == second_teacher & To == "class")) %>% sum()
text_length_second_teacher_to_pupil <- ntoken(tokens_subset(tok_merged_teachers, Speaker == second_teacher & To == "pupil")) %>% sum()
text_length_second_teacher_to_teacher <- ntoken(tokens_subset(tok_merged_teachers, Speaker == second_teacher & To == "teacher")) %>% sum()
text_length_second_teacher <- ntoken(tokens_subset(tok_merged_teachers, Speaker == second_teacher)) %>% sum()

# compute values for text length of merged_teachers
text_length_merged_teachers_to_class <- ntoken(tokens_subset(tok_merged_teachers, To == "class")) %>% sum()
text_length_merged_teachers_to_pupil <- ntoken(tokens_subset(tok_merged_teachers, To == "pupil")) %>% sum()
text_length_merged_teachers_to_teacher <- ntoken(tokens_subset(tok_merged_teachers, To == "teacher")) %>% sum()
text_length_merged_teachers <- ntoken(tokens_subset(tok_merged_teachers)) %>% sum()

# compute values for vocabulary size of first_teacher
vocabulary_size_first_teacher_to_class <- length(types(tokens_subset(tok_merged_teachers, Speaker == first_teacher & To == "class")))
vocabulary_size_first_teacher_to_pupil <- length(types(tokens_subset(tok_merged_teachers, Speaker == first_teacher & To == "pupil")))
vocabulary_size_first_teacher_to_teacher <- length(types(tokens_subset(tok_merged_teachers, Speaker == first_teacher & To == "teacher")))
vocabulary_size_first_teacher <- length(types(tokens_subset(tok_merged_teachers, Speaker == first_teacher)))

# compute values for vocabulary size of second_teacher
vocabulary_size_second_teacher_to_class <- length(types(tokens_subset(tok_merged_teachers, Speaker == second_teacher & To == "class")))
vocabulary_size_second_teacher_to_pupil <- length(types(tokens_subset(tok_merged_teachers, Speaker == second_teacher & To == "pupil")))
vocabulary_size_second_teacher_to_teacher <- length(types(tokens_subset(tok_merged_teachers, Speaker == second_teacher & To == "teacher")))
vocabulary_size_second_teacher <- length(types(tokens_subset(tok_merged_teachers, Speaker == second_teacher)))

# compute values for vocabulary size of merged_teachers
vocabulary_size_merged_teachers_to_class <- length(types(tokens_subset(tok_merged_teachers, To == "class")))
vocabulary_size_merged_teachers_to_pupil <- length(types(tokens_subset(tok_merged_teachers, To == "pupil")))
vocabulary_size_merged_teachers_to_teacher <- length(types(tokens_subset(tok_merged_teachers, To == "teacher")))
vocabulary_size_merged_teachers <- length(types(tok_merged_teachers))

# compute values for duration of first_teacher 
duration_first_teacher_to_class <- tokens_subset(tok_merged_teachers, Speaker == first_teacher & To == "class")$Duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
duration_first_teacher_to_pupil <- tokens_subset(tok_merged_teachers, Speaker == first_teacher & To == "pupil")$Duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
duration_first_teacher_to_teacher <- tokens_subset(tok_merged_teachers, Speaker == first_teacher & To == "teacher")$Duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
duration_first_teacher <- tokens_subset(tok_merged_teachers, Speaker == first_teacher)$Duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }

# compute values for duration of second_teacher
duration_second_teacher_to_class <- tokens_subset(tok_merged_teachers, Speaker == second_teacher & To == "class")$Duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
duration_second_teacher_to_pupil <- tokens_subset(tok_merged_teachers, Speaker == second_teacher & To == "pupil")$Duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
duration_second_teacher_to_teacher <- tokens_subset(tok_merged_teachers, Speaker == second_teacher & To == "teacher")$Duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
duration_second_teacher <- tokens_subset(tok_merged_teachers, Speaker == second_teacher)$Duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }

# compute values for duration of merged_teachers
duration_merged_teachers_to_class <- tokens_subset(tok_merged_teachers, To == "class")$Duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
duration_merged_teachers_to_pupil <- tokens_subset(tok_merged_teachers, To == "pupil")$Duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
duration_merged_teachers_to_teacher <- tokens_subset(tok_merged_teachers, To == "teacher")$Duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
duration_merged_teachers <- tok_merged_teachers$Duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }

# count number of statements for first_teacher
nbr_statements_first_teacher_to_class <- length(tokens_subset(tok_merged_teachers, Speaker == first_teacher & To == "class"))
nbr_statements_first_teacher_to_pupil <- length(tokens_subset(tok_merged_teachers, Speaker == first_teacher & To == "pupil"))
nbr_statements_first_teacher_to_teacher <- length(tokens_subset(tok_merged_teachers, Speaker == first_teacher & To == "teacher"))
nbr_statements_first_teacher <- length(tokens_subset(tok_merged_teachers, Speaker == first_teacher))

# count number of statements for second_teacher
nbr_statements_second_teacher_to_class <- length(tokens_subset(tok_merged_teachers, Speaker == second_teacher & To == "class"))
nbr_statements_second_teacher_to_pupil <- length(tokens_subset(tok_merged_teachers, Speaker == second_teacher & To == "pupil"))
nbr_statements_second_teacher_to_teacher <- length(tokens_subset(tok_merged_teachers, Speaker == second_teacher & To == "teacher"))
nbr_statements_second_teacher <- length(tokens_subset(tok_merged_teachers, Speaker == second_teacher))

# count number of statements for merged_teachers
nbr_statements_merged_teachers_to_class <- length(tokens_subset(tok_merged_teachers, To == "class"))
nbr_statements_merged_teachers_to_pupil <- length(tokens_subset(tok_merged_teachers, To == "pupil"))
nbr_statements_merged_teachers_to_teacher <- length(tokens_subset(tok_merged_teachers, To == "teacher"))
nbr_statements_merged_teachers <- length(tok_merged_teachers)

# first-teacher-stats-table-creation ----

# create one vector for each column of the df
ft_recipients <- c("Whole class", "Individual pupil", "Other teacher", "Altogether")
ft_number_of_statements <- c(nbr_statements_first_teacher_to_class, nbr_statements_first_teacher_to_pupil, nbr_statements_first_teacher_to_teacher, nbr_statements_first_teacher)
ft_duration <- c(duration_first_teacher_to_class, duration_first_teacher_to_pupil, duration_first_teacher_to_teacher, duration_first_teacher)
ft_text_length <- c(text_length_first_teacher_to_class, text_length_first_teacher_to_pupil, text_length_first_teacher_to_teacher, text_length_first_teacher)
ft_vocabulary_size <- c(vocabulary_size_first_teacher_to_class, vocabulary_size_first_teacher_to_pupil, vocabulary_size_first_teacher_to_teacher, vocabulary_size_first_teacher)

# create dataframe with those vectors
first_teacher_stats_table <- data.frame(ft_recipients, ft_number_of_statements, ft_duration, ft_text_length, ft_vocabulary_size)

# change column names
colnames(first_teacher_stats_table) <- c("Address to", "No. of statements","Duration", "Text length (in words)", "Vocabulary size (in words)")

first_teacher_stats_table_caption <- paste("Statistics on classroom discourse for", first_teacher)

# second-teacher-stats-table-creation ----

# create one vector for each column of the df
st_recipients <- c("Whole class", "Individual pupil", "Other teacher", "Altogether")
st_number_of_statements <- c(nbr_statements_second_teacher_to_class, nbr_statements_second_teacher_to_pupil, nbr_statements_second_teacher_to_teacher, nbr_statements_second_teacher)
st_duration <- c(duration_second_teacher_to_class, duration_second_teacher_to_pupil, duration_second_teacher_to_teacher, duration_second_teacher)
st_text_length <- c(text_length_second_teacher_to_class, text_length_second_teacher_to_pupil, text_length_second_teacher_to_teacher, text_length_second_teacher)
st_vocabulary_size <- c(vocabulary_size_second_teacher_to_class, vocabulary_size_second_teacher_to_pupil, vocabulary_size_second_teacher_to_teacher, vocabulary_size_second_teacher)

# create dataframe with those vectors
second_teacher_stats_table <- data.frame(st_recipients, st_number_of_statements, st_duration, st_text_length, st_vocabulary_size)

# change column names
colnames(second_teacher_stats_table) <- c("Address to", "No. of statements","Duration", "Text length (in words)", "Vocabulary size (in words)")

second_teacher_stats_table_caption <- paste("Statistics on classroom discourse for", second_teacher)

# merged-teachers-stats-table-creation ----

# create one vector for each column of the df
mt_recipients <- c("Whole class", "Individual pupil", "Other teacher", "Altogether")
mt_number_of_statements <- c(nbr_statements_merged_teachers_to_class, nbr_statements_merged_teachers_to_pupil, nbr_statements_merged_teachers_to_teacher, nbr_statements_merged_teachers)
mt_duration <- c(duration_merged_teachers_to_class, duration_merged_teachers_to_pupil, duration_merged_teachers_to_teacher, duration_merged_teachers)
mt_text_length <- c(text_length_merged_teachers_to_class, text_length_merged_teachers_to_pupil, text_length_merged_teachers_to_teacher, text_length_merged_teachers)
mt_vocabulary_size <- c(vocabulary_size_merged_teachers_to_class, vocabulary_size_merged_teachers_to_pupil, vocabulary_size_merged_teachers_to_teacher, vocabulary_size_merged_teachers)

# create dataframe with those vectors
merged_teachers_stats_table <- data.frame(mt_recipients, mt_number_of_statements, mt_duration, mt_text_length, mt_vocabulary_size)

# change column names
colnames(merged_teachers_stats_table) <- c("Address to", "No. of statements","Duration", "Text length (in words)", "Vocabulary size (in words)")

# Save merged_teachers_stats_table to a file for export to global analysis
saveRDS(merged_teachers_stats_table, file = paste0(data_dir, "/export/", class_id, "_",lesson_topic, "_merged_teachers_stats_table.rds"))

merged_teachers_stats_table_caption <- paste("Statistics on classroom discourse for", first_teacher, "and", second_teacher)

# compute-number-of-tokens-by-statement ----

# test --- as presented in https://api.rpubs.com/cbpuschmann/textmining

# plot the number of tokens per statement, with colors for To
tokens_per_statement_by_address_to <- ggplot(
  corpus_merged_teachers.stats, 
  aes(x = fct_inorder(Text), y = Tokens, group=1, color=To)) + 
  geom_line(linewidth = .3) + 
  geom_point(size = .3) + 
  labs(title = "", x = "Statements", y = "Words") +
  # dark_mode(theme_bw()) + # if we want to use dark mode
  theme_bw() +
  theme(legend.title=element_text(size=13), legend.text=element_text(size=12)) +
  # rotate x labels
  theme(axis.text.x = element_text(angle = 0, vjust = 0.8, hjust=1)) + 
  # limit the number of x labels
  scale_x_discrete(breaks = c(corpus_merged_teachers.stats$Text[1], corpus_merged_teachers.stats$Text[nrow(corpus_merged_teachers.stats)/4], corpus_merged_teachers.stats$Text[nrow(corpus_merged_teachers.stats)/2], corpus_merged_teachers.stats$Text[nrow(corpus_merged_teachers.stats)/4*3], corpus_merged_teachers.stats$Text[nrow(corpus_merged_teachers.stats)]))

# Save tokens_per_statement_by_address_to to a file for export to global analysis
saveRDS(tokens_per_statement_by_address_to, file = paste0(data_dir, "/export/", class_id, "_", lesson_topic, "_tokens_per_statement_by_recipient.rds"))

# smoothing intents

# same plot than before, but with smoothing function integrated in ggplot with library tidyquant (instead of geom_line())
# https://search.r-project.org/CRAN/refmans/tidyquant/html/geom_ma.html
# library(tidyquant)
# ggplot(corpus_merged_teachers.stats, aes(Text, Tokens, group=1, color=To)) + geom_point(size = 1) + ggtitle("Tokens per statement") + geom_ma(ma_fun = DEMA, n = 3, linetype = "solid", size = 2)

# smoothing with rollmean and ksmooth, creating a temporary df
# https://boostedml.com/2020/05/an-introduction-to-time-series-smoothing-in-r.html
# temp <- corpus_merged_teachers.stats %>% select(Text, Tokens)
# plot(temp$Tokens)
# Simple Moving Average (SMA)
# lines(rollmean(temp$Tokens,3),col='blue')
# Triangular Moving Average (TMA)
# lines(rollmean(rollmean(temp$Tokens,2),2),col='blue')
# this one doesn’t work because I don’t know how to plot a serie of individual values (it requires two numeric axes)
# lines(ksmooth(temp$Text, temp$Tokens, "normal", bandwidth = 2), col = 2)

# plot the number of tokens per statement, with colors for Speaker
tokens_per_statement_by_teacher <- ggplot(
  corpus_merged_teachers.stats, 
  aes(x = fct_inorder(Text), y = Tokens, group=1, color=Speaker)) + 
  geom_line(linewidth = .3) + 
  geom_point(size = .3) + 
  labs(title = "", x = "Statements", y = "Words") +
  # dark_mode(theme_bw()) + # if we want to use dark mode
  theme_bw() +
  theme(legend.title=element_text(size=13), legend.text=element_text(size=12)) +
  # rotate x labels
  theme(axis.text.x = element_text(angle = 0, vjust = 0.8, hjust=1)) + 
  scale_x_discrete(breaks = c(corpus_merged_teachers.stats$Text[1], corpus_merged_teachers.stats$Text[nrow(corpus_merged_teachers.stats)/4], corpus_merged_teachers.stats$Text[nrow(corpus_merged_teachers.stats)/2], corpus_merged_teachers.stats$Text[nrow(corpus_merged_teachers.stats)/4*3], corpus_merged_teachers.stats$Text[nrow(corpus_merged_teachers.stats)]))

# Save tokens_per_statement_by_teacher to a file for export to global analysis
saveRDS(tokens_per_statement_by_teacher, file = paste0(data_dir, "/export/", class_id, "_", lesson_topic, "_tokens_per_statement_by_teacher.rds"))

# plot the number of types per statement
ggplot(corpus_merged_teachers.stats, aes(Text, Types, group=1)) + geom_line() + geom_point() + ggtitle("Types per statement")

# plot the number of sentences per statement
ggplot(corpus_merged_teachers.stats, aes(Text, Sentences, group=1)) + geom_line() + geom_point() + ggtitle("Sentences per statement")

# plot the three together
ggplot(corpus_merged_teachers.stats %>% gather(Types, Tokens, Sentences, key = "Unit", value = "Number"), aes(Text, Number, group = Unit, col = Unit)) + geom_line(linewidth = 1) + ggtitle("Tokens, Types and Sentences per statement")

# plot the type-token ratio per statement
ggplot(corpus_merged_teachers.stats, aes(Tokens, Types, group=1, label = Text)) + geom_smooth(method = "lm", se = FALSE) + geom_text(check_overlap = T) + ggtitle("Type-Token ratio per statement")

# compute-tokens-in-context-and-dispersion ----

# find a word in context
matching <- kwic(tok_merged_teachers, "program*")
head(matching, 20)

# find two words in context
matching <- kwic(tok_merged_teachers, c("avance*", "tourne*"), window = 10, case_insensitive = FALSE)
head(matching, 30)

# dispersion of a word in the text
textplot_xray(kwic(tok_merged_teachers, "commande*", valuetype = "regex", case_insensitive = FALSE), kwic(tok_merged_teachers, "program*", valuetype = "regex", case_insensitive = FALSE))