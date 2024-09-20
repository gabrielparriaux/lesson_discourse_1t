# first-teacher-columns-management ----

# delete unnecessary columns
drops <- c("Out","Duration.y", "Commented_Text", "Thread_Resolved")
first_teacher_disc_df <- first_teacher_disc_df[ , !(names(first_teacher_disc_df) %in% drops)]

# rename columns after merge
names(first_teacher_disc_df)[names(first_teacher_disc_df) == 'Duration.x'] <- 'statement_duration'
names(first_teacher_disc_df)[names(first_teacher_disc_df) == 'Comment_1'] <- 'recipient'

# remove unnecessary email in recipient column
first_teacher_disc_df$recipient<-gsub("gabriel.parriaux@hepl.ch: ","",as.character(first_teacher_disc_df$recipient))

# triplicate column to separate comments
first_teacher_disc_df$comment_didactic <- first_teacher_disc_df$recipient
first_teacher_disc_df$comment_bugs <- first_teacher_disc_df$recipient

## recipient column ----

# delete all comments that do not contain adresse: in recipient column and replace them with NA values
first_teacher_disc_df$recipient <- with(first_teacher_disc_df, ifelse(!grepl("*ADRESSE:*", first_teacher_disc_df$recipient), NA, first_teacher_disc_df$recipient))

# remove other comments about didactic or bugs from the recipient column
# ADRESSE: comes always first in the comments, so we need to remove what comes after
# BUG ENREGISTREMENT.* selects all the text starting from BUG ENREGISTREMENT and everything that follows
first_teacher_disc_df$recipient <- gsub("BUG ENREGISTREMENT.*","",as.character(first_teacher_disc_df$recipient))
first_teacher_disc_df$recipient <- gsub("DIDACTIQUE.*","",as.character(first_teacher_disc_df$recipient))

# remove possible \n introduced by a line break when introducing the comment in Trint
first_teacher_disc_df$recipient <- gsub("\n"," ",as.character(first_teacher_disc_df$recipient))

## Comment_didactic column ----

# delete all comments that do not contain DIDACTIQUE: in comment_didactic column
first_teacher_disc_df$comment_didactic <- with(first_teacher_disc_df, ifelse(!grepl("*DIDACTIQUE:*", first_teacher_disc_df$comment_didactic), "", first_teacher_disc_df$comment_didactic))

# remove other comments about adresse: or bugs from the comment_didactic column
# DIDACTIQUE: comes in the middle in the comments, so there are things to remove before and after…
# .*DIDACTIQUE selects all the text that comes before and up to DIDACTIQUE
first_teacher_disc_df$comment_didactic <-gsub(".*DIDACTIQUE","DIDACTIQUE",as.character(first_teacher_disc_df$comment_didactic))
# BUG ENREGISTREMENT.* selects all the text starting from BUG ENREGISTREMENT and everything that follows
first_teacher_disc_df$comment_didactic <- gsub("BUG ENREGISTREMENT.*","",as.character(first_teacher_disc_df$comment_didactic))

# remove possible \n introduced by a line break when introducing the comment in Trint
first_teacher_disc_df$comment_didactic <- gsub("\n"," ",as.character(first_teacher_disc_df$comment_didactic))

## Comment_bugs column ----

# delete all comments that do not contain BUG in comment_bugs column
first_teacher_disc_df$comment_bugs <- with(first_teacher_disc_df, ifelse(!grepl("*BUG*", first_teacher_disc_df$comment_bugs), "", first_teacher_disc_df$comment_bugs))

# remove other comments about adresse: or didactic from the comment_bugs column
# BUG ENREGISTREMENT always comes last in the comments, so we need to remove all that comes before
# .*BUG selects all the text that comes before and up to BUG
first_teacher_disc_df$comment_bugs <-gsub(".*BUG","BUG",as.character(first_teacher_disc_df$comment_bugs))

# remove possible \n introduced by a line break when introducing the comment in Trint
first_teacher_disc_df$comment_bugs <- gsub("\n"," ",as.character(first_teacher_disc_df$comment_bugs))

# copy ADRESSE: values in recipient column when value is NA, direction down (default)
first_teacher_disc_df <- first_teacher_disc_df %>% fill(recipient)

# replace values into column to remove ADDRESS and to put in English
first_teacher_disc_df$recipient <- with(first_teacher_disc_df, ifelse(grepl("ADRESSE: classe", first_teacher_disc_df$recipient), "class", first_teacher_disc_df$recipient))
first_teacher_disc_df$recipient <- with(first_teacher_disc_df, ifelse(grepl("ADRESSE: élève", first_teacher_disc_df$recipient), "pupil", first_teacher_disc_df$recipient))
first_teacher_disc_df$recipient <- with(first_teacher_disc_df, ifelse(grepl("ADRESSE: enseignant", first_teacher_disc_df$recipient), "teacher", first_teacher_disc_df$recipient))

# make lesson discourse from first_teacher_disc_df ----

# put first_teacher_disc_df into lesson_disc_df
lesson_disc_df <- first_teacher_disc_df

# time-management ----

# remove digits for seconds
lesson_disc_df$timestamp = substr(lesson_disc_df$timestamp, 1, nchar(lesson_disc_df$timestamp)-4)

# store first statement starting time in a variable
lesson_start_time <- period_to_seconds(hms(lesson_disc_df$timestamp[1]))

# remove pause duration if needed (only for statements after the pause)
if (pause_yes) {
  # loop through lesson_disc_df$timestamp and remove pause duration for all values after pause_start_time
  lesson_disc_df <- mutate(lesson_disc_df, timestamp = case_when(
    period_to_seconds(hms(lesson_disc_df$timestamp)) > pause_start_time ~ strftime(as.POSIXct("00:00:00", format="%H:%M:%S") + (seconds_to_period(period_to_seconds(hms(lesson_disc_df$timestamp)) - pause_duration_in_s)), format="%H:%M:%S"), 
    TRUE   ~ timestamp 
  ))
}

# adjust starting time to zero and move all the timing backward accordingly
lesson_disc_df <- mutate(lesson_disc_df, timestamp = case_when(
  TRUE   ~ strftime(as.POSIXct("00:00:00", format="%H:%M:%S") + (seconds_to_period(period_to_seconds(hms(lesson_disc_df$timestamp)) - lesson_start_time)), format="%H:%M:%S")
))

# store last statement starting time in a variable
lesson_end_time <- period_to_seconds(hms(lesson_disc_df$timestamp[nrow(lesson_disc_df)]))

# total duration of the lesson in seconds
lesson_full_duration <- lesson_end_time + lesson_disc_df$statement_duration[nrow(lesson_disc_df)]

## lesson_disc_comments_dida_df ----

# remove rows where comment_didactic is empty
lesson_disc_comments_dida_df <- subset(lesson_disc_df, comment_didactic!="")

# delete unnecessary columns
drops8 <- c("statement_duration", "recipient", "comment_bugs")
lesson_disc_comments_dida_df <- lesson_disc_comments_dida_df[ , !(names(lesson_disc_comments_dida_df) %in% drops8)]

# compute-time-slices ----

# intervals in seconds
time_interval <- 600

# number of intervals
number_of_intervals <- ceiling(lesson_full_duration / time_interval)

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

# for lesson_disc_df 

# compute a new column with timestamp value converted in seconds in lesson_disc_df
lesson_disc_df$timestamp_sec <- period_to_seconds(hms(lesson_disc_df$timestamp))

# create an empty column for time_slice
lesson_disc_df$time_slice = NA_character_

# loop through the dataframe to fill in the time_slice column until number_of_intervals-1
for(i in 1:(number_of_intervals-1)){
  lesson_disc_df <- lesson_disc_df %>%
    mutate(time_slice = case_when(
      timestamp_sec >= time_slices[i, 1] & timestamp_sec < time_slices[i+1, 1] ~ rownames(time_slices)[i], 
      TRUE ~ time_slice
    ))
}

# fill in the time_slice column for the last interval 
lesson_disc_df <- lesson_disc_df %>%
  mutate(time_slice = case_when(
    timestamp_sec >= time_slices[number_of_intervals, 1] ~ rownames(time_slices)[number_of_intervals],
    TRUE ~ time_slice
  ))

# remove timestamp_sec column
lesson_disc_df <- subset(lesson_disc_df, select = -c(timestamp_sec))

# Here we have a complete corpus (in the form of a dataframe) with all the words in their original form

## create a copy just for display because we will remove rownames so that they are not displayed in output
lesson_disc_for_display <- lesson_disc_df

## remove rownames
rownames(lesson_disc_for_display) <- NULL

# apostrophe replacement for quanteda ----

## in the lemmatisation process, ’ character (French apostrophe or typographic apostrophe) is not recognised by quanteda, so we need to replace it with ' (straight apostrophe) for the corpus creation. We keep the original text with the curved apostrophe for the display, which is nicer. 

## replace ’ character with '
lesson_disc_df$statement_text <- gsub("’","'",as.character(lesson_disc_df$statement_text))

# main-corpus-creation ----

## create a corpus object with quanteda from full discourse of the lesson
corpus_lesson_disc <- corpus(lesson_disc_df, text_field = "statement_text")

## for the record, display one full text entry of the object
## as.character(corpus_lesson_disc)[1]

## save summary of the corpus in a table
corpus_lesson_disc.stats <- summary(corpus_lesson_disc, n = 1000000)

## create a tokens object with the full corpus unlemmatised (used just after for statistics, and later for dispersion and word in context)
tok_lesson_disc <- tokens(corpus_lesson_disc, remove_punct = TRUE, split_hyphens = FALSE)
tok_lesson_disc <- tokens_split(tok_lesson_disc,"'") # to split forms like "l'ordinateur" into "l" and "ordinateur"
tok_lesson_disc <- tokens_tolower(tok_lesson_disc)

# compute-general-stats-for-lesson ----

# as the corpus has not been lemmatised and is complete, the stats we get here are for the original text complete without lemmatisation. Punctuation is removed.

## text_length

## compute values for text_length of first_teacher (= discourse & lesson)
first_teacher_disc_to_class_text_length <- ntoken(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "class")) %>% sum()
first_teacher_disc_to_pupil_text_length <- ntoken(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "pupil")) %>% sum()
if (discourse_to_other_teacher) {
  first_teacher_disc_to_teacher_text_length <- ntoken(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "teacher")) %>% sum()
}
first_teacher_disc_text_length <- ntoken(tokens_subset(tok_lesson_disc, teacher_id == first_teacher)) %>% sum()

## vocab_size

## compute values for vocabulary size of first_teacher (= discourse & lesson)
first_teacher_disc_to_class_vocab_size <- length(types(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "class")))
first_teacher_disc_to_pupil_vocab_size <- length(types(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "pupil")))
if (discourse_to_other_teacher) {
  first_teacher_disc_to_teacher_vocab_size <- length(types(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "teacher")))
}
first_teacher_disc_vocab_size <- length(types(tokens_subset(tok_lesson_disc, teacher_id == first_teacher)))

## duration

## compute values for duration of first_teacher (= discourse & lesson)
first_teacher_disc_to_class_duration <- tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "class")$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
first_teacher_disc_to_pupil_duration <- tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "pupil")$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
if (discourse_to_other_teacher) {
  first_teacher_disc_to_teacher_duration <- tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "teacher")$statement_duration %>% sum() %>%
    round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
}
first_teacher_disc_duration <- tokens_subset(tok_lesson_disc, teacher_id == first_teacher)$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }

## number_of_statements

## count number of statements for first_teacher discourse (& lesson)
first_teacher_disc_to_class_number_of_statements <- length(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "class"))
first_teacher_disc_to_pupil_number_of_statements <- length(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "pupil"))
if (discourse_to_other_teacher) {
  first_teacher_disc_to_teacher_number_of_statements <- length(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "teacher"))
}
first_teacher_disc_number_of_statements <- length(tokens_subset(tok_lesson_disc, teacher_id == first_teacher))

## lexical_diversity

## create df with lexical_diversity values by teacher
teacher_recipient_lexical_diversity_df <- tokens_group(tok_lesson_disc, groups = interaction(teacher_id, recipient)) %>% textstat_lexdiv(c("MATTR"), MATTR_window = 50)
teacher_lexical_diversity_df <- tokens_group(tok_lesson_disc, groups = teacher_id) %>% textstat_lexdiv(c("MATTR"), MATTR_window = 50)

## compute values for lexical diversity of first_teacher (= discourse & lesson)
first_teacher_disc_to_class_lexical_diversity <- teacher_recipient_lexical_diversity_df %>% filter(document == paste0(first_teacher, ".", "class")) %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)
first_teacher_disc_to_pupil_lexical_diversity <- teacher_recipient_lexical_diversity_df %>% filter(document == paste0(first_teacher, ".", "pupil")) %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)
if (discourse_to_other_teacher) {
  first_teacher_disc_to_teacher_lexical_diversity <- teacher_recipient_lexical_diversity_df %>% filter(document == paste0(first_teacher, ".", "teacher")) %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)
}
first_teacher_disc_lexical_diversity <- teacher_lexical_diversity_df %>% filter(document == first_teacher) %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)

# first-teacher-discourse-stats-table-creation ----

# create one vector for each column of the df
if (discourse_to_other_teacher) {
  ft_recipients <- c("Whole class", "Individual pupil", "Other teacher", "Full discourse")
  ft_number_of_statements <- c(first_teacher_disc_to_class_number_of_statements, first_teacher_disc_to_pupil_number_of_statements, first_teacher_disc_to_teacher_number_of_statements, first_teacher_disc_number_of_statements)
  ft_duration <- c(first_teacher_disc_to_class_duration, first_teacher_disc_to_pupil_duration, first_teacher_disc_to_teacher_duration, first_teacher_disc_duration)
  ft_text_length <- c(first_teacher_disc_to_class_text_length, first_teacher_disc_to_pupil_text_length, first_teacher_disc_to_teacher_text_length, first_teacher_disc_text_length)
  ft_vocab_size <- c(first_teacher_disc_to_class_vocab_size, first_teacher_disc_to_pupil_vocab_size, first_teacher_disc_to_teacher_vocab_size, first_teacher_disc_vocab_size)
  ft_lexical_diversity <- c(first_teacher_disc_to_class_lexical_diversity, first_teacher_disc_to_pupil_lexical_diversity, first_teacher_disc_to_teacher_lexical_diversity, first_teacher_disc_lexical_diversity)
} else {
  ft_recipients <- c("Whole class", "Individual pupil", "Full discourse")
  ft_number_of_statements <- c(first_teacher_disc_to_class_number_of_statements, first_teacher_disc_to_pupil_number_of_statements, first_teacher_disc_number_of_statements)
  ft_duration <- c(first_teacher_disc_to_class_duration, first_teacher_disc_to_pupil_duration, first_teacher_disc_duration)
  ft_text_length <- c(first_teacher_disc_to_class_text_length, first_teacher_disc_to_pupil_text_length, first_teacher_disc_text_length)
  ft_vocab_size <- c(first_teacher_disc_to_class_vocab_size, first_teacher_disc_to_pupil_vocab_size, first_teacher_disc_vocab_size)
  ft_lexical_diversity <- c(first_teacher_disc_to_class_lexical_diversity, first_teacher_disc_to_pupil_lexical_diversity, first_teacher_disc_lexical_diversity)
}

# create dataframe with those vectors
first_teacher_stats_table <- data.frame(ft_recipients, ft_number_of_statements, ft_duration, ft_text_length, ft_vocab_size, ft_lexical_diversity)

# change column names
colnames(first_teacher_stats_table) <- c("Recipient", "No. of statements","Duration", "Text length (in words)", "Vocabulary size (in words)", "Lexical diversity (MATTR)")

first_teacher_stats_table_caption <- paste("Statistics on classroom discourse for", first_teacher, "and for the lesson_id:", this_lesson_id)

# compute-number-of-tokens-by-statement ----

# test --- as presented in https://api.rpubs.com/cbpuschmann/textmining

# plot the number of tokens per statement, with colors for To
tokens_per_statement_by_recipient <- ggplot(
  corpus_lesson_disc.stats, 
  aes(x = fct_inorder(Text), y = Tokens, group=1, color=recipient)) + 
  # geom_line(linewidth = .3) + 
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  geom_point(size = .5) +
  labs(title = "", x = "Statements", y = "Words") +
  # dark_mode(theme_bw()) + # if we want to use dark mode
  theme_bw() +
  theme(legend.title=element_text(size=13), legend.text=element_text(size=12)) +
  # rotate x labels
  theme(axis.text.x = element_text(angle = 0, vjust = 0.8, hjust=1)) + 
  # limit the number of x labels
  scale_x_discrete(breaks = c(corpus_lesson_disc.stats$Text[1], corpus_lesson_disc.stats$Text[nrow(corpus_lesson_disc.stats)/4], corpus_lesson_disc.stats$Text[nrow(corpus_lesson_disc.stats)/2], corpus_lesson_disc.stats$Text[nrow(corpus_lesson_disc.stats)/4*3], corpus_lesson_disc.stats$Text[nrow(corpus_lesson_disc.stats)]))

# smoothing intents

# same plot than before, but with smoothing function integrated in ggplot with library tidyquant (instead of geom_line())
# https://search.r-project.org/CRAN/refmans/tidyquant/html/geom_ma.html
# library(tidyquant)
# ggplot(corpus_first_teacher.stats, aes(Text, Tokens, group=1, color=To)) + geom_point(size = 1) + ggtitle("Tokens per statement") + geom_ma(ma_fun = DEMA, n = 3, linetype = "solid", size = 2)

# smoothing with rollmean and ksmooth, creating a temporary df
# https://boostedml.com/2020/05/an-introduction-to-time-series-smoothing-in-r.html
# temp <- corpus_first_teacher.stats %>% select(Text, Tokens)
# plot(temp$Tokens)
# Simple Moving Average (SMA)
# lines(rollmean(temp$Tokens,3),col='blue')
# Triangular Moving Average (TMA)
# lines(rollmean(rollmean(temp$Tokens,2),2),col='blue')
# this one doesn’t work because I don’t know how to plot a serie of individual values (it requires two numeric axes)
# lines(ksmooth(temp$Text, temp$Tokens, "normal", bandwidth = 2), col = 2)

# plot the number of types per statement
ggplot(corpus_lesson_disc.stats, aes(Text, Types, group=1)) + geom_line() + geom_point() + ggtitle("Types per statement")

# plot the number of sentences per statement
ggplot(corpus_lesson_disc.stats, aes(Text, Sentences, group=1)) + geom_line() + geom_point() + ggtitle("Sentences per statement")

# plot the three together
ggplot(corpus_lesson_disc.stats %>% gather(Types, Tokens, Sentences, key = "Unit", value = "Number"), aes(Text, Number, group = Unit, col = Unit)) + geom_line(linewidth = 1) + ggtitle("Tokens, Types and Sentences per statement")

# plot the type-token ratio per statement
ggplot(corpus_lesson_disc.stats, aes(Tokens, Types, group=1, label = Text)) + geom_smooth(method = "lm", se = FALSE) + geom_text(check_overlap = T) + ggtitle("Type-Token ratio per statement")

# compute-tokens-in-context-and-dispersion ----

# find a word in context
matching <- kwic(tok_lesson_disc, "program*")
head(matching, 20)

# find two words in context
matching <- kwic(tok_lesson_disc, c("avance*", "tourne*"), window = 10, case_insensitive = FALSE)
head(matching, 30)

# dispersion of a word in the text
textplot_xray(kwic(tok_lesson_disc, "commande*", valuetype = "regex", case_insensitive = FALSE), kwic(tok_lesson_disc, "program*", valuetype = "regex", case_insensitive = FALSE))