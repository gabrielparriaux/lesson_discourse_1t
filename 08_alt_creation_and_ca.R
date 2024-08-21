# aggregated-lexical-table-creation ----

## rename-some-factors ----

# address to
docvars(dfm_without_stopwords_and_short_segments) <- docvars(dfm_without_stopwords_and_short_segments) %>%
  mutate(To = case_when(
    To == "class" ~ "address to class",
    To == "pupil" ~ "address to pupil",
    To == "teacher" ~ "address to teacher"
  )) %>% mutate(To = as.factor(To))

# lesson topic
docvars(dfm_without_stopwords_and_short_segments) <- docvars(dfm_without_stopwords_and_short_segments) %>%
  mutate(lesson_topic = case_when(
    lesson_topic == "programming1" ~ "lesson prog1",
    lesson_topic == "programming2" ~ "lesson prog2"
  )) %>% mutate(lesson_topic = as.factor(lesson_topic))

# programing type
docvars(dfm_without_stopwords_and_short_segments) <- docvars(dfm_without_stopwords_and_short_segments) %>%
  mutate(programming_type = case_when(
    programming_type == "visual" ~ "visual programing",
    programming_type == "textual" ~ "textual programing"
  )) %>% mutate(programming_type = as.factor(programming_type))

## group-documents-by-variables ----

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_speaker <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$Speaker)
# Convert to data.frame
df_speaker <- convert(dfm_speaker, "data.frame")
# Convert column doc_id into rownames
df_speaker <- df_speaker %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_recipients <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$To)
# Convert to data.frame
df_recipients <- convert(dfm_recipients, "data.frame")
# Convert column doc_id into rownames
df_recipients <- df_recipients %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_time_slice <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$time_slice)
# Convert to data.frame
df_time_slice <- convert(dfm_time_slice, "data.frame")
# Convert column doc_id into rownames
df_time_slice <- df_time_slice %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_cluster <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$Cluster)
# Convert to data.frame
df_cluster <- convert(dfm_cluster, "data.frame")
# Convert column doc_id into rownames
df_cluster <- df_cluster %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_lesson_topic <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$lesson_topic)
# Convert to data.frame
df_lesson_topic <- convert(dfm_lesson_topic, "data.frame")
# Convert column doc_id into rownames
df_lesson_topic <- df_lesson_topic %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_programming_type <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$programming_type)
# Convert to data.frame
df_programming_type <- convert(dfm_programming_type, "data.frame")
# Convert column doc_id into rownames
df_programming_type <- df_programming_type %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_gender <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$gender)
# Convert to data.frame
df_gender <- convert(dfm_gender, "data.frame")
# Convert column doc_id into rownames
df_gender <- df_gender %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_age_range <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$age_range)
# Convert to data.frame
df_age_range <- convert(dfm_age_range, "data.frame")
# Convert column doc_id into rownames
df_age_range <- df_age_range %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_professional_role <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$professional_role)
# Convert to data.frame
df_professional_role <- convert(dfm_professional_role, "data.frame")
# Convert column doc_id into rownames
df_professional_role <- df_professional_role %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_discipline <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$discipline)
# Convert to data.frame
df_discipline <- convert(dfm_discipline, "data.frame")
# Convert column doc_id into rownames
df_discipline <- df_discipline %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_teaching_experience <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$teaching_experience)
# Convert to data.frame
df_teaching_experience <- convert(dfm_teaching_experience, "data.frame")
# Convert column doc_id into rownames
df_teaching_experience <- df_teaching_experience %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_cs_teaching_experience <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$cs_teaching_experience)
# Convert to data.frame
df_cs_teaching_experience <- convert(dfm_cs_teaching_experience, "data.frame")
# Convert column doc_id into rownames
df_cs_teaching_experience <- df_cs_teaching_experience %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_teaching_qualification <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$teaching_qualification)
# Convert to data.frame
df_teaching_qualification <- convert(dfm_teaching_qualification, "data.frame")
# Convert column doc_id into rownames
df_teaching_qualification <- df_teaching_qualification %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_degree <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$degree)
# Convert to data.frame
df_degree <- convert(dfm_degree, "data.frame")
# Convert column doc_id into rownames
df_degree <- df_degree %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_cs_education <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$cs_education)
# Convert to data.frame
df_cs_education <- convert(dfm_cs_education, "data.frame")
# Convert column doc_id into rownames
df_cs_education <- df_cs_education %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_cs_education_type <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$cs_education_type)
# Convert to data.frame
df_cs_education_type <- convert(dfm_cs_education_type, "data.frame")
# Convert column doc_id into rownames
df_cs_education_type <- df_cs_education_type %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_Integrated_TPCK_Mastery <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$Integrated_TPCK_Mastery)
# Convert to data.frame
df_Integrated_TPCK_Mastery <- convert(dfm_Integrated_TPCK_Mastery, "data.frame")
# Convert column doc_id into rownames
df_Integrated_TPCK_Mastery <- df_Integrated_TPCK_Mastery %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_Foundational_Knowledge_Base <- dfm_group(dfm_without_stopwords_and_short_segments, docvars(dfm_without_stopwords_and_short_segments)$Foundational_Knowledge_Base)
# Convert to data.frame
df_Foundational_Knowledge_Base <- convert(dfm_Foundational_Knowledge_Base, "data.frame")
# Convert column doc_id into rownames
df_Foundational_Knowledge_Base <- df_Foundational_Knowledge_Base %>% remove_rownames %>% column_to_rownames(var="doc_id")

# group all dfm into one
tableau_lexical_questions <- rbind(df_speaker, df_recipients)
tableau_lexical_questions <- rbind(tableau_lexical_questions, df_time_slice)
tableau_lexical_questions <- rbind(tableau_lexical_questions, df_cluster)

# Order lexicon decreasing to view high and low values
tableau_lexical_questions <- tableau_lexical_questions[,order(colSums(tableau_lexical_questions), decreasing = TRUE)]

## add-clusters-to-alt-and-compute-mca ----

### group small clusters ----

# If I don’t want to keep a fixed number of clusters, but use a threshold value of number of segments in the cluster under which we group them, this is the way to do it

# How to decide the threshold? Clusters including more than 1% of segments of the corpus represent 12 clusters. We decide to regroup clusters under 1% of the corpus.
# cluster_threshold <- sum(cluster_nb_of_segments)/25
# cluster_nb_of_segments_under_threshold <- cluster_nb_of_segments[cluster_nb_of_segments < cluster_threshold]
# get the names of the small clusters to regroup
# clusters_to_regroup <- names(cluster_nb_of_segments_under_threshold)

# And this is the way to go if I decide to regroup all but the biggest clusters (fixed number of 10)

# create a vector with a list of all clusters numbers, depending on the number of clusters defined at the beginning
clusters_numbers_list <- 1:clusters_number

# create a vector for the small clusters, as a diff between the list of all clusters and the biggest clusters
smallest_clusters <- setdiff(clusters_numbers_list, biggest_clusters)

# group small clusters from ALT into a new row with the sum of their values
# Create a df with sum of all small clusters
sum_small_clusters <- tableau_lexical_questions %>% filter(rownames(tableau_lexical_questions) %in% smallest_clusters) %>% summarise(across(everything(), sum))

# rename the unique row of this dataframe
rownames(sum_small_clusters) <- "small_clusters"

# remove the rows corresponding to small clusters in the generalised aggregated lexical table
tableau_lexical_questions <- tableau_lexical_questions[!(row.names(tableau_lexical_questions) %in% smallest_clusters),]

# add the row with the sum of small clusters
tableau_lexical_questions <- rbind(tableau_lexical_questions, sum_small_clusters)

# rename clusters if they have been analysed
if (clusters_renamed_yes) {
  # extract rownames into a column
  tableau_lexical_questions <- rownames_to_column(tableau_lexical_questions)
  # rename it for merge
  colnames(tableau_lexical_questions)[1] <- "Cluster"
  # merge with the biggest clusters df into another df
  tbl_lex_quest_merged <- merge(tableau_lexical_questions, biggest_clusters_df, by = "Cluster")
  # rename the column and drop the other
  tbl_lex_quest_merged$Cluster <- tbl_lex_quest_merged$name
  tbl_lex_quest_merged$name <- NULL
  # remove the biggest clusters from the generalised aggregated lexical table
  tableau_lexical_questions <- tableau_lexical_questions[!tableau_lexical_questions$Cluster %in% biggest_clusters, ]
  # add the renamed clusters
  tableau_lexical_questions <- rbind(tableau_lexical_questions, tbl_lex_quest_merged)
  # transform first column into rownames
  rownames(tableau_lexical_questions) <- tableau_lexical_questions[ , 1]
  tableau_lexical_questions[ , 1] <- NULL
} else {
  # extract rows corresponding to the biggest clusters
  tbl_lex_quest_extract <- tableau_lexical_questions %>% filter(rownames(tableau_lexical_questions) %in% biggest_clusters)
  # rename clusters with cluster_
  rownames(tbl_lex_quest_extract) <- paste0("cluster_", rownames(tbl_lex_quest_extract))
  # remove the biggest clusters from the generalised aggregated lexical table
  tableau_lexical_questions <- tableau_lexical_questions[!rownames(tableau_lexical_questions) %in% biggest_clusters, ]
  # add the renamed clusters
  tableau_lexical_questions <- rbind(tableau_lexical_questions, tbl_lex_quest_extract)
}

# Remove columns with values under… (5?)
# tableau_lexical_questions_extract <- tableau_lexical_questions %>%
#   select_if(~ !any(.x < 5))

# Limit lexicon to a fixed number of words… (70?)
# tableau_lexical_questions_extract <- tableau_lexical_questions[,1:70]

# transpose the aggregated lexical table
tableau_lexical_questions <- as.data.frame(t(tableau_lexical_questions))

# Correspondence Analysis (CA) ----

ca_two_teachers <- CA(tableau_lexical_questions, graph = FALSE)
# explor(ca_two_teachers)

# compute-plot-eigenvalues ----

# Eigenvalues
ca_alt_screeplot <- fviz_screeplot(ca_two_teachers, addlabels = TRUE, ncp = 20, title = "", ylim = c(0, 50), theme = theme_minimal())

# compute-plot-ca ----

set.seed(42)

plot_ca_two_teachers <- fviz_ca_biplot(
  ca_two_teachers,
  axes = c(1, 2),
  geom = c("point", "text"),
  select.row = list(cos2 = 50),
  # select.col = list(cos2 = 50),
  labelsize = 6,
  label = "all",
  col.col = "#CC79A7",
  col.row = "#0072B2",
  shape.row = 0,
  shape.col = 4,
  pointsize = 3,
  repel = TRUE,
  title = ""
) + theme(text = element_text(size = 7.5),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          plot.margin = margin(0, 10, 10, 10, unit = "pt"))

# ggsave(filename="mca.pdf", path = "output", scale = 1.4)

# contrib-cos2-coord-create-tables ----

## coord ----

# create a dataframe with coordinates for all dimensions of CA
# variables
ca_two_teachers_var_coord <- as.data.frame(ca_two_teachers[["col"]]$coord)
# lexicon
ca_two_teachers_lexicon_coord <- as.data.frame(ca_two_teachers[["row"]]$coord)
# rbind variables and lexicon
ca_two_teachers_coord <- rbind(ca_two_teachers_var_coord, ca_two_teachers_lexicon_coord)

## contrib ----

# create a dataframe with contrib for all dimensions of CA
# variables
ca_two_teachers_var_contrib <- as.data.frame(ca_two_teachers[["col"]]$contrib)
# lexicon
ca_two_teachers_lexicon_contrib <- as.data.frame(ca_two_teachers[["row"]]$contrib)
# rbind variables and lexicon
ca_two_teachers_contrib <- rbind(ca_two_teachers_var_contrib, ca_two_teachers_lexicon_contrib)

## cos2 ----

# create a dataframe with cos2 for all dimensions of CA
# variables
ca_two_teachers_var_cos2 <- as.data.frame(ca_two_teachers[["col"]]$cos2)
# lexicon
ca_two_teachers_lexicon_cos2 <- as.data.frame(ca_two_teachers[["row"]]$cos2)
# rbind variables and lexicon
ca_two_teachers_cos2 <- rbind(ca_two_teachers_var_cos2, ca_two_teachers_lexicon_cos2)

# round values of the three tables to 3 digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
ca_two_teachers_coord <- round_df(ca_two_teachers_coord, 3)
ca_two_teachers_contrib <- round_df(ca_two_teachers_contrib, 3)
ca_two_teachers_cos2 <- round_df(ca_two_teachers_cos2, 3)

## Isolate dimension 1 ----

# create a dataframe with coord, contrib and cos2 values for dimension 1
table_ca_two_teachers_dim1 <- data.frame(ca_two_teachers_coord[,1], ca_two_teachers_contrib[,1], ca_two_teachers_cos2[,1])

# rename rows and columns
rownames(table_ca_two_teachers_dim1) <- rownames(ca_two_teachers_coord)
colnames(table_ca_two_teachers_dim1) <- c('coord', 'contrib', 'cos2')

# order columns in decreasing order according to cos2
table_ca_two_teachers_dim1 <- table_ca_two_teachers_dim1 %>% arrange(desc(table_ca_two_teachers_dim1$cos2))

# keep 30 first rows
table_ca_two_teachers_dim1_extract <- table_ca_two_teachers_dim1[1:30, ]

## Isolate dimension 2 ----

# create a dataframe with coord, contrib and cos2 values for dimension 2
table_ca_two_teachers_dim2 <- data.frame(ca_two_teachers_coord[,2], ca_two_teachers_contrib[,2], ca_two_teachers_cos2[,2])

# rename rows and columns
rownames(table_ca_two_teachers_dim2) <- rownames(ca_two_teachers_coord)
colnames(table_ca_two_teachers_dim2) <- c('coord', 'contrib', 'cos2')

# order columns in decreasing order according to cos2
table_ca_two_teachers_dim2 <- table_ca_two_teachers_dim2 %>% arrange(desc(table_ca_two_teachers_dim2$cos2))

# keep 30 first rows
table_ca_two_teachers_dim2_extract <- table_ca_two_teachers_dim2[1:30, ]
