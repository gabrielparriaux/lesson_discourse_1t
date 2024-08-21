# compute-clustering ----

# copy dfm into a new one
dfm_without_stopwords_and_short_segments <- dfm_without_stopwords

# while loop to remove the segments with less than 3 tokens and the words present in less than 3 documents (we loop as long as there are…)
# !all(ntoken(dfm_without_stopwords_and_short_segments) >= 3) returns TRUE as long as there are segments with less than 3 tokens
# !all(docfreq(dfm_without_stopwords_and_short_segments) >= 3) returns TRUE as long as there are words present in less than 3 documents
while (!all(ntoken(dfm_without_stopwords_and_short_segments) >= 3) | !all(docfreq(dfm_without_stopwords_and_short_segments) >= 3)) {
  # remove the same documents in the corpus to keep the corpus same size than the dfm
  # find the documents that are shorter than 3 in the dfm and isolate them
  segments_longer_than_threshold <- dfm_subset(dfm_without_stopwords_and_short_segments, ntoken(dfm_without_stopwords_and_short_segments) >= 3)
  # get their names
  segments_longer_than_threshold_names <- docnames(segments_longer_than_threshold)
  # subset the corpus with the names of the documents that are longer than 3
  corpus_merged_teachers_segmented <- subset(corpus_merged_teachers_segmented, docnames(corpus_merged_teachers_segmented) %in% segments_longer_than_threshold_names)
  # remove documents in dfm with less than 3 tokens
  dfm_without_stopwords_and_short_segments <- dfm_subset(dfm_without_stopwords_and_short_segments, ntoken(dfm_without_stopwords_and_short_segments) >= 3)  
  # remove words present in less than 3 documents
  dfm_without_stopwords_and_short_segments <- dfm_trim(dfm_without_stopwords_and_short_segments, min_docfreq = 3)
}

# for the record, list of words appearing less often in the dfm
topfeatures(dfm_without_stopwords_and_short_segments, decreasing = FALSE)

# for the record, list of document containing less words
# Calculate total words in each document
doc_word_counts <- rowSums(dfm_without_stopwords_and_short_segments)

# Find the document(s) with the fewest words
fewest_words_docs <- names(which.min(doc_word_counts))

# Print the document name(s) and word count(s)
for (doc in fewest_words_docs) {
  cat("Document:", doc, "- Word count:", doc_word_counts[doc], "\n")
}

# Reinert clustering
res <- rainette(dfm_without_stopwords_and_short_segments, k = clusters_number, min_segment_size = 15, min_split_members = 8)

# explor
# rainette_explor(res, dfm_without_stopwords_and_short_segments, corpus_merged_teachers_segmented)

# create a vector with the cluster to which each segment has been classified
groups <- cutree_rainette(res, k = clusters_number, criterion = "chi2")

# add groups as a docvar
docvars(dfm_without_stopwords_and_short_segments)$Cluster <- groups

# Reinert plot dendrogram ----

## Clustering description plot
# rainette_dendrogram <- rainette_plot(
#   res, dfm_without_stopwords_and_short_segments, k = clusters_number,
#   n_terms = 20,
#   free_scales = FALSE,
#   measure = "chi2",
#   show_negative = TRUE,
#   text_size = 8
# )

# because of the fact that the plot is not displayed correctly in the Quarto file if we just use the object created by the plot, we embed the plot in a function that returns the object, and we call the function in the Quarto file. This way, it works correctly (advice from juba)
plot_dendrogram <- function() {
  rainette_dendrogram <- rainette_plot(
    res, dfm_without_stopwords_and_short_segments, k = clusters_number,
    n_terms = 20,
    free_scales = FALSE,
    measure = "chi2",
    show_negative = TRUE,
    text_size = 8
  )
  return(rainette_dendrogram)
}

# identify-biggest-clusters ----

# get cluster number of segments in a table
cluster_nb_of_segments <- table(groups)

# order table of clusters by highest number of segments
cluster_nb_of_segments <- cluster_nb_of_segments[order(cluster_nb_of_segments,decreasing = TRUE)]

# decide number of biggest clusters to keep for interpretation
cluster_nbr_for_interpretation <- 10

# isolate the names of clusters for interpretation
biggest_clusters <- names(cluster_nb_of_segments[1:cluster_nbr_for_interpretation])

# create an object (list of list) with tibbles of overrepresented terms in every cluster
tokens_in_clusters <- rainette_stats(groups, dfm_without_stopwords_and_short_segments, n_terms = 20, show_negative = TRUE)

first_cluster_caption <- paste("Over- and underrepresented tokens in the cluster", biggest_clusters[1], "(ordered by chi2 value)")
second_cluster_caption <- paste("Overrepresented tokens in the cluster", biggest_clusters[2], "(ordered by chi2 value)")
third_cluster_caption <- paste("Overrepresented tokens in the cluster", biggest_clusters[3], "(ordered by chi2 value)")
fourth_cluster_caption <- paste("Overrepresented tokens in the cluster", biggest_clusters[4], "(ordered by chi2 value)")
fifth_cluster_caption <- paste("Overrepresented tokens in the cluster", biggest_clusters[5], "(ordered by chi2 value)")
sixth_cluster_caption <- paste("Overrepresented tokens in the cluster", biggest_clusters[6], "(ordered by chi2 value)")
seventh_cluster_caption <- paste("Overrepresented tokens in the cluster", biggest_clusters[7], "(ordered by chi2 value)")
eighth_cluster_caption <- paste("Overrepresented tokens in the cluster", biggest_clusters[8], "(ordered by chi2 value)")
nineth_cluster_caption <- paste("Overrepresented tokens in the cluster", biggest_clusters[9], "(ordered by chi2 value)")
tenth_cluster_caption <- paste("Overrepresented tokens in the cluster", biggest_clusters[10], "(ordered by chi2 value)")

# compute-clusters-size-in-segments ----

# Summarize counts for each modality in the current column
modality_counts <- table(docvars(dfm_without_stopwords_and_short_segments)$Cluster)

# Convert the summary table to a data frame
counts_df <- as.data.frame(modality_counts)
colnames(counts_df) <- c("Modality", "Sample_Size")

# Reorder based on cluster names
counts_df <- counts_df %>% arrange(Modality)
# Reorder based on number of segments
# counts_df <- counts_df %>% arrange(desc(Sample_Size))

counts_df$Modality <- paste0("cluster_", counts_df$Modality)

# Create the bar plot using ggplot2
clusters_size_in_segments <- ggplot(counts_df, aes(x=factor(Modality, level=Modality), y = Sample_Size)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Sample_Size), vjust = -0.5, color = "black", size = 3) +  # Add labels inside bars
  labs(title = "",
       x = "", y = "Number of segments") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Rotate x-axis labels

# remove-NA-values-in-Cluster-docvar ----

# copy the dfm to a new one to keep the first untouched
dfm_without_stopwords_and_short_segments_for_slices <- dfm_without_stopwords_and_short_segments

# if there are NA values in the Cluster docvar, we remove the rows with NA values in the dfm and the corresponding documents in the corpus (as it generates an error with textstat_keyness())
if (anyNA(dfm_without_stopwords_and_short_segments_for_slices$Cluster)) {
  
  # Find the rows in docvars(dtm_for_analysis) that have a NA value for Cluster
  na_rows <- which(is.na(docvars(dfm_without_stopwords_and_short_segments_for_slices)$Cluster))
  
  # loop to remove all rows with NA value for Cluster in the corpus
  for (i in na_rows) {
    # find the name of the document with NA value for Cluster (example)
    segments_with_NA_cluster_name <- docnames(dfm_without_stopwords_and_short_segments_for_slices)[i]
    # subset the corpus with the names of the documents that are longer than 3
    corpus_merged_teachers_segmented_for_slices <- subset(corpus_merged_teachers_segmented, docnames(corpus_merged_teachers_segmented) != segments_with_NA_cluster_name)
  }
  
  # remove the rows with NA value for Cluster in the dfm
  dfm_without_stopwords_and_short_segments_for_slices <- dfm_without_stopwords_and_short_segments_for_slices[-na_rows, ]
  # remove rows where docvar(Cluster) is NA in groups
  groups_for_slices <- groups[-na_rows]
}

# compute-evolution-of-clusters-during-lesson ----

# create a df with time_slice and Cluster columns
df_time_slice_cluster <- docvars(dfm_without_stopwords_and_short_segments_for_slices) %>% select(time_slice, Cluster)
rownames(df_time_slice_cluster) <- docnames(dfm_without_stopwords_and_short_segments_for_slices)

# keep only segments belonging to biggest clusters
df_time_slice_cluster <- df_time_slice_cluster %>% filter(Cluster %in% biggest_clusters)

# rename clusters
if (clusters_renamed_yes) {
  biggest_clusters_df <- data.frame(biggest_clusters, biggest_clusters_names)
  colnames(biggest_clusters_df) <- c("Cluster", "name")
  df_time_slice_merged <- merge(df_time_slice_cluster, biggest_clusters_df, by = "Cluster")
  df_time_slice_merged$Cluster <- df_time_slice_merged$name
  df_time_slice_merged$name <- NULL
  df_time_slice_cluster <- df_time_slice_merged
} else {
  df_time_slice_cluster$Cluster <- paste0("cluster_", df_time_slice_cluster$Cluster)
}

# count the number of segments in each cluster for each time slice
df_time_slice_cluster_grouped <- df_time_slice_cluster %>% count(time_slice, Cluster)

# convert count to percentage by time_slice
df_time_slice_cluster_grouped <- df_time_slice_cluster_grouped %>%
  group_by(time_slice) %>%
  mutate(sum_time_slice = sum(n)) %>% 
  group_by(time_slice, Cluster) %>% 
  mutate(freq = n/sum_time_slice,
         freq_perc = round(freq*100 %>% round(2)))

## plot-evolution-of-clusters-during-lesson ----

plot_evolution_of_clusters <- ggplot(df_time_slice_cluster_grouped, aes(fill=Cluster, y=n, x=time_slice)) + 
  geom_bar(position="fill", stat="identity", width = .98) # + # decide if I want the labels or not
# geom_text(aes(label= paste0(freq_perc, "%")), position=position_fill(vjust=0.5), size=3, colour="black")