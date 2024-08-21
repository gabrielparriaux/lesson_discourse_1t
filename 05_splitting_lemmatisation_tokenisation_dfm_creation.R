# splitting-lemmatisation-tokenisation-data-feature-matrix-creation ----

# Splitting in segments (with Rainette)
corpus_merged_teachers_segmented <- split_segments(corpus_merged_teachers, segment_size = 40)

## Process to create a lemmatized corpus ----

# instructions to create a python virtual environment for spacy
# not needed to run once the environment is created --- keeping it here for archive if needed
# detailed infos in: /Users/p2955/Library/CloudStorage/OneDrive-HEPVaud/HEP/recherche/statistiques/classification Reinert/Instructions to create an environment for spacy-updated.md

# choose the version of python you want
# version <- "3.11.7"

# do a python install with this version
# the function returns the path to the Python executable file
# python_exe <- reticulate::install_python(version)

# create a virtual environment with this python install
# reticulate::virtualenv_create("venvforspacy", python = python_exe)

# install spaCy in it
# unfortunately, using this function spacy_install(), I wasn’t able to specify the environment in which I wanted to install spaCy… so the function didn’t install spaCy in the environment I created just before, it created a new environment (!) with the default name "r-spacyr" and installed spaCy into it… good things: this environment took the python version I decided for the first environment (here 3.11.7); spaCy could be installed with the option [apple] that makes it use the capacities of the GPU of the Apple Silicon chip through Metal Performance Shaders; the language model is the one I wanted.
# spacy_install(version = "apple", lang_models = "fr_dep_news_trf", ask = interactive())

# Lancement de la lemmatisation avec le bon modèle 

# original option with a conda environment… deprecated
# Sys.setenv(SPACY_PYTHON = "/Users/p2955/miniconda3")
# spacy_initialize(model = "fr_dep_news_trf", condaenv = "condaenvforspacy")

# new option with a python virtual environment venv, working
spacy_initialize(model = "fr_dep_news_trf", virtualenv = "r-spacyr")

# Création d’une data.table avec le résultat de la lemmatisation et division en mots du corpus
data_table_lemmatisee <- spacy_parse(corpus_merged_teachers_segmented, pos = TRUE, tag = FALSE, lemma = TRUE, entity = TRUE, dependency = TRUE, nounphrase = FALSE, multithread = TRUE)

# Terminer l’environnement spaCy
spacy_finalize()

## tokenisation ----

tok_lemmatized_full <- as.tokens(
  data_table_lemmatisee,
  concatenator = "/",
  include_pos = c("none"),
  use_lemma = TRUE
)

# for the record, display the list of tokens for one document in the tokens object
# as.list(tok_lemmatized)[1]

# splitter aux apostrophes, aux parenthèses, aux slash et aux tirets
# tok_lemmatized_full <- tokens_split(tok_lemmatized_full,"'")
# tok_lemmatized_full <- tokens_split(tok_lemmatized_full,"(")
# tok_lemmatized_full <- tokens_split(tok_lemmatized_full,"/")
# tok_lemmatized_full <- tokens_split(tok_lemmatized_full,"-")
# tok_lemmatized_full <- tokens_split(tok_lemmatized_full,"’")

# tokenize and remove punctuation, symbols, numbers and urls
tok_lemmatized <- tokens(tok_lemmatized_full, remove_punct = TRUE, remove_numbers = TRUE)

# pass the docvars from the corpus
docvars(tok_lemmatized) <- docvars(corpus_merged_teachers_segmented)

# add other global variables as docvars
docvars(tok_lemmatized, "lesson_id") <- lesson_id
docvars(tok_lemmatized, "lesson_topic") <- lesson_topic
docvars(tok_lemmatized, "programming_type") <- programming_type
docvars(tok_lemmatized, "class_id") <- class_id
docvars(tok_lemmatized, "number_of_teachers") <- number_of_teachers
docvars(tok_lemmatized, "first_teacher_number_of_attempts_for_this_lesson") <- first_teacher_number_of_attempts_for_this_lesson
docvars(tok_lemmatized, "second_teacher_number_of_attempts_for_this_lesson") <- second_teacher_number_of_attempts_for_this_lesson

## add variables from teachers survey as docvars ----

# gender
docvars(tok_lemmatized, "gender") <- ifelse(tok_lemmatized$Speaker == colnames(first_teacher_variables_from_survey), first_teacher_variables_from_survey["gender", ], NA)
docvars(tok_lemmatized, "gender") <- ifelse(tok_lemmatized$Speaker == colnames(second_teacher_variables_from_survey), second_teacher_variables_from_survey["gender", ], tok_lemmatized$gender)

# age_range
docvars(tok_lemmatized, "age_range") <- ifelse(tok_lemmatized$Speaker == colnames(first_teacher_variables_from_survey), first_teacher_variables_from_survey["age_range", ], NA)
docvars(tok_lemmatized, "age_range") <- ifelse(tok_lemmatized$Speaker == colnames(second_teacher_variables_from_survey), second_teacher_variables_from_survey["age_range", ], tok_lemmatized$age_range)

# professional_role
docvars(tok_lemmatized, "professional_role") <- ifelse(tok_lemmatized$Speaker == colnames(first_teacher_variables_from_survey), first_teacher_variables_from_survey["professional_role", ], NA)
docvars(tok_lemmatized, "professional_role") <- ifelse(tok_lemmatized$Speaker == colnames(second_teacher_variables_from_survey), second_teacher_variables_from_survey["professional_role", ], tok_lemmatized$professional_role)

# discipline
docvars(tok_lemmatized, "discipline") <- ifelse(tok_lemmatized$Speaker == colnames(first_teacher_variables_from_survey), first_teacher_variables_from_survey["discipline", ], NA)
docvars(tok_lemmatized, "discipline") <- ifelse(tok_lemmatized$Speaker == colnames(second_teacher_variables_from_survey), second_teacher_variables_from_survey["discipline", ], tok_lemmatized$discipline)

# teaching_experience
docvars(tok_lemmatized, "teaching_experience") <- ifelse(tok_lemmatized$Speaker == colnames(first_teacher_variables_from_survey), first_teacher_variables_from_survey["teaching_experience", ], NA)
docvars(tok_lemmatized, "teaching_experience") <- ifelse(tok_lemmatized$Speaker == colnames(second_teacher_variables_from_survey), second_teacher_variables_from_survey["teaching_experience", ], tok_lemmatized$teaching_experience)

# cs_teaching_experience
docvars(tok_lemmatized, "cs_teaching_experience") <- ifelse(tok_lemmatized$Speaker == colnames(first_teacher_variables_from_survey), first_teacher_variables_from_survey["cs_teaching_experience", ], NA)
docvars(tok_lemmatized, "cs_teaching_experience") <- ifelse(tok_lemmatized$Speaker == colnames(second_teacher_variables_from_survey), second_teacher_variables_from_survey["cs_teaching_experience", ], tok_lemmatized$cs_teaching_experience)

# teaching_qualification
docvars(tok_lemmatized, "teaching_qualification") <- ifelse(tok_lemmatized$Speaker == colnames(first_teacher_variables_from_survey), first_teacher_variables_from_survey["teaching_qualification", ], NA)
docvars(tok_lemmatized, "teaching_qualification") <- ifelse(tok_lemmatized$Speaker == colnames(second_teacher_variables_from_survey), second_teacher_variables_from_survey["teaching_qualification", ], tok_lemmatized$teaching_qualification)

# degree
docvars(tok_lemmatized, "degree") <- ifelse(tok_lemmatized$Speaker == colnames(first_teacher_variables_from_survey), first_teacher_variables_from_survey["degree", ], NA)
docvars(tok_lemmatized, "degree") <- ifelse(tok_lemmatized$Speaker == colnames(second_teacher_variables_from_survey), second_teacher_variables_from_survey["degree", ], tok_lemmatized$degree)

# cs_education
docvars(tok_lemmatized, "cs_education") <- ifelse(tok_lemmatized$Speaker == colnames(first_teacher_variables_from_survey), first_teacher_variables_from_survey["cs_education", ], NA)
docvars(tok_lemmatized, "cs_education") <- ifelse(tok_lemmatized$Speaker == colnames(second_teacher_variables_from_survey), second_teacher_variables_from_survey["cs_education", ], tok_lemmatized$cs_education)

# cs_education_type
docvars(tok_lemmatized, "cs_education_type") <- ifelse(tok_lemmatized$Speaker == colnames(first_teacher_variables_from_survey), first_teacher_variables_from_survey["cs_education_type", ], NA)
docvars(tok_lemmatized, "cs_education_type") <- ifelse(tok_lemmatized$Speaker == colnames(second_teacher_variables_from_survey), second_teacher_variables_from_survey["cs_education_type", ], tok_lemmatized$cs_education_type)

# Integrated_TPCK_Mastery
docvars(tok_lemmatized, "Integrated_TPCK_Mastery") <- ifelse(tok_lemmatized$Speaker == colnames(first_teacher_variables_from_survey), first_teacher_variables_from_survey["Integrated_TPCK_Mastery", ], NA)
docvars(tok_lemmatized, "Integrated_TPCK_Mastery") <- ifelse(tok_lemmatized$Speaker == colnames(second_teacher_variables_from_survey), second_teacher_variables_from_survey["Integrated_TPCK_Mastery", ], tok_lemmatized$Integrated_TPCK_Mastery)

# Foundational_Knowledge_Base
docvars(tok_lemmatized, "Foundational_Knowledge_Base") <- ifelse(tok_lemmatized$Speaker == colnames(first_teacher_variables_from_survey), first_teacher_variables_from_survey["Foundational_Knowledge_Base", ], NA)
docvars(tok_lemmatized, "Foundational_Knowledge_Base") <- ifelse(tok_lemmatized$Speaker == colnames(second_teacher_variables_from_survey), second_teacher_variables_from_survey["Foundational_Knowledge_Base", ], tok_lemmatized$Foundational_Knowledge_Base)

# Save tok_lemmatized to a file for export to global analysis
saveRDS(tok_lemmatized, file = paste0(data_dir, "/export/", class_id, "_", lesson_topic, "_tokens.rds"))

# copy docvars from tokens to corpus
docvars(corpus_merged_teachers_segmented) <- docvars(tok_lemmatized)

# Save corpus_merged_teachers_segmented to a file for export to global analysis
saveRDS(corpus_merged_teachers_segmented, file = paste0(data_dir, "/export/", class_id, "_", lesson_topic, "_corpus.rds"))

# create data-feature matrix
dfm_complete_lemmatized <- dfm(tok_lemmatized)

# for the record, display the full list of features in the dfm
# dfm_complete_lemmatized@Dimnames[["features"]]