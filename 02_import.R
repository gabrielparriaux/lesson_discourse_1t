# import ----

## import transcript and comments ----
first_teacher_transcript <- read.csv(paste0(data_dir, "/import/", first_teacher, "_transcript.csv"))
first_teacher_comments <- read.csv(paste0(data_dir, "/import/", first_teacher, "_comments.csv"))

# check errors ----

## check errors first_teacher ----
check1 <- subset(first_teacher_transcript, xor(Speaker == first_teacher_name, Status == "verified"))