# first-teacher-columns-management ----

# delete unnecessary columns
drops <- c("Out","Duration.y", "Commented_Text", "Thread_Resolved", "Comment_2")
first_teacher_reduced <- first_teacher_reduced[ , !(names(first_teacher_reduced) %in% drops)]

# rename columns after merge
names(first_teacher_reduced)[names(first_teacher_reduced) == 'Duration.x'] <- 'Duration'
names(first_teacher_reduced)[names(first_teacher_reduced) == 'Comment_1'] <- 'To'

# remove unnecessary email in To column
first_teacher_reduced$To<-gsub("name@anonymous.org: ","",as.character(first_teacher_reduced$To))

# triplicate column to separate comments
first_teacher_reduced$Comment_didactic <- first_teacher_reduced$To
first_teacher_reduced$Comment_bugs <- first_teacher_reduced$To

## To column ----

# delete all comments that do not contain adresse: in To column and replace them with NA values
first_teacher_reduced$To <- with(first_teacher_reduced, ifelse(!grepl("*ADRESSE:*", first_teacher_reduced$To), NA, first_teacher_reduced$To))

# remove other comments about didactic or bugs from the To column
# ADRESSE: comes always first in the comments, so we need to remove what comes after
# BUG ENREGISTREMENT.* selects all the text starting from BUG ENREGISTREMENT and everything that follows
first_teacher_reduced$To <- gsub("BUG ENREGISTREMENT.*","",as.character(first_teacher_reduced$To))
first_teacher_reduced$To <- gsub("DIDACTIQUE.*","",as.character(first_teacher_reduced$To))

# remove possible \n introduced by a line break when introducing the comment in Trint
first_teacher_reduced$To <- gsub("\n"," ",as.character(first_teacher_reduced$To))

## Comment_didactic column ----

# delete all comments that do not contain DIDACTIQUE: in Comment_didactic column
first_teacher_reduced$Comment_didactic <- with(first_teacher_reduced, ifelse(!grepl("*DIDACTIQUE:*", first_teacher_reduced$Comment_didactic), "", first_teacher_reduced$Comment_didactic))

# remove other comments about adresse: or bugs from the Comment_didactic column
# DIDACTIQUE: comes in the middle in the comments, so there are things to remove before and after…
# .*DIDACTIQUE selects all the text that comes before and up to DIDACTIQUE
first_teacher_reduced$Comment_didactic <-gsub(".*DIDACTIQUE","DIDACTIQUE",as.character(first_teacher_reduced$Comment_didactic))
# BUG ENREGISTREMENT.* selects all the text starting from BUG ENREGISTREMENT and everything that follows
first_teacher_reduced$Comment_didactic <- gsub("BUG ENREGISTREMENT.*","",as.character(first_teacher_reduced$Comment_didactic))

# remove possible \n introduced by a line break when introducing the comment in Trint
first_teacher_reduced$Comment_didactic <- gsub("\n"," ",as.character(first_teacher_reduced$Comment_didactic))

## Comment_bugs column ----

# delete all comments that do not contain BUG in Comment_bugs column
first_teacher_reduced$Comment_bugs <- with(first_teacher_reduced, ifelse(!grepl("*BUG*", first_teacher_reduced$Comment_bugs), "", first_teacher_reduced$Comment_bugs))

# remove other comments about adresse: or didactic from the Comment_bugs column
# BUG ENREGISTREMENT always comes last in the comments, so we need to remove all that comes before
# .*BUG selects all the text that comes before and up to BUG
first_teacher_reduced$Comment_bugs <-gsub(".*BUG","BUG",as.character(first_teacher_reduced$Comment_bugs))

# remove possible \n introduced by a line break when introducing the comment in Trint
first_teacher_reduced$Comment_bugs <- gsub("\n"," ",as.character(first_teacher_reduced$Comment_bugs))

# copy ADRESSE: values in To column when value is NA, direction down (default)
first_teacher_reduced <- first_teacher_reduced %>% fill(To)

# replace values into column to remove ADDRESS and to put in English
first_teacher_reduced$To <- with(first_teacher_reduced, ifelse(grepl("ADRESSE: classe", first_teacher_reduced$To), "class", first_teacher_reduced$To))
first_teacher_reduced$To <- with(first_teacher_reduced, ifelse(grepl("ADRESSE: élève", first_teacher_reduced$To), "pupil", first_teacher_reduced$To))
first_teacher_reduced$To <- with(first_teacher_reduced, ifelse(grepl("ADRESSE: enseignant", first_teacher_reduced$To), "teacher", first_teacher_reduced$To))

# first-teacher-discourse-tables ----

## first_teacher_to_class ----

# remove rows that are do not address whole class
first_teacher_to_class <- subset(first_teacher_reduced, To=="class")

# delete unnecessary columns
drops3 <- c("Speaker", "To", "Comment_didactic", "Comment_bugs")
first_teacher_to_class <- first_teacher_to_class[ , !(names(first_teacher_to_class) %in% drops3)]

## first_teacher_to_pupil ----

# remove rows that are do not address pupils
first_teacher_to_pupil <- subset(first_teacher_reduced, To=="pupil")

# delete unnecessary columns
first_teacher_to_pupil <- first_teacher_to_pupil[ , !(names(first_teacher_to_pupil) %in% drops3)]

## first_teacher_to_teacher ----

# remove rows that are do not address teacher
first_teacher_to_teacher <- subset(first_teacher_reduced, To=="teacher")

# delete unnecessary columns
first_teacher_to_teacher <- first_teacher_to_teacher[ , !(names(first_teacher_to_teacher) %in% drops3)]

## first_teacher_complete ----

# simple copy of first_teacher_reduced
first_teacher_complete <- first_teacher_reduced

# delete unnecessary columns
drops4 <- c("Speaker", "Comment_didactic", "Comment_bugs")
first_teacher_complete <- first_teacher_complete[ , !(names(first_teacher_complete) %in% drops4)]

## first_teacher_comments_dida ----

# remove rows where Comment_didactic is empty
first_teacher_comments_dida <- subset(first_teacher_reduced, Comment_didactic!="")

# delete unnecessary columns
drops5a <- c("Duration", "Speaker", "To", "Comment_bugs")
first_teacher_comments_dida <- first_teacher_comments_dida[ , !(names(first_teacher_comments_dida) %in% drops5a)]

## first_teacher_comments_bugs ----

# remove rows where Comment_bugs is empty
first_teacher_comments_bugs <- subset(first_teacher_reduced, Comment_bugs!="")

# delete unnecessary columns
drops5b <- c("Duration", "Speaker", "To", "Comment_didactic")
first_teacher_comments_bugs <- first_teacher_comments_bugs[ , !(names(first_teacher_comments_bugs) %in% drops5b)]