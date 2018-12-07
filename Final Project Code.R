library(data.table)
library(ggplot2)
library(RedditExtractoR)
library(lubridate)

#set working directory
setwd('D:/DLIANG/School/Undergraduate/Semester 3/STAT 184 (R Resources)/Final Project')

#SCRAPE data for r/leagueoflegends posts
posts <-  reddit_urls(search_terms = 'leagueoflegends', 
                      cn_threshold = 50, page_threshold = 400,
                      sort_by = 'comments', wait_time = 0.5)

#loop to scrape data for post comments
data <- NULL
for (i in 1:20){
  # go to each post's page
  goto<-paste(posts[i,'URL'])
  # get the data on the page
  table<-reddit_content(goto, wait_time=0.5)
  # remove columns
  table$domain <- NULL
  table$link <- NULL
  table$subreddit <- NULL
  table$structure <- NULL
  table$num_comments <- NULL
  table$URL <- NULL
  table$id <- NULL
  #add table to overall DT
  data<-rbind(data,table)
}

#MERGE comment data w/ post data
DT <- merge(posts, data, by='title',all=TRUE)
#clean final DT
DT$date <- NULL
DT$subreddit <- NULL
DT <- na.omit(DT)

#make sure variables are in date-time format
DT$post_date <- dmy(DT$post_date)
DT$comm_date <- dmy(DT$comm_date)
#Variable 1: create date-time difference variable
DT$com_postAge <- difftime(DT$comm_date, DT$post_date, unit = 'days')
DT$com_postAge <- as.integer(DT$com_postAge)

#Variable 2: create title length variable
DT$title_length <- nchar(DT$title)

#Variable 3: create mean comment score variable
meancommentscore <- dcast(DT, title~., mean, value.var = 'comment_score')

#create DT for analysis
postscore <- dcast(DT, title~., mean, value.var = 'post_score')
comments <- dcast(DT, title~., mean, value.var='num_comments')
lengthtitle <- dcast(DT, title~., mean, value.var = 'title_length')
setnames(meancommentscore, '.', 'mean_commentscore')
setnames(postscore, '.', 'post_score')
setnames(comments, '.', 'num_comments')
setnames(lengthtitle, '.', 'title_length')
aggDT <- merge(meancommentscore, postscore, by = 'title', all.x = T)
aggDT <- merge(aggDT, comments, by = 'title', all.x = T)
aggDT <- merge(aggDT, lengthtitle, by = 'title', all.x = T)

#fit lm model to see what relates to post score
lm_score <- lm(post_score~mean_commentscore+num_comments+title_length, data=aggDT)
plot(lm_score) #better models may be available. for sake of this project, continue with analysis.
summary(lm_score) #Predictors num_comments and title_length not significant
#Perform backward and forward selection to choose best model
step <- step(lm_score)
step #model: Post score vs. Mean comment score

#fit lm model for relationship between comment score and post age in days at comment submission
lm_daydiff <- lm(comment_score~com_postAge, data=DT)
plot(lm_daydiff) #normality, equivariance, linearity assumptions not met. no further analysis.

step_coef <- unclass(step)$coefficients
#ggplots
ggplot(aggDT, aes(x=mean_commentscore, y=post_score))+geom_point()+labs(title='Post Score vs. Mean Comment Score',caption='This plot is intended to visualize how the score of each post relates to the mean score of their comments')+geom_abline(aes(intercept=step_coef[1],slope=step_coef[2]))
ggplot(DT, aes(x=com_postAge, y=comment_score))+geom_point()+labs(title='Comment Score vs. Post Age in Days at Comment Submission', caption='This plot is intended to visualize how popular a comment gets based on how long it is posted after the post it is under')
ggplot(DT, aes(x=comment_score))+geom_histogram(binwidth = 1000)+labs(title='Frequencies of Comment Scores',caption='This plot visualizes the distribution of how comments are scored')
ggplot(aggDT, aes(x=title_length))+geom_histogram(binwidth = 10)+labs(title='Frequencies of Title Lengths',caption='This plot visualizes the distribution of how long users make post titles')

#write out data
fwrite(aggDT, 'aggDT.csv')
fwrite(DT, 'DT.csv')
