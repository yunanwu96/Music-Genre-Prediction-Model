# Install packages
install.packages('nortest')

# Import libraries
library(dplyr)
library(nortest)
library(janitor)
library(csv)
library(dplyr)
library(plyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(car)
library(tidyverse)
library(leaps)
library(gvlma)
library(caTools)
library(caret)
library(stats)
library(ISLR)
library(gridExtra)
library(pROC)
library(InformationValue)

# Import dataset
data<-read.csv("C:/Users/Yunan/Desktop/Aly6015/finalproject/music_genre.csv")
# for use in MacOS system
#getwd()
#setwd("/Users/zhaoqian/Desktop/NEU/ALY6015/FinalProject")
#data <- read.csv("music_genre.csv",stringsAsFactors = FALSE)
View(data)

#Determine data type and find missing value and delete
is.numeric(data$popularity)
is.numeric(data$acousticness)
is.numeric(data$danceability)
is.numeric(data$duration_ms)
is.numeric(data$energy)
is.numeric(data$instrumentalness)
is.numeric(data$key)
typeof(data$key)
is.numeric(data$liveness)
is.numeric(data$loudness)
is.character(data$mode)
typeof(data$mode)
is.numeric(data$speechiness)
is.numeric(data$tempo)
typeof(data$tempo)
is.numeric(data$valence)
is.numeric(data$obtained_date)
typeof(data$obtained_date)
is.numeric(data$valence)
data$tempo<-as.numeric(as.character(data$tempo))
summary(data$tempo)
summary(data$popularity)

#Outlier and distribution 
nortest::ad.test(data$danceability)
hist(data$danceability)
boxplot.stats(data$danceability)$out
boxplot.stats(data$popularity)$out
boxplot.stats(data$instrumentalness)$out
boxplot.stats(data$energy)$out
boxplot.stats(data$acousticness)$out
boxplot.stats(data$liveness)$out
boxplot.stats(data$loudness)$out
boxplot.stats(data$speechiness)$out
boxplot.stats(data$tempo)$out

#Find the missing value and 
which(is.na(data$popularity))
which(is.na(data$acousticness))
which(is.na(data$danceability))
which(is.na(data$duration_ms))
which(is.na(data$energy))
which(is.na(data$instrumentalness))
which(is.na(data$key))
which(is.na(data$liveness))
which(is.na(data$loudness))
which(is.na(data$tempo))
Nonmissing<-data[-c(10001,10002,10003,10004,10005),]
View(Nonmissing)
summary(Nonmissing)
Nonmissing$tempo<-as.numeric(as.character(Nonmissing$tempo))
summary(Nonmissing)
boxplot.stats(Nonmissing$tempo)
View(Nonmissing)
Nonmissing$tempo[is.na(Nonmissing$tempo)]<-mean(Nonmissing$tempo,na.rm=TRUE)
df <- Nonmissing
View(Nonmissing)
summary(Nonmissing)
summary(Nonmissing$music_genre)

#Create dummy variables 
Nonmissing$Alternativeness<-with(Nonmissing,ifelse(music_genre=="Alternative",'1','0'))
Nonmissing$Animeness<-with(Nonmissing,ifelse(music_genre=="Anime",'1','0'))
Nonmissing$Blueness<-with(Nonmissing,ifelse(music_genre=="Blues",'1','0'))
Nonmissing$Classicalness<-with(Nonmissing,ifelse(music_genre=="Classical",'1','0'))
Nonmissing$Countriness<-with(Nonmissing,ifelse(music_genre=="Country",'1','0'))
Nonmissing$Electronicness<-with(Nonmissing,ifelse(music_genre=="Electronic",'1','0'))
Nonmissing$Hiphopness<-with(Nonmissing,ifelse(music_genre=="Hip-Hop",'1','0'))
Nonmissing$Jazzness<-with(Nonmissing,ifelse(music_genre=="Jazz",'1','0'))
Nonmissing$Rapness<-with(Nonmissing,ifelse(music_genre=="Rap",'1','0'))
Nonmissing$Rockness<-with(Nonmissing,ifelse(music_genre=="Rock",'1','0'))

#Convert to numerical
Nonmissing$Alternativeness<-as.numeric(as.character(Nonmissing$Alternativeness))
Nonmissing$Animeness<-as.numeric(as.character(Nonmissing$Animeness))
Nonmissing$Blueness<-as.numeric(as.character(Nonmissing$Blueness))
Nonmissing$Classicalness<-as.numeric(as.character(Nonmissing$Classicalness))
Nonmissing$Countriness<-as.numeric(as.character(Nonmissing$Countriness))
Nonmissing$Electronicness<-as.numeric(as.character(Nonmissing$Electronicness))
Nonmissing$Hiphopness<-as.numeric(as.character(Nonmissing$Hiphopness))
Nonmissing$Jazzness<-as.numeric(as.character(Nonmissing$Jazzness))
Nonmissing$Rapness<-as.numeric(as.character(Nonmissing$Rapness))
Nonmissing$Rockness<-as.numeric(as.character(Nonmissing$Rockness))
clean<-Nonmissing
View(clean)
summary(clean)
str(clean)

slices <-table(clean$key)
lbls <-paste(names(slices))
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Music key distribution",radius=1.5)

slices <-table(clean$mode)
lbls <-paste(names(slices))
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Music mode distribution",radius=1.5)

# Check distribution of numeric variables
hist(clean$liveness, prob = TRUE, ylab = "Density", xlab = "Size")
hist(clean$acousticness, prob = TRUE, ylab = "Density", xlab = "Size")
hist(clean$danceability, prob = TRUE, ylab = "Density", xlab = "Size")
hist(clean$duration_ms, prob = TRUE, ylab = "Density", xlab = "Size")
hist(clean$energy, prob = TRUE, ylab = "Density", xlab = "Size")
hist(clean$instrumentalness, prob = TRUE, ylab = "Density", xlab = "Size")
hist(clean$loudness, prob = TRUE, ylab = "Density", xlab = "Size")
hist(clean$speechiness, prob = TRUE, ylab = "Density", xlab = "Size")
hist(clean$tempo, prob = TRUE, ylab = "Density", xlab = "Size")
hist(clean$valence, prob = TRUE, ylab = "Density", xlab = "Size")

# Popularity and Music Genre
# Pie chart of music genre
slices <-table(clean$music_genre)
lbls <-paste(names(slices))
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Music genre distribution",radius=1.5)

# Table of Popularity & Music genre
genre_popu <- clean %>% group_by(music_genre) %>% 
  summarise(count = n(), popular_sum = sum(popularity), popular_mean = mean(popularity)) %>%
  arrange(desc(popular_mean))

# Top music genre by average popularity
barplot(genre_popu$popular_mean,
        ylab = "Average Popularity", cex.names = 0.6,
        names.arg = genre_popu$music_genre, xlab = "Music Genre", col = "lightblue", las = 2) 

# Distribution of popularity by music genre
clean %>%
  group_by(music_genre) %>% 
  mutate(avg = mean(popularity)) %>%
  ggplot() + 
  geom_boxplot(aes(reorder(music_genre, avg), popularity, fill = avg)) + 
  coord_flip() + theme_minimal() + 
  scale_fill_continuous(low = '#ffffcc', high = '#fc4e2a', name = "Average Popularity") + 
  labs(x = 'Music Genre', y = 'Popularity')+
  expand_limits(y = c(0,5))
# Remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
clean %>%
  group_by(music_genre) %>% 
  mutate(avg = mean(popularity), popularity = remove_outliers(popularity)) %>%
  ggplot() + 
  geom_boxplot(aes(reorder(music_genre, avg), popularity, fill = avg)) + 
  coord_flip() + theme_minimal() + 
  scale_fill_continuous(low = '#ffffcc', high = '#fc4e2a', name = "Average Popularity") + 
  labs(x = 'Music Genre', y = 'Popularity')+
  expand_limits(y = c(0,5))



###############################################################
# The 1st research question
# Create data frames
df1 <- clean %>% group_by(music_genre) %>% filter(music_genre == 'Blues') %>%
  summarize(popularity = popularity)

df2 <- clean %>% group_by(music_genre) %>% filter(music_genre == 'Electronic') %>%
  summarize(popularity = popularity)

df3 <- clean %>% group_by(music_genre) %>% filter(music_genre == 'Jazz') %>%
  summarize(popularity = popularity)

# Check the distribution
hist(df1$popularity, prob = TRUE, ylab = "Density", xlab = "Size")
hist(df2$popularity, prob = TRUE, ylab = "Density", xlab = "Size")
hist(df3$popularity, prob = TRUE, ylab = "Density", xlab = "Size")

# Combine the data frame into one
df4 <- rbind(df1, df2, df3)
df4$music_genre <- as.factor(df4$music_genre)

set.seed(123)
Index <- createDataPartition(df4$popularity, p = 0.5, list = FALSE, times = 1)
df5 <- df4[Index,]

# Check the variance
bartlett.test(popularity ~ music_genre, data = df5)

# Run the ANOVA test
anova <- aov(popularity ~ music_genre, data = df5)

# View the model summary
summary(anova)

# Save summary to an object
a.summary <- summary(anova)

# Degrees of freedom
# k - 1: between group variance - numerator
df.numerator <- a.summary[[1]][1, 'DF']
df.numerator

# N - k: within group variance - denominator
df.denominator <- a.summary[[1]][2, 'DF']
df.denominator

# Extract the F test value from the summary
F.value <- a.summary[[1]][[1, 'F value']]
F.value

# Extract the F test value from the summary
p.value <- a.summary[[1]][[1, 'Pr(>F)']]
p.value

# Determine if we should reject the null hypothesis
ifelse(p.value > 0.05, 'fail to reject the null', 'reject the null')

# Find the difference
turkey.test <- TukeyHSD(anova)
turkey.test
plot(turkey.test)
ggplot(df5,aes(x=music_genre,y=popularity))+
  geom_boxplot(aes(fill=music_genre),notch=T,width=0.5)+
  geom_jitter(width=0.1)+theme_bw()+
  theme(legend.position = "top")


# The 2nd Research Question
# create correlate matrix and plot
clean_num <- select_if(clean, is.numeric) # extract numeric variables
str(clean_num)
#pairs(clean_num, pch = 19)
corr <- cor(clean_num, use = "pairwise")
corr["acousticness",] # Energy has the highest correlation
corrplot(corr, type = "upper", tl.cex = 0.5)

# check the distribution of dependent variable before establishing the model
hist(clean_num$acousticness,main = "Distribution of acousticness")
summary(clean_num$acousticness)
# use glm since distribution is not normal
# add smaller values to "accousticness" since gamma cannot fit values 0
clean_num$adj_accoustic <- clean_num$acousticness + 0.000001
clean_num <- subset(clean_num, select = -c(acousticness, Alternativeness,
                                           Animeness, Blueness, Classicalness, 
                                           Countriness, Electronicness,
                                           Hiphopness, Jazzness, Rapness, Rockness))
summary(clean_num$adj_accoustic)

# split
set.seed(123)
trainIndex <- sort(sample(x = nrow(clean_num), size = nrow(clean_num) * 0.7)) 
sample_train <- clean_num[trainIndex,]
sample_test <- clean_num[-trainIndex,]

train_x <- model.matrix(adj_accoustic~., sample_train)[,-1]
test_x <- model.matrix(adj_accoustic~., sample_test)[,-1]

train_y <- sample_train$adj_accoustic
test_y <- sample_test$adj_accoustic

# apply lasso regression to find the best lambda
library(glmnet)
set.seed(123)
cv.lasso <- cv.glmnet(train_x, train_y, nfolds = 10)
cv.lasso
plot(cv.lasso)

lambda.min <- cv.lasso$lambda.min
lambda.1se <- cv.lasso$lambda.1se

model.lasso.min <- glmnet(train_x,train_y,alpha = 1, lambda = lambda.min)
coef(model.lasso.min)
model.lasso.1se <- glmnet(train_x,train_y,alpha = 1, lambda = lambda.1se)
coef(model.lasso.1se)

# lasso model with lambda min
predict.lasso.min.train <- predict(model.lasso.min, newx = train_x)
rmse(train_y, predict.lasso.min.train)
predict.lasso.min.test <- predict(model.lasso.min, newx = test_x)
rmse(test_y, predict.lasso.min.test)

# lasso regression model with lambda 1se
predict.lasso.1se.train <- predict(model.lasso.1se, newx = train_x)
rmse(train_y, predict.lasso.1se.train)
predict.lasso.1se.test <- predict(model.lasso.1se, newx = test_x)
rmse(test_y, predict.lasso.1se.test)


#Third question
library("caret")
View(clean_num)
View(data)
str(clean_num)
clean<-subset(clean,select=-c(Alternativeness,Animeness,Blueness,Classicalness,Countriness,Electronicness,
                              Hiphopness,Jazzness,Rapness,Rockness))
View(clean)
clean$ModeMajorness<-with(clean,ifelse(mode=="Major",'Yes','No'))
clean$ModeMajorness<-as.factor(clean$ModeMajorness)
str(clean)
Index<-sort(sample(x=nrow(clean),size=nrow(clean)*0.7))
TrainM<-clean[Index,]
TestM<-clean[-Index,]
View(TrainM)
View(TestM)
str(TrainM)
str(TrainM)
str(College)
model_back<-step(glm(ModeMajorness~acousticness+danceability+energy+instrumentalness+liveness+loudness
                     +speechiness+tempo+valence,data=TrainM,family=binomial(link="logit")),direction = 'backward')
str(TrainM)
summary(model_back)
prob.train<-predict(model_back,newdata=TrainM,type="response")
predicted.Modeness<-as.factor(ifelse(prob.train>=0.5,"Yes","No"))
confusionMatrix(predicted.Modeness,TrainM$ModeMajorness,positive="Yes")
library(pROC)
ROC1<-roc(TrainM$ModeMajorness,prob.train)
plot(ROC1,col="blue",ylab="Sensitivity-TP Rate",xlab="Specificity-FP Rate")
auc<-auc(ROC1)
auc