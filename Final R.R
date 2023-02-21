################################################################################
# APANPS5205_D01_2022_2 - IMDB Movie Data Analysis - Group Project 
# Authors : Kanyarat Suwannama 
#           Aaron K Johnson 
#           Nam Nguyen 
#           Katherine Mulligan 
#           venkat Deenamsetty
#
# Project Group : Group 10 
################################################################################
library(car); 
library(dplyr); 
library(lubridate); 
library(stringr)
library(caret); 
library(corrplot); 
library(ggcorrplot); 
library(glmnet)
library(caret); 
library(rpart); 
library(rpart.plot)
library(gbm); 
library(randomForest)
library(dplyr);
library(tidyr)

#setwd("C:/cu/s2/R_PROG/Assignment/Project_group")
imdb = read.csv("Final-IMDB-Movie-Data.csv" , header=TRUE)
str(imdb)
summary(imdb)
glimpse(imdb)
head(imdb)
nrow(imdb)

#===============================================================================
# Data exploration 
#===============================================================================

#Identify Missing values 
missing.values <- imdb %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 

missing.values

missing.values %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='variable', y="number of missing values", title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

imdb  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()


# considering only numneric values for co relation matrix 
rem_cols<- c( "Title", "Genre", "Description" , "Director" , "Actors"   )
rem_cols <- colnames(imdb) %in% rem_cols
rem_cols
imdb_num <- imdb[,!rem_cols]
str(imdb_num)


#---------------------------------------------------------------------------------------------
# 	Correlation matrix visual plot
#---------------------------------------------------------------------------------------------
library(tidyr); library(dplyr); library(ggplot2)
columns_cor <- imdb_num
corMatrix = as.data.frame(cor(columns_cor))
corMatrix$var1 = rownames(corMatrix)

head(corMatrix)

corMatrix %>%
  gather(key=var2,value=r,1:7)%>%
  arrange(var1,desc(var2))%>%
  ggplot(aes(x=var1,y=reorder(var2, order(var2,decreasing=F)),fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'))+
  theme(axis.text.x=element_text(angle=75,hjust = 1))+xlab('')+ylab('')


#---------------------------------------------------------------------------------------------
# Correlation heat maps
#---------------------------------------------------------------------------------------------
library(ggcorrplot)
ggcorrplot(cor(imdb_num),
           method = 'square',
           type = 'lower',
           show.diag = F,
           colors = c('#e9a3c9', '#f7f7f7', '#a1d76a'))

#---------------------------------------------------------------------------------------------
#	Variance Inflating Factor (VIF) Map
#---------------------------------------------------------------------------------------------
model = lm(Metascore~.,imdb_num)
library(broom)
summary(model) %>%
  tidy()

library(car)
set.seed(100)


# #the linearly dependent variables and removing them from those variables 
# ld.vars <- attributes(alias(model)$Complete)$dimnames[[1]]
# ld.vars
# # [1] "host_total_listings_count"
# 
# ktrain_num$host_total_listings_count
# ktrain_num = select(ktrain_num, -host_total_listings_count)   

model = lm(Metascore~.,imdb_num)  # Re establish the model

#the linearly dependent variables
ld.vars <- attributes(alias(model)$Complete)$dimnames[[1]]
ld.vars

#calculate VIF values for predictor variables in model
vif(model)

#	Variance Inflating Factor (VIF) Map
data.frame(Predictor = names(vif(model)), VIF = vif(model)) %>%
  ggplot(aes(x=VIF, y = reorder(Predictor, VIF), fill=VIF))+
  geom_col()+
  geom_vline(xintercept=5, color = 'gray', size = 1.5)+
  geom_vline(xintercept = 10, color = 'red', size = 1.5)+
  scale_fill_gradient(low = '#fff7bc', high = '#d95f0e')+
  scale_y_discrete(name = "Predictor")+
  scale_x_continuous(breaks = seq(5,30,5))+
  theme_classic()


#---------------------------------------------------------------------------------------------
# 	Best Subset selection
#---------------------------------------------------------------------------------------------
install.packages('leaps')
library(leaps)
subsets = regsubsets(Metascore~.,data=imdb_num, nvmax=11)
summary(subsets)

#names(summary(subsets))
subsets_measures = data.frame(model=1:length(summary(subsets)$cp),
                              cp=summary(subsets)$cp,
                              bic=summary(subsets)$bic, 
                              adjr2=summary(subsets)$adjr2)
subsets_measures

library(ggplot2)
library(tidyr)
subsets_measures %>%
  gather(key = type, value=value, 2:4)%>%
  group_by(type)%>%
  mutate(best_value = factor(ifelse(value == min(value) | value== max(value),0,1)))%>%
  ungroup()%>%
  ggplot(aes(x=model,y=value))+
  geom_line(color='gray2')+
  geom_point(aes(color = best_value), size=2.5)+
  scale_x_discrete(limits = seq(1,11,1),name = 'Number of Variables')+
  scale_y_continuous(name = '')+
  guides(color=F)+
  theme_bw()+
  facet_grid(type~.,scales='free_y')


#===============================================================================
# Data cleanup 
#===============================================================================
# Removes the zero revenue rowswith NA values 
# imdb_temp = imdb %>%   drop_na(c("Revenue"))
imdb_temp = imdb
summary(imdb_temp)
# Removes the zero revenue rows
imdb_temp = imdb_temp[imdb_temp$Revenue>0.0001,]

# Replace Metascore NA valaues with mean of Metascore under the same rating 

imdb1 = imdb_temp
imdb_temp <- imdb_temp %>% 
  mutate(Metascore = ifelse(is.na(Metascore),
            mean(imdb1[imdb1$Rating == imdb_temp$Rating , "Metascore"] , na.rm =T ),
            Metascore))
summary(imdb_temp)

imdb_temp <- imdb_temp %>% 
  mutate(Revenue = ifelse(is.na(Revenue),
               mean(imdb1[imdb1$Rating == imdb_temp$Rating , "Revenue"] , na.rm =T ),
               Revenue))


#===============================================================================
#Final CSV file for analysis
write.csv(imdb_temp , "c:/temp/Final-IMDB-Movie-Data.csv" , row.names=TRUE)
#===============================================================================


################################################################################
# Research Question 1 
# Which factors contribute most to the IMDb score?
   
# Research Question 2		
# What is the impact of votes and revenue on IMDb score?
#   
# Research Question 3
# How do the categories affect IMDb score?
#   
# Research Question 4
# Are there any description keywords that are correlated with a higher IMDb score?
  
################################################################################

imdb=read.csv('Final-IMDB-Movie-Data.csv' , header=TRUE)

#===============================================================================
# (1) Which factors contribute most to the IMDb score?
#===============================================================================

## FEATURE SELECTION STARTS HERE

#	We split the Genre variable into 3 categorical variables, namely Genre1, 
# Genre2, Genre3.
library(stringr)
imdb[c('Genre1', 'Genre2', 'Genre3')] <- str_split_fixed(imdb$Genre,',', 3)
imdb[c('Actor1', 'Actor2', 'Actor3', 'Actor4')] <- str_split_fixed(imdb$Actors,',', 4)

# From domain knowledge, we select Genre1 as the main genre for the movie and 
# will use this variable for analysis.
# We convert two categorical variables, Genre1 and Director, to numerical ones 
# for further analysis.

imdb_subset = imdb
imdb_subset$Genre1_num <- as.numeric(as.factor(imdb_subset$Genre1))
imdb_subset$Director_num <- as.numeric(as.factor(imdb_subset$Director))

# Then we selected 7 numerical features that are most potential in predicting 
# Metascore: Rating, Votes, Revenue, Year, Runtime, Genre1_num (from Genre1), 
# and Director_num (from Director variable).
imdb_subset_num = imdb_subset[,c('Rating', 'Votes', 'Revenue', 'Year', 'Runtime','Genre1_num','Director_num', 'Metascore')]

## Split the analysis data into train & test sets
library(caret)
set.seed(617)
split = createDataPartition(y = imdb_subset_num$Metascore, p = 0.7, list = F, groups = 20)
train = imdb_subset_num[split,]
test = imdb_subset_num[-split,]

## SUBSET FORWARD SELECTION
start_mod = lm(Metascore~1,data=train)
empty_mod = lm(Metascore~1,data=train)
full_mod = lm(Metascore~.,data=train)
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')
forwardStepwise$anova %>% 
  mutate(step_number = as.integer(rownames(forwardStepwise$anova))-1) %>%
  mutate(Step = as.character(Step))%>%
  ggplot(aes(x = reorder(Step,X = step_number), y = AIC))+
  geom_point(color = 'darkgreen', size = 2) + 
  scale_x_discrete(name = 'Variable Entering Model')+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9))
# Best model with lowest AIC of 3625.65, having 4 variables: Rating, Genre1_num, Revenue and Year

## SUBSET BACKWARD SELECTION
start_mod = lm(Metascore~.,data=train)
empty_mod = lm(Metascore~1,data=train)
full_mod = lm(Metascore~.,data=train)
backwardStepwise = step(start_mod,
                        scope=list(upper=full_mod,lower=empty_mod),
                        direction='backward')
backwardStepwise$anova %>% 
  mutate(step_number = as.integer(rownames(backwardStepwise$anova))-1) %>%
  mutate(Step = as.character(Step))%>%
  ggplot(aes(x = reorder(Step,X = step_number), y = AIC))+
  geom_point(color = 'darkgreen', size = 2) + 
  scale_x_discrete(name = 'Variable Dropped')+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9))
# Best model with lowest AIC of 3626.08, having 4 variables: Genre1_num, Year, Rating and votes


## SUBSET STEPWISE SEARCH
start_mod = lm(Metascore~1,data=train)
empty_mod = lm(Metascore~1,data=train)
full_mod = lm(Metascore~.,data=train)
hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')
hybridStepwise$anova %>% 
  mutate(step_number = as.integer(rownames(hybridStepwise$anova))-1) %>%
  mutate(Step = as.character(Step))%>%
  ggplot(aes(x = reorder(Step,X = step_number), y = AIC))+
  geom_point(color = 'darkgreen', size = 2) + 
  scale_x_discrete(name = 'Variable Added or Dropped')+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9))
# Best model with lowest AIC of 3625.65, having 4 variables: Rating, Genre1_num, Revenue and Year

## From feature selection result, FEATURE SELECTION 1 includes (1)Rating, (2)Genre1_num, (3)Revenue and (4)Year
## FEATURE SELECTION ENDS HERE

## MODEL 1: LINEAR REGRESSION WITH FEATURES FROM FEATURE SELECTION 1
model1_fea1 = lm(Metascore~Rating+Genre1_num+Revenue+Year,train)
pred1_fea1_train = predict(model1_fea1,newdata = train)
rmse1_fea1_train = sqrt(mean((pred1_fea1_train-train$Metascore)^2)); rmse1_fea1_train
pred1_fea1 = predict(model1_fea1,newdata = test)
rmse1_fea1 = sqrt(mean((pred1_fea1-test$Metascore)^2)); rmse1_fea1
summary(model1_fea1)

#Train RMSE of 12.89758
#Test  RMSE of 13.130248

## MODEL 2: DECISION TREE WITH FEATURES FROM FEATURE SELECTION 1
library(rpart); library(rpart.plot)
set.seed(617)
trControl_fea1 = trainControl(method = 'cv',number = 5)
tuneGrid_fea1 = expand.grid(cp = seq(0,0.1,0.001))
tree_cv_train_fea1 = train(Metascore~Rating+Genre1_num+Revenue+Year,
                           data = train,
                           method = 'rpart',
                           trControl = trControl_fea1, 
                           tuneGrid = tuneGrid_fea1)
cvtree_train_fea1 = rpart(Metascore~Rating+Genre1_num+Revenue+Year, 
                          data = train, 
                          method = 'anova', 
                          cp = tree_cv_train_fea1$bestTune)

cvtree_train_fea1$variable.importance
plot(cvtree_train_fea1$variable.importance)

pred2_tree_train_fea1 = predict(cvtree_train_fea1, newdata = train)
rmse2_tree_train_fea1 = sqrt(mean((pred2_tree_train_fea1-train$Metascore)^2)); rmse2_tree_train_fea1

pred2_tree_fea1 = predict(cvtree_train_fea1, newdata = test)
rmse2_tree_fea1 = sqrt(mean((pred2_tree_fea1-test$Metascore)^2)); rmse2_tree_fea1

rpart.plot(cvtree_train_fea1)

#Train RMSE of 12.64019
#Test  RMSE of 13.25851

## MODEL 3: BOOSTING MODEL WITH FEATURES FROM FEATURE SELECTION 1
library(gbm)
set.seed(617)
boost_fea1 = gbm(Metascore~Rating+Genre1_num+Revenue+Year,
                 data=train,
                 distribution="gaussian",
                 n.trees = 500,
                 interaction.depth = 2,
                 shrinkage = 0.01)
summary(boost_fea1)

#From the Influence matrix in Boosting model which has lowest RMSE,..
#..we could see that Rating has the biggest influence on IMDb score (Metascore), far more than other variables.

pred3_boost_train_fea1 = predict(boost_fea1, newdata = train, n.trees = 500)
rmse3_boost_train_fea1 = sqrt(mean((pred3_boost_train_fea1-train$Metascore)^2)); rmse3_boost_train_fea1

pred3_boost_fea1 = predict(boost_fea1, newdata = test, n.trees = 500)
rmse3_boost_fea1 = sqrt(mean((pred3_boost_fea1-test$Metascore)^2)); rmse3_boost_fea1

plot(boost_fea1)
rpart.plot(boost_fea1)
str(imdb)
#Train RMSE of 11.98625
#Test  RMSE of 12.50689

#===============================================================================
#   (2) What is the impact of votes and revenue on IMDb score?
#===============================================================================
# We run two linear regression models to evaluate the impact each variable on IMDb score.

##Model 4: Metascore ~ Votes
plot(imdb$Votes, imdb$Metascore)
model4 = lm(Metascore~Votes,train)
equation1 = noquote(paste('Metascore =',round(model4$coefficients[1],4),' + ',model4$coefficients[2],'* Votes') )
equation1
summary(model4)

ggplot(data=train,aes(x=Votes,y=Metascore))+
  geom_point(color='darkgreen',size=0.9)+
  geom_smooth(method="lm",size=1.2,color="steelblue3",se=FALSE)+
  coord_cartesian(ylim=c(0,100))+
  theme_bw()+
  annotate('text',label=equation1,x=900000,y=95,color='sienna')

pred4_train = predict(model4,newdata = train)
rmse4_train = sqrt(mean((pred4_train-train$Metascore)^2)); rmse4_train
pred4 = predict(model4,newdata = test)
rmse4 = sqrt(mean((pred4-test$Metascore)^2)); rmse4

#Train RMSE of 15.89425
#Test  RMSE of 15.44226

##Model 5: Metascore ~ Revenue
plot(imdb$Revenue, imdb$Metascore)
model5 = lm(Metascore~Revenue,train)
equation2 = noquote(paste('Metascore =',round(model5$coefficients[1],4),' + ',round(model5$coefficients[2],4),'* Revenue') )
equation2
summary(model5)

ggplot(data=train,aes(x=Revenue,y=Metascore))+
  geom_point(color='darkgreen',size=0.9)+
  geom_smooth(method="lm",size=1.2,color="steelblue3",se=FALSE)+
  coord_cartesian(ylim=c(0,100))+
  theme_bw()+
  annotate('text',label=equation2,x=700,y=80,color='sienna')

pred5_train = predict(model5,newdata = train)
rmse5_train = sqrt(mean((pred5_train-train$Metascore)^2)); rmse5_train
pred5 = predict(model5,newdata = test)
rmse5 = sqrt(mean((pred5-test$Metascore)^2)); rmse5

#Train RMSE of 16.58929
#Test  RMSE of 16.21720

# The two linear regression models are statistically significant, so we can see both votes and revenue (independently) have positive impact on IMDb score.
# The correlation between Votes and Revenue is 0.61 which is significant.
# This can be explained that the more Votes a movie receives, the more people may see it, indicating higher potential box office/ Revenue.

## MODEL 6: LINEAR REGRESSION WITH VOTES & REVENUE
model6 = lm(Metascore~Revenue+Votes,train)

summary(model6)

## MODEL 7: TREE MODEL WITH VOTES & REVENUE
set.seed(617)
trControl_fea2 = trainControl(method = 'cv',number = 5)
tuneGrid_fea2 = expand.grid(cp = seq(0,0.1,0.001))
tree_cv_train_fea2 = train(Metascore~Votes+Revenue,
                           data = train,
                           method = 'rpart',
                           trControl = trControl_fea1, 
                           tuneGrid = tuneGrid_fea1)
cvtree_train_fea2 = rpart(Metascore~Votes+Revenue, 
                          data = train, 
                          method = 'anova', 
                          cp = tree_cv_train_fea2$bestTune)

cvtree_train_fea2$variable.importance
plot(cvtree_train_fea2$variable.importance)

pred7_tree_train_fea2 = predict(cvtree_train_fea2, newdata = train)
rmse7_tree_train_fea2 = sqrt(mean((pred7_tree_train_fea2-train$Metascore)^2)); rmse7_tree_train_fea2

pred7_tree_fea2 = predict(cvtree_train_fea2, newdata = test)
rmse7_tree_fea2 = sqrt(mean((pred7_tree_fea2-test$Metascore)^2)); rmse7_tree_fea2

rpart.plot(cvtree_train_fea2)

#Train RMSE of 15.48244
#Test  RMSE of 14.99312

## MODEL 8: BOOSTING MODEL WITH VOTES & REVENUE
# We then run a boosting model with two features, Votes and Revenue, to evaluate its impact on IMDb score. 
set.seed(617)
boost_fea2 = gbm(Metascore~Votes+Revenue,
                 data=train,
                 distribution="gaussian",
                 n.trees = 500,
                 interaction.depth = 2,
                 shrinkage = 0.01)
summary(boost_fea2)
# From the Influence matrix, we could see that Votes has the bigger influence than Revenue on IMDb score (Metascore).

pred8_boost_train_fea2 = predict(boost_fea2, newdata = train, n.trees = 500)
rmse8_boost_train_fea2 = sqrt(mean((pred8_boost_train_fea2-train$Metascore)^2)); rmse8_boost_train_fea2

pred8_boost_fea2 = predict(boost_fea2, newdata = test, n.trees = 500)
rmse8_boost_fea2 = sqrt(mean((pred8_boost_fea2-test$Metascore)^2)); rmse8_boost_fea2

#Train RMSE of 14.54730
#Test  RMSE of 14.50478

#===============================================================================
#   (3) How do the categories affect rating?
#===============================================================================

# We will apply text mining technique to evaluate impact of movie genre on IMDb score (Metascore)
# Remove all comma with space

library(stringr)
imdb$Genre = str_replace_all(imdb$Genre,",", " ")

# Create a corpus
library(tm)
corpus = Corpus(VectorSource(imdb$Genre))

# Clean text
corpus = tm_map(corpus,FUN = content_transformer(tolower))
# Create a dictionary
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(imdb$Genre))),lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

# Creating a DocumentTermMatrix (dtm)
dtm = DocumentTermMatrix(corpus)
xdtm = dtm
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))

#Browse tokens
sort(colSums(xdtm),decreasing = T)

# Building predictive model CART for Metascore
imdb_data = cbind(Metascore = imdb$Metascore,xdtm)

set.seed(617)
split2 = sample(1:nrow(imdb_data),size = 0.7*nrow(imdb_data))
train2 = imdb_data[split2,]
test2 = imdb_data[-split2,]

library(rpart); library(rpart.plot)
tree9 = rpart(Metascore~.,train2)
rpart.plot(tree9)

pred9_tree = predict(tree9,newdata=test2)
rmse9_tree = sqrt(mean((pred9_tree - test2$Metascore)^2)); rmse9_tree

# Visualizing Text
# Word Cloud
library(wordcloud)
wordcloudData = 
  imdb%>%
  group_by(Rank)%>%
  unnest_tokens(output=word,input=Genre)%>%
  ungroup()%>%
  select(Rank,word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()


library(wordcloud)
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(4,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))



imdb%>%
  unnest_tokens(input = Genre, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()


#===============================================================================
#(4) Are there any description keywords that are correlated with a higher IMDb score?
#===============================================================================
# Explore Text & Conduct Sentiment Analysis

str(imdb)
# Ratings of Reviews
median(imdb$Metascore) # median review rating

mean(imdb$Metascore)   # mean review rating

# Using dplyr

library(dplyr); library(magrittr)
imdb%>%
  summarize(average_rating = mean(Metascore), median_rating = median(Metascore))

# Distribution of Metascore
library(ggplot2); library(ggthemes)
ggplot(data=imdb,aes(x=Metascore))+
  geom_histogram(fill='sienna3')+
  theme_bw()+
  scale_x_reverse()+
  xlab('Metas core')+
  coord_flip()

# Characters
summary(nchar(imdb$Description))

# Words
summary(str_count(string = imdb$Description,pattern = '\\S+'))


# Common Words

library(dplyr); library(tidytext); library(magrittr)
imdb%>%
  unnest_tokens(input = Description, output = word)%>%
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)

# Visualize Common words
imdb%>%
  unnest_tokens(input = Description, output = word)%>%
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()


# Not surprisingly the list contains a lot of prepositions and articles and words 
# that doesn't convey much meaning. Such words are called stopwords. Let us look at the 
# top 25 list after removing the stopwords.
# 
# In the code below, this is accomplished through an anti-join with a list of stop words, 
# tidytext::stop_words.

imdb%>%
  unnest_tokens(input = Description, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)

imdb%>%
  unnest_tokens(input = Description, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()

# Words
# Let us begin by counting the number of words in each review.

library(dplyr); library(tidytext)
imdb %>%
  select(Rank , Title,Description)%>%
  group_by(Rank ,Title)%>%
  unnest_tokens(output = word,input=Description)%>%
  ungroup()%>%
  group_by(Rank ,Title)%>%
  summarize(count = n())


library(dplyr); library(tidytext)
imdb %>%
  select(Rank ,Title,Description)%>%
  group_by(Rank ,Title)%>%
  unnest_tokens(output = word,input=Description)%>%
  ungroup()%>%
  group_by(Rank ,Title)%>%
  summarize(count = n())%>%
  ggplot(aes(x=count))+geom_histogram(bins = 40)+xlab('Number of Words')


# It adds up to a lot of words.

imdb %>%
  select(Rank , Title,Description)%>%
  group_by(Title)%>%
  unnest_tokens(output = word,input=Description)%>%
  ungroup()%>%
  count()

# Categorize
#===================================================
# Bing
#====================================================
as.data.frame(get_sentiments('bing'))[1:50,]

get_sentiments('bing')%>%
  group_by(sentiment)%>%
  count()

# Valence of words
imdb%>%
  group_by(Rank , Title)%>%
  unnest_tokens(output = word, input = Description)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)

# Number of postive and negative words
imdb %>%
  group_by(Rank ,Title)%>%
  unnest_tokens(output = word, input = Description)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()

# Visualizing Number of postive and negative words

imdb%>%
  group_by(Rank ,Title)%>%
  unnest_tokens(output = word, input = Description)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=sentiment,y=n,fill=sentiment))+
  geom_col()+
  theme_economist()+
  guides(fill=F)+
  coord_flip()


# Positive Words
# Next, let us find out the proportion of words in reviews that are positive. 
# This is the ratio of number of positive words to sum of positive and negative words.

imdb %>%
  select(Rank ,Title,Description)%>%
  group_by(Rank ,Title)%>%
  unnest_tokens(output=word,input=Description)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

# Let us drill down a bit more to see whether the proportion of positive words has 
# any impact on its helpfulness. We will look at the proportion of positive 
# (and negative words) for each rating.

str(imdb)
imdb %>%
  select(Rank ,Title, Description, Metascore )%>%
  group_by(Rank ,Title, Metascore)%>%
  unnest_tokens(output=word,input= Description)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by( Metascore ,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))


# Here it is in pictures. What do you think?
library(ggthemes)
imdb %>%
  select(Rank ,Title, Description, Metascore)%>%
  group_by(Rank ,Title , Metascore)%>%
  unnest_tokens(output=word,input=Description)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Metascore,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=Metascore,y=proportion,fill=sentiment))+
  geom_col()+
  theme_economist()+
  coord_flip()


# Positive Reviews
# Let us compute the proportion of positive words for each review. The proportion 
# of positive words is the ratio of positive words and the sum of positive and negative words. 
# This differs from the analysis above as it is computed for each review.

imdb %>%
  group_by(Rank ,Title , Metascore)%>%
  unnest_tokens(output = word, input = Description)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Rank ,Title,Metascore)%>%
  summarize(positive_words = sum(sentiment=='positive'),
            negative_words = sum(sentiment=='negative'),
            proportion_positive = positive_words/(positive_words+negative_words))%>%
  ungroup()


# Let us see if reviews with a lot of positive words are rated favorably.

imdb%>%
  group_by(Rank ,Title , Metascore)%>%
  unnest_tokens(output = word, input = Description)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Rank ,Title, Metascore)%>%
  summarize(positive_words = sum(sentiment=='positive'),
            negative_words = sum(sentiment=='negative'),
            proportion_positive = positive_words/(positive_words+negative_words))%>%
  ungroup()%>%
  summarize(correlation = cor(proportion_positive,Metascore))

# correlation
# <dbl>
#   1      0.0199

# Visualizing Text
# In my opinion, word clouds offer little insight into the data, yet they tend to 
# be very good at capturing interest of non-technical audiences. Let us begin by 
# creating a word cloud from our data using 
# library(tidytext), library(dplyr), library(tidyr),and library(wordcloud) functions.


# Word Cloud
library(wordcloud)
wordcloudData = 
  imdb%>%
  group_by(Rank ,Title)%>%
  unnest_tokens(output=word,input=Description)%>%
  ungroup()%>%
  select(Rank ,Title,word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()


library(wordcloud)
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(4,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))  

# Comparison Cloud
# Finally, here is a comparison cloud to contrast positive and negative words in the reviews.

library(tidyr)
wordcloudData = 
  imdb%>%
  group_by(Rank)%>%
  unnest_tokens(output=word,input=Description)%>%
  ungroup()%>%
  select(Rank,word)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(3,0.5),max.words = 200, rot.per=0)


# afinn sentiment
# The bing and nrc emotion lexicons classify a word based on the presence or absence of an emotion or valence. The afinn lexicon scores each word based on the extent to which it is positive or negative. For instance, the afinn lexicon will make a distinction between words “satisfied” and “delighted” based on how positive they are, but the bing lexicon will simply categorize both as being positive.
get_sentiments('bing')[get_sentiments('bing')$word %in% c('satisfied','delighted'), ]
get_sentiments('afinn')[get_sentiments('afinn')$word %in% c('satisfied','delighted'),]

afinn = get_sentiments('afinn')
afinn[1:50,]


imdb %>%
  select(Rank ,Title,Description)%>%
  group_by(Rank)%>%
  unnest_tokens(output=word,input=Description)%>%q
inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),
            max=max(reviewSentiment),
            median=median(reviewSentiment),
            mean=mean(reviewSentiment))


# Here is the distribution of sentiment.

imdb %>%
  select(Rank ,Title, Description)%>%
  group_by(Rank)%>%
  unnest_tokens(output=word,input=Description)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+
  scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_wsj()

#===============================================================================
# MODEL ENDS HERE
#===============================================================================