#################################################################################
# APANPS5205_D01_2022_2 - APPLIED ANALYTICS FRAMEWORKS & METHDS II - Project Proposal 
# Authors : Kanyarat Suwannama 
#           Aaron K Johnson 
#           Nam Nguyen 
#           Katherine Mulligan 
#           venkat Deenamsetty
#
# Project Group : Group 10 
#################################################################################
library(car); 
library(dplyr); 
library(lubridate); 
library(stringr)
library(caret); 
install.packages('corrplot')
library(corrplot); 
library(ggcorrplot); 
library(glmnet)
library(caret); 
library(rpart); 
library(rpart.plot)
library(gbm); 
library(randomForest)

install.packages('dplyr')
library(dplyr);
library(tidyr)

setwd("C:/cu/s2/R_PROG/Assignment/Project_group")
imdb = read.csv("IMDB-Movie-Data.csv" , header=TRUE)
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


#===================================================================================================
#Final CSV file for analysis

write.csv(imdb_temp , "c:/temp/Final-IMDB-Movie-Data.csv" , row.names=TRUE)














