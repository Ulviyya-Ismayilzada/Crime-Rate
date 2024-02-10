library(tidyverse)
library(readr)
library(skimr)
library(inspectdf)
library(mice)
library(h2o)
library(faraway)
library(glue)
library(plotly)
library(patchwork)


#---------------------------Data understanding and data preprocessing--------------------------------------
df <- read.csv('crimes.csv')
df %>% view()
df %>% skimr::skim()

df %>% inspect_na()

target <-'ViolentCrimesPerPop'
features <- df %>% select(-ViolentCrimesPerPop) %>% names()

f <- as.formula(paste(target,paste(features,collapse='+'),sep='~'))  #collapse is for gathering data sctructures

glm <- glm(f,data=df)
glm %>% summary()



while(glm %>% faraway::vif() %>% sort(decreasing=T) %>% .[1]>=1.5){
  aftervif <- glm %>% faraway::vif() %>% sort(decreasing=T) %>% .[-1] %>% names()
  f <- as.formula(paste(target,paste(aftervif,collapse = '+'),sep='~'))
  glm <- glm(f,data=df)
}

glm %>% faraway::vif() %>% sort(decreasing=T) %>% names()->features

df <- df %>% select(target,features)


h2o.init()
h2o_data <- df %>% as.h2o()

h2o_data <- h2o_data %>% h2o.splitFrame(ratios=0.8,seed=123) #train and test split
train <- h2o_data[[1]]
test <- h2o_data[[2]]

#--------------------------------Modelling--------------------------------------------

model <- h2o.glm( 
  x=features,y=target,
  training_frame = train, 
  validation_frame = test,
  nfolds=10, seed=123, 
  lambda=0,compute_p_values = T) 

model@model$coefficients_table %>% 
  as.data.frame() %>% 
  dplyr::select(names,p_value) %>% 
  mutate(p_value=round(p_value,3)) %>% 
  .[-1,] %>% 
  arrange(desc(p_value))


features <- features[features!='PctImmigRecent']

y_pred <- model %>% h2o.predict(newdata=test) %>% as.data.frame()


#----------------------Checking metrics for evaluation of model-------------------------

test_set <- test %>% as.data.frame()  
residuals=test_set$ViolentCrimesPerPop-y_pred$predict
#RMSE
RMSE_test=sqrt(mean(residuals^2)) 
y_test_mean=mean(test_set$ViolentCrimesPerPop)

#R2
tss=sum((test_set$ViolentCrimesPerPop-y_test_mean)^2) #Total Sum of Squares 
rss=sum(residuals^2)
R2 = 1 - (rss/tss); R2

#Adj.R2
n <- test_set %>% nrow()
k <- features %>% length()
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))

tibble(RMSE=round(RMSE_test,1),
       R2,Adjusted_R2) 

my_data <- cbind(predicted=y_pred$predict,
                 observed=test_set$ViolentCrimesPerPop) %>% 
  as.data.frame() 


g <- my_data %>% 
  ggplot(aes(predicted,observed))+
  geom_point(color='darkred')+
  geom_smooth(method=lm)+
  labs(x='Predicted Power Output',
       y='Observed Power Output',
       title=glue('Test:Adjusted R2={round(enexpr(Adjusted_R2),2)}'))+
  theme(plot.title = element_text(color='darkgreen',size=16,hjust=0.5),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14))

g %>% ggplotly()

 #overfitting
y_pred_train <- model %>% h2o.predict(newdata=train) %>% as.data.frame()
train_set <- train %>% as.data.frame()  
residuals=train_set$ViolentCrimesPerPop-y_pred_train$predict

#RMSE
RMSE_train=sqrt(mean(residuals^2)) 
y_train_mean=mean(test_set$ViolentCrimesPerPop)

#R2
tss=sum((train_set$ViolentCrimesPerPop-y_train_mean)^2) #Total Sum of Squares 
rss=sum(residuals^2)
R2_train = 1 - (rss/tss); R2_train

#Adj R2
n <- train_set %>% nrow()
k <- features %>% length()
Adjusted_R2_train = 1-(1-R2_train)*((n-1)/(n-k-1))


my_data_train <- cbind(predicted=y_pred_train$predict,
                       observed=train_set$ViolentCrimesPerPop) %>% as.data.frame()



g_train <- my_data_train %>% 
  ggplot(aes(predicted,observed))+
  geom_point(color='darkred')+
  geom_smooth(method=lm)+
  labs(x='Predicted Power Output',
       y='Observed Power Output',
       title=glue('Train:Adjusted R2={round(enexpr(Adjusted_R2_train),2)}'))+
  theme(plot.title = element_text(color='darkgreen',size=16,hjust=0.5),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14))

g_train %>% ggplotly()


#Comparing train and testing results in one visual.

g_train+g

tibble(RMSE_train=round(RMSE_train,1),
       RMSE_test=round(RMSE_test,1),
       
       Adjusted_R2_train,
       Adjusted_R2_test=Adjusted_R2)

