setwd('/Users/timchen/R/big_data_final/')
library(dplyr)
library(data.table)
library(stringr)
library(tidyverse)
library(DMwR)
#player.personal.data<-fread("player_data.csv")
player.personal.data2<-fread("player_data3.csv")
free.throws<-fread('free_throws.csv')


#################Create make_miss ############
play_id<-paste(free.throws$game_id,free.throws$period,free.throws$time,sep = '-')
free.throws<-mutate(free.throws,Play_id=play_id)
free.throws$make_miss<-ifelse(grepl('make',free.throws$play),1,0)
free.throws$techique<-ifelse(grepl('technical',free.throws$play),TRUE,FALSE)
###############################################
#################individule freethrow status ############

generate_new_data<-function(z){
  if(length(z)==1){
    return(paste(z[1],-1,-1))
  }
  else if(length(z)==2){
    return(paste(z[1],z[2],-1))
  }
  else{
    return(paste(z[1],z[2],z[3]))
  }
} 

new.free.throws<-free.throws%>%group_by(Play_id)%>%summarise(binary_status=generate_new_data(make_miss)
                                                             ,tech=sum(techique),
                                                             player=first(player)
                                                             ,time_line=first(time)
                                                             ,period=first(period))
new.free.throws<- separate(new.free.throws,binary_status,c("free_1", "free_2", "free_3"),' ')

merge.data=merge(new.free.throws,player.personal.data2,by = "player",all.x = TRUE)
 

# Andrew's code
# transform freethrow "-1" to NA
free_2 <- merge.data$free_2 
free_2[which(free_2 == '-1')] <- NA
merge.data$free_2 <- free_2

free_3 <- merge.data$free_3 
free_3[which(free_3 == '-1')] <- NA
merge.data$free_3 <- free_3



# group by the freethrow percentage
merge.data$free_1 <- as.integer(merge.data$free_1)
merge.data$free_2 <- as.integer(merge.data$free_2)
merge.data$free_3 <- as.integer(merge.data$free_3)

merge.data_gb<- group_by(merge.data, player, Pos, body_fat, hand_length, hand_width, height_with_shoes, standing_reach,vertical_max ,weight, wingspan,bench,agility,sprint) %>%
  summarise(free_average = (sum(free_1, na.rm = T) + sum(free_2, na.rm = T) + sum(free_3, na.rm = T)) / (sum(!is.na(free_1)) + sum(!is.na(free_2)) + sum(!is.na(free_3))), free_total = sum(!is.na(free_1)) + sum(!is.na(free_2)) + sum(!is.na(free_3)) )
####### filter free total over 50 #######
merge.data_gb<-filter(merge.data_gb,free_total>=0) # free_total>=50

####### modify column 


####################
 
############  transform'-' to na ############
merge.data_gb[is.na(merge.data_gb$hand_length)|merge.data_gb$hand_length=='',]$hand_length='-'
merge.data_gb[is.na(merge.data_gb$hand_width)|merge.data_gb$hand_width=='',]$hand_width='-'
merge.data_gb[is.na(merge.data_gb$weight)|merge.data_gb$weight=='',]$weight='-'

merge.data_gb[merge.data_gb$hand_length=='-',]$hand_length=NA
merge.data_gb[merge.data_gb$hand_width=='-',]$hand_width=NA
merge.data_gb[merge.data_gb$weight=='-',]$weight=NA

####################################
# transform char to interger or factor

merge.data_gb$Pos=as.factor(merge.data_gb$Pos)
# remove % from body_fat
merge.data_gb$body_fat<-gsub( "%", "", as.character(factor(merge.data_gb$body_fat)))
colu_names<-c('body_fat', 'hand_length','hand_width','weight')
merge.data_gb[colu_names]=lapply(merge.data_gb[colu_names], unlist)
merge.data_gb[colu_names]=lapply(merge.data_gb[colu_names], as.numeric)

#######  mice generate data ######
library(mice)

merge.data_gb.mice<-mice(merge.data_gb,m=2,maxit = 50,method="cart",seed = 100)
merge.data_gb=complete(merge.data_gb.mice,1)
new_data_cat <- mutate(merge.data_gb, 
                       free_cat = case_when(
                         free_average <= 0.63 ~ 'suck',
                         free_average > 0.63 &  free_average <= 0.83 ~ 'mediocre',
                         free_average > 0.83 ~ 'fantastic'),
                       wingspan_ratio=wingspan/height_with_shoes,
                       BMI=weight*703/(height_with_shoes*height_with_shoes)
)

new_data_cat$free_cat <- as.factor(new_data_cat$free_cat)
table(new_data_cat$free_cat)
smote.new_data_cat=SMOTE(free_cat~.,new_data_cat[,-1],perc.over=400,perc.under=200)
table(smote.new_data_cat$free_cat)
smote.new_data_cat=SMOTE(free_cat~.,smote.new_data_cat,perc.over=110,perc.under=400)
table(smote.new_data_cat$free_cat)
new_data_cat=smote.new_data_cat
new_data_cat$free_cat <- as.factor(new_data_cat$free_cat)
new_data_cat=new_data_cat[,-14]

library(caret)
smp_size<-floor(nrow(merge.data_gb)*0.7)
set.seed(100)
train_idx<-sample(seq_len(nrow(new_data_cat)),size = smp_size)
train_set<-new_data_cat[train_idx,]
test_set<- new_data_cat[-train_idx,] 

train_set_withoutna=na.omit(train_set)
test_set_withoutna=na.omit(test_set)

### multinomial logistic regression
library(nnet)
model=multinom(data = train_set_withoutna,formula = free_cat~.-free_average)

summary(model)
response <- predict(model,newdata=test_set_withoutna)
probability <- predict(model,newdata=test_set_withoutna, 'prob')

confusionMatrix(response, test_set_withoutna$free_cat)
library(pROC)
plot.roc(test_set_withoutna$free_cat, probability[, 1]) #probability of fantastic
auc(test_set_withoutna$free_cat, probability[, 1])




#------- Random forest
library(ranger)
library(randomForest)

#rf_free_cat<-train(free_cat~.-free_average,data=train_set_withoutna,method="rf")
tc <- trainControl("cv",10)
rf_free_cat <- train(free_cat~.-free_average,
                     data=train_set_withoutna, method="rf",
                     trControl=tc)
summary(rf_free_cat)
varImp(rf_free_cat)

response <- predict(rf_free_cat,newdata=test_set_withoutna)
probability <- predict(rf_free_cat,newdata=test_set_withoutna, 'prob')

confusionMatrix(response, test_set_withoutna$free_cat)
library(pROC)
plot.roc(test_set_withoutna$free_cat, probability[, 1]) #probability of fantastic
auc(test_set_withoutna$free_cat, probability[, 1])



### plot the decision tree
library(rattle)
#tree_free_cat<-train(free_cat~.-free_average,data=train_set_withoutna,method="rpart",tuneLength=10)
#rpart.model<-rpart::rpart(free_cat~.-free_average,train_set_withoutna)

tc <- trainControl("cv",10)
rpart.grid <- expand.grid(.cp=0.01) 
tree_free_cat <- train(free_cat~.-free_average,
                       data=train_set_withoutna, method="rpart",
                       trControl=tc,  tuneGrid=rpart.grid)


fancyRpartPlot(tree_free_cat$finalModel)

########### the model of feature selection #########
tc1 <- trainControl("cv",10)
rf_free_cat.b <- train(free_cat~wingspan+hand_width+standing_reach+agility+weight,
                       data=train_set_withoutna, method="rf",
                       trControl=tc1)
summary(rf_free_cat.b)
varImp(rf_free_cat.b)

response <- predict(rf_free_cat.b,newdata=test_set_withoutna)
probability <- predict(rf_free_cat.b,newdata=test_set_withoutna, 'prob')

confusionMatrix(response, test_set_withoutna$free_cat)
library(pROC)
plot.roc(test_set_withoutna$free_cat, probability[, 1]) #probability of fantastic
auc(test_set_withoutna$free_cat, probability[, 1])

plot.roc(test_set_withoutna$free_cat, probability[, 2]) #probability of mediocre
auc(test_set_withoutna$free_cat, probability[, 2])



#-------------------  xgboost
library(xgboost)
library(keras)

Train_data_class <- train_set_withoutna %>% select(-(free_average:free_total))

Test_data_class <- test_set_withoutna %>% select(-(free_average:free_total))
# 將訓練期與測試期資料轉為xgboost矩陣格式
Train_data_class$free_cat<-as.numeric(Train_data_class$free_cat)
Train_data_class$Pos<-as.numeric(Train_data_class$Pos)
Test_data_class$Pos<-as.numeric(Test_data_class$Pos)

Test_data_class$free_cat<-as.numeric(Test_data_class$free_cat)


x_train<-Train_data_class %>% select(-free_cat) %>% as.matrix()
y_train<-Train_data_class %>% select(free_cat) %>% pull() %>% as.matrix()
x_test<-Test_data_class %>% select(-free_cat) %>% as.matrix()
y_test<-Test_data_class %>% select(free_cat) %>% pull() %>% as.matrix()

trainData<-list(data=as(x_train, "dgCMatrix"),label=as.numeric(y_train))
trainData1<-xgb.DMatrix(trainData$data,label=trainData$label)
testData<-list(data=as(x_test, "dgCMatrix"),label=as.numeric(y_test))
# -------------將訓練期與測試期資料轉為xgboost矩陣格式-----------------

# 建立參數組合表
paramTable <- expand.grid(eta = c(0.05, 0.1,0.3,0.5), max_depth = c(3:10), 
                          subsample = c(0.5,0.7, 0.9), colsample_bytree = c(0.7, 1, 0.5) 
)
# 隨機抽取n=??個參數組合，在此用5
paramTable <- paramTable[sample(c(1:nrow(paramTable)), 5),]


# 進行交叉驗證挑選最佳參數
cvOutput <- NULL
for(iy in c(1:nrow(paramTable))){
  
  params <- list(booster = "gbtree", 
                 num_class =4,
                 eta = paramTable$eta[iy], 
                 max_depth = paramTable$max_depth[iy], 
                 subsample = paramTable$subsample[iy], 
                 colsample_bytree = paramTable$colsample_bytree[iy], 
                 "eval_metric" = "auc",
                 objective = "multi:softprob")
  
  cvResult <- xgb.cv(params = params, data = trainData1, nrounds = 300, nfold = 5, early_stopping_rounds = 10, verbose = 1)
  
  cvOutput <- cvOutput %>%
    bind_rows(tibble(paramsNum = iy,
                     bestIteration = cvResult$best_iteration,
                     bestCvlogloss = cvResult$evaluation_log$auc[bestIteration]))
  print(tail(cvOutput,10))
}

# ---------------------------------交叉驗證最佳參數--------------------------------
bestCvSite <- which(cvOutput$bestCvlogloss == min(cvOutput$bestCvlogloss))
bestCvlogloss <- cvOutput$bestCvlogloss[bestCvSite]
bestIteration <- cvOutput$bestIteration[bestCvSite]
bestParamsNum <- cvOutput$paramsNum[bestCvSite]
# ---------------------------------交叉驗證最佳參數--------------------------------
# 最佳參數組合
my_params <- list(booster = "gbtree", 
                  num_class =4,
                  eta = paramTable$eta[bestParamsNum], 
                  max_depth = paramTable$max_depth[bestParamsNum], 
                  subsample = paramTable$subsample[bestParamsNum], 
                  colsample_bytree = paramTable$colsample_bytree[bestParamsNum], 
                  "eval_metric" = "mlogloss",
                  objective = "multi:softmax")
# xgboost模型訓練
xgbModel <- xgb.train(data = trainData1,
                      # watchlist = list(train = trainData),
                      params = my_params,
                      maximize = FALSE,       # 如果模型衡量值要越大越好就要選TRUE，例如AUC，此處為logloss，要越小越好
                      nrounds = bestIteration)

#---------------------------------------模型存取-------------------------------------
xgb.save(xgbModel, "xgboost.model")
mod1 <- xgb.load("xgboost.model")
#---------------------------------------模型存取-------------------------------------
importance_matrix <- xgb.importance(colnames(trainData$data),model = mod1)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix[1:nrow(importance_matrix),])

#---------------------------------------模型預測-------------------------------------
pred_es = (predict(mod1, testData$data)) %>% as.numeric()

cft_es = confusionMatrix(pred_es %>% as.factor(), testData$label %>% as.factor(), positive = '1')
print(cft_es$overall)
cft_es$table

#-------------a smaill hypothsis test for variables
hand_cat_relation<-test_set_withoutna%>%select(free_cat,hand_width)%>%group_by(free_cat)%>%summarise(hand_width_mean=mean(hand_width))
anova.hand_width<-aov(hand_width~free_cat,data = test_set_withoutna)
summary(anova.hand_width)

agility_relation<-test_set_withoutna%>%select(free_cat,agility)%>%group_by(free_cat)%>%summarise(agility_mean=mean(wingspan))
anova.agility<-aov(agility~free_cat,data=test_set_withoutna)
summary(anova.agility)

wingspan.relation<-test_set_withoutna%>%select(free_cat,wingspan)%>%group_by(free_cat)%>%summarise(wingspan_mean=mean(wingspan))
anova.wingspan<-aov(wingspan~free_cat,data = test_set_withoutna)
summary(anova.wingspan)
