read.csv("/Users/shrilekha/Desktop/Kaggle/Titanic/train.csv", header = TRUE)
 train = read.table("/Users/shrilekha/Desktop/Kaggle/Titanic/train.csv", header = TRUE, sep = ",")
train
test = read.table("/Users/shrilekha/Desktop/Kaggle/Titanic/test.csv", header = TRUE, sep = ",")
test
head(train)
str(train)
install.packages("ggplot")
install.library(ggplot)
install.packages("dplyr")
install.library(dplyr)
install.packages("ggthemes")
install.library(ggthemes)
library(scales)

#pclass = ticketclass: 1, 2 and 3 class
#SibSp = # of siblings / spouses aboard the Titanic
#Parch = # of parents/children aboard the Titanic, if parch = 0 then those children travel with nanny only
# Cabin = cabi number
#Port of Embarkation; key:C = Cherbourg,Q = Queenstown,S = Southampton

dim(train)
dim(test)
attach(train)
detach(train)
combine = bind_rows(train, test)
combine
md.pattern(combine)
bind_rows
dim(combine)
head(combine)
dim(combine) 
combine[891:1309,]
##R has subsituted all unavailable survival value as NA in Combine table
str(combine)
## Feature Engineering
## We can use families surname in predition. It will help us to know that whether the family members 
##have survived together or not. Or were they together during the accident time. 
combine$Name
combine$Title <- gsub('(.*, )|(\\..*)', '', combine$Name) ##(Did not get that)
combine$Title
table(combine$Sex, combine$Title)
Rare = c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer", "Lady", "Major", "Rev", "Sir", "the Countess")
Rare
combine$Title[combine$Title == 'Mlle']        <- 'Miss' 
combine$Title[combine$Title == 'Ms']          <- 'Miss'
combine$Title[combine$Title == 'Mme']         <- 'Mrs' 
combine$Title[combine$Title %in% Rare]  <- 'Rare Title'
table(combine$Sex, combine$Title)

combine$Surname <- sapply(combine$Name,  
                  function(x) strsplit(x, split = '[,.]')[[1]][1])
combine$Surname
cat(paste( unique(Surname),nlevels(factor(Surname))))
combine$Family_size <- combine$SibSp + combine$Parch + 1         
combine$Family_size  

combine$Family <- paste(combine$Surname, combine$Family_size, sep='_')
combine$Family

ggplot(combine[1:891,], aes(x= Family_size, fill = factor(Survived))) + geom_bar(stat = 'count', position = 'dodge')
+labs(x ='Family Size') 

##Discretized family size
combine$Family_sizeD[combine$Family_size==1] = 'single'
combine$Family_sizeD[combine$Family_size < 4 & combine$Family_size >1] = 'medium'
combine$Family_sizeD[combine$Family_size >= 4] = 'large'

combine$Family_sizeD


mosaicplot(table(combine$Family_sizeD, combine$Survived), main='Family Size by Survival', shade=TRUE)

## Small families survived compared to the single and large families
combine$Cabin[1:28]
strsplit(combine$Cabin[2], NULL)[[1]]
##Creation of Deck Variable:

combine$Deck = sapply(combine$Cabin, function(x) strsplit(x,NULL)[1][1])
combine[28,]

##Value Imputation 
 
cat(paste(combine[c(62, 830), 'Fare'][[1]][1], combine[c(62,830),'Fare'][[1]][2],
          combine[c(62, 830), 'Pclass'][[1]][1], combine[c(62,830),'Pclass'][[1]][2]))

embark_fare = combine%>%
  filter(combine$PassengerId != 62 & combine$PassengerId != 830)
embark_fare
head(embark_fare)
  combine$Embarked[c(62, 830)]= 'C'
combine[1044,]

ggplot(combine[combine$Pclass == '3' & combine$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = 'pink') + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=0.5) 

combine$Fare[1044]
combine$Fare[1044] <- median(combine[combine$Pclass=='3' & combine$Embarked=='S',]$Fare, na.rm = TRUE)
combine$Fare[1044]
table(is.na(combine$Age))

factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                'Title','Surname','Family','Family_sizeD')


 combine[factor_vars] <- lapply(combine[factor_vars], function(x) as.factor(x))
combine[factor_vars] 
names(combine[factor_vars])
names(combine)
ss = combine[,-c(1,3,5,12,13,14,16,17,4,9,11,18,15)]
ss
library(rpart)
install.packages("mice")
install.library(mice)
install.packages("Hmisc")
install.library(Hmisc)
set.seed(200)

factor(combine$Survived)
is.factor(combine$Survived)

# install.packages("Amelia")
#  libraamelia_fit <- amelia(ss, m=5, parallel = "multicore")
# md.pattern(combine)
#  # ?amelia
 mice_mod <- mice(combine[, !names(combine) %in% c('PassengerId', 'Pclass', 'Sex', 'Embarked', 'Title', 'Surname', 'Family', 'Family_sizeD')], method='rf')
 
 # 
output = complete(mice_mod)

output

par(mfrow=c(1,2))
hist(combine$Age, freq=F, main='Age: Original Data', 
     col='pink', ylim=c(0,0.04))
hist(output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

summary(output$Age)
summary(combine$Age)
combine$Age = output$Age
combine$Age
summary(combine$Age)
sum(is.na(combine$Age))
install.packages("stringi",dependencies = TRUE )
library(stringi)
install.packages("devtools")
library(devtools)

# First we'll look at the relationship between age & survival
ggplot(combine[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram( + theme_classic()
  
?ggplot
?facet_grid
 combine$child[combine$Age < 18] = 'child'
 combine$adult[combine$Age>= 18] ='adult'
 table(combine$child, combine$Survived)
 table(combine$adult, combine$Survived)
 
 
 combine$Mother[combine$Sex =='female' & combine$Parch > 0 & combine$Age > 18 & combine$Title !='Miss'] = 'Mother'
 table(combine$Survived,combine$Mother)
 
 combine$child = factor(combine$child)
 combine$Mother = factor(combine$Mother)
 is.factor(combine$Mother)
 md.pattern(combine)
 train_titanic <- combine[1:891,]
 dim(train)
 test_titanic <- combine[892:1309,]
 
 install.packages("e1071")
 library(e1071)
 # Naive_bayes =naiveBayes(combine$Survived ~., data=train)
 # Naive_bayes
 # set.seed(754)
 # rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
 #                            Fare + Embarked + Title + 
 #                         child + Mother,
 #                          data = train)
 # 
 # 
# combine$Mother[combine$Mother==NA] = 'NO'
# table(combine$Survived, combine$Mother)
 combine$Survived= factor(combine$Survived)
 is.factor(combine$Survived)
 contrasts(combine$Survived)
 
#  test_titanic$Survived[test_titanic$Survived==1] ='yes'
#  test_titanic$Survived[test_titanic$Survived==0]= 'no'
# contrasts(test_titanic$Survived)
# test_titanic$Survived=factor(test_titanic$Survived)
 
 log_fit<-glm(Survived~Sex+ Age+Parch+Title+Embarked + SibSp+ Pclass,data = train_titanic, family="binomial")
 log_fit
 predicted_log_prob<-predict(log_fit,data=train_titanic, type="response")
predicted_log_fit
 predicted_log_fit<-ifelse(predicted_log_prob>0.5,"1","0")
 predicted_log_fit
 mean(train_titanic$Survived==predicted_log_fit) #fraction of data that is correctly predicted
 1-mean( train_titanic$Survived == predicted_log_fit)
 
 log_fit_test<-glm(factor(Survived)~Sex+ Age+Parch+Embarked + SibSp+ Pclass,data = test_titanic, family="binomial")
 log_fit_test
 predicted_log_prob_test<-predict(log_fit,data=test_titanic, type="response")
 predicted_log_prob_test
 predicted_log_fit_test<-ifelse(predicted_log_prob>0.5,"1","0")
 predicted_log_fit_test

 


 