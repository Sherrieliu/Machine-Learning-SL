rm(list =ls(all=T)); gc()
library(data.table)
mydata<- fread("ProjectDatabatch-aa.csv", header=F)
colnames(mydata) <- c("id","click","hour","C1","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model","device_type","device_conn_type","C14","C15","C16","C17","C18","C19","C20","C21")
head(mydata)

# Initial decisions on features go to fit the training data set 
# strategy 1: combine site/app based features to save the room
mydata$site <- paste(mydata$site_id,mydata$site_domain,mydata$site_category,sep = ".")
mydata$app <- paste(mydata$app_category,mydata$app_id,mydata$app_domain,sep = ".")

# strategy 2: calculate the click-through rate for the current day
#mydata$date <- substr(mydata$hour,5,6)
#mydata$pctr <- ave(mydata$click,mydata$date,  FUN=mean)

# we delete 141021xx

# strategy 3: extract day and hour from 'hour'
# day
library(lubridate)
lubridate::origin
mydata$weekday <- weekdays(as.Date(mydata$hour,origin="1970-01-01 UTC"))

#time
mydata$time <- substr(mydata$hour,7,8)

# pre-processing state one finished!!!

head(mydata)
# Strategy 4: count numbers of impression for the user in the whole 1 million dataset 
mydata$userid <- paste(mydata$device_model,mydata$device_ip,sep = "_")
mydata$count_device_ip <- ave(mydata$userid,mydata$device_ip,  FUN=length) # how popular the device_ip is
#save the cleaned file 
write.csv(mydata,file = "processedData1million.csv")

colnames(mydata)

mydata<-fread(file="processedData1million.csv",header=T)
# check levels of each variable. Apply 80-20 rule 
`%notin%` <- Negate(`%in%`)

var_count <- function (p){
    tmp <-0
    pct <- 0 
    while (pct<0.5) {
      tmp <- tmp +1 
      pct <-  nrow(mydata[mydata[, p %in% names(head(sort(table(p), decreasing = T),tmp)) ]])/nrow(mydata)
      print(pct)
    }
    return(tmp)}

colnames(mydata)
str(mydata)

var_count(mydata$banner_pos)
mydata[mydata[,banner_pos %notin% names(head(sort(table(mydata$banner_pos), decreasing = T), 2)) ]]$banner_pos <- "Other"

#2
var_count(mydata$app)
mydata[mydata[,app %notin% names(head(sort(table(mydata$app), decreasing = T), 10)) ]]$app <- "Other"
#10

var_count(mydata$site)
mydata[mydata[,site %notin% names(head(sort(table(mydata$site), decreasing = T), 27)) ]]$site <- "Other"
#27 

var_count(mydata$device_id)
unique(mydata$device_id)
mydata[mydata[,device_id %notin% names(head(sort(table(mydata$device_id), decreasing = T), 1)) ]]$device_id<- "Other"
#1 #drop

var_count(mydata$device_type)
mydata$device_type <- as.character(mydata$device_type)
mydata[mydata[,device_type %notin% names(head(sort(table(mydata$device_type), decreasing = T), 1)) ]]$device_type<- "Other"
#1 #drop
var_count(mydata$device_conn_type)
mydata$device_conn_type <- as.character(mydata$device_conn_type)
mydata[mydata[,device_type %notin% names(head(sort(table(mydata$device_type), decreasing = T), 1)) ]]$device_type<- "Other"
#1 # drop

# for those that have one level accounts for more than 80% of the total number, we decide to drop the variable 

var_count(mydata$device_model)
plot(table(mydata$device_model))
#since random forest only accepts 53 levels for each categorical variable, we decide to limit the level to 52, same apply to the following variables
mydata[mydata[,device_model %notin% names(head(sort(table(mydata$device_model), decreasing = T), 52)) ]]$device_model<- "Other"
# 285 

plot(table(mydata$device_ip))
var_count(mydata$device_ip)
mydata[mydata[,device_ip %notin% names(head(sort(table(mydata$device_ip), decreasing = T), 52)) ]]$device_ip<- "Other"
# too many unique values 
# drop 
var_count(mydata$site_id)
mydata[mydata[,site_id %notin% names(head(sort(table(mydata$site_id), decreasing = T), 2)) ]]$site_id<- "Other"
#2 

var_count(mydata$site_domain)
mydata[mydata[,site_domain %notin% names(head(sort(table(mydata$site_domain), decreasing = T), 2)) ]]$site_domain<- "Other"
#2 

var_count(mydata$site_category)
mydata[mydata[,site_category%notin% names(head(sort(table(mydata$site_category), decreasing = T), 2)) ]]$site_category<- "Other"
#2 

var_count(mydata$app_id)
mydata[mydata[,app_id %notin% names(head(sort(table(mydata$app_id), decreasing = T), 1)) ]]$app_id<- "Other"
#1

var_count(mydata$app_category)
mydata[mydata[,app_category %notin% names(head(sort(table(mydata$app_category), decreasing = T), 1)) ]]$app_category<- "Other"

#1

var_count(mydata$app_domain)
mydata[mydata[,app_domain %notin% names(head(sort(table(mydata$app_domain), decreasing = T), 1)) ]]$app_domain<- "Other"
#1 

#var_count(mydata$C1)
#str(mydata$C1)
#mydata[mydata[,C1 %notin% names(head(sort(table(mydata$C1), decreasing = T), 1)) ]]$C1<- "Other"
#1 
#drop

var_count(mydata$userid)
mydata[mydata[,userid%notin% names(head(sort(table(mydata$userid), decreasing = T), 52)) ]]$userid<- "Other"
# too many variables 

var_count(mydata$C14)
mydata$C14 <- as.character(mydata$C14)
mydata[mydata[,C14 %notin% names(head(sort(table(mydata$C14), decreasing = T), 52)) ]]$C14<- "Other"
#193

var_count(mydata$C15)
mydata$C15 <- as.character(mydata$C15)
mydata[mydata[,C15 %notin% names(head(sort(table(mydata$C15), decreasing = T), 1)) ]]$C15<- "Other"
#1 #drop
var_count(mydata$C16)
mydata$C16<- as.character(mydata$C16)
mydata[mydata[,C16 %notin% names(head(sort(table(mydata$C16), decreasing = T), 1)) ]]$C16<- "Other"
#1 #drop

var_count(mydata$C17)
mydata$C17<- as.character(mydata$C17)
mydata[mydata[,C17 %notin% names(head(sort(table(mydata$C17), decreasing = T), 52)) ]]$C17<- "Other"
#71 

var_count(mydata$C18)
mydata$C18<- as.character(mydata$C18)
mydata[mydata[,C18 %notin% names(head(sort(table(mydata$C18), decreasing = T), 3)) ]]$C18<- "Other"
#3

var_count(mydata$C19)
mydata$C19 <- as.character(mydata$C19)
mydata[mydata[,C19 %notin% names(head(sort(table(mydata$C19), decreasing = T), 10)) ]]$C19<- "Other"
#10 

var_count(mydata$C20)
mydata$C20 <- as.character(mydata$C20)
mydata[mydata[,C20 %notin% names(head(sort(table(mydata$C20), decreasing = T), 10)) ]]$C20<- "Other"
#10

var_count(mydata$C21)
mydata$C21 <- as.character(mydata$C21)
mydata[mydata[,C21 %notin% names(head(sort(table(mydata$C21), decreasing = T), 11)) ]]$C21<- "Other"
#11

var_count(mydata$weekday)
mydata[mydata[,weekday %notin% names(head(sort(table(mydata$weekday), decreasing = T), 6)) ]]$weekday<- "Other"
#6 

var_count(mydata$time)
mydata[mydata[,time %notin% names(head(sort(table(mydata$time), decreasing = T), 17)) ]]$time<- "Other"
#17 
var_count(mydata$C1)
mydata$C1 <- as.character(mydata$C1)
mydata[mydata[,C1 %notin% names(head(sort(table(mydata$C1), decreasing = T), 1)) ]]$C1<- "Other"

#change categorical variable levels to factor
Catval<- c("banner_pos","app","site", "device_id", "device_type","site_id","site_domain","site_category","app_id","app_domain",
           "app_category","device_conn_type",
           "device_conn_type","C15","C16","C18" , "C19" ,"C20","C21","C14","device_ip",
           "device_model","C17","weekday","time","userid","C1")

for(v in Catval) {
  mydata[[v]] <- as.factor(mydata[[v]])
}

mydata$count_device_ip<-as.numeric(mydata$count_device_ip)
str(mydata)


write.csv(mydata,file = "cleanData1million.csv")



#Transform test dataset so the variables match 
Test<-fread(file = "ProjectTestData.csv",header = T)
head(Test)
Test$site <- paste(Test$site_id,Test$site_domain,Test$site_category,sep = ".")
Test$app <- paste(Test$app_category,Test$app_id,Test$app_domain,sep = ".")
Test$weekday <- weekdays(as.Date(Test$hour,origin="1970-01-01 UTC"))
Test$time <- substr(Test$hour,7,8)
Test$userid <- paste(Test$device_model,Test$device_ip,sep = "_")
Test$count_device_ip <- ave(Test$userid,Test$device_ip,  FUN=length)
# how popular the device_ip is
#save the processed file 
write.csv(Test,file = "processedtest.csv")

#read in test file
Test<-fread(file="processedtest.csv",header = T)
# check variable type
str(Test)
# change categorical variables to factor 
colnames(Test)
#banner pos

Test$banner_pos <- as.character(Test$banner_pos)
Test[Test[,banner_pos %notin% names(head(sort(table(mydata$banner_pos), decreasing = T), 2)) ]]$banner_pos <- "Other"

table(Test$banner_pos)
#site id

Test[Test[,site_id %notin% names(head(sort(table(mydata$site_id), decreasing = T), 2)) ]]$site_id <- "Other"

#site_domain
Test[Test[,site_domain %notin% names(head(sort(table(mydata$site_domain), decreasing = T),2)) ]]$site_domain <- "Other"

# site category
Test[Test[,site_category %notin% names(head(sort(table(mydata$site_category), decreasing = T),2)) ]]$site_category <- "Other"

# app_id
Test[Test[,app_id %notin% names(head(sort(table(mydata$app_id), decreasing = T),1)) ]]$app_id<- "Other"

#app domain
Test[Test[,app_domain %notin% names(head(sort(table(mydata$app_domain), decreasing = T),1)) ]]$app_domain<- "Other"

#app category
Test[Test[,app_category %notin% names(head(sort(table(mydata$app_category), decreasing = T),1)) ]]$app_category<- "Other"

#device model
Test[Test[,device_model %notin% names(head(sort(table(mydata$device_model), decreasing = T),52)) ]]$device_model<- "Other"
#device_id
Test[Test[,device_id %notin% names(head(sort(table(mydata$device_id), decreasing = T),1)) ]]$device_id<- "Other"
#device_ip
Test[Test[,device_ip %notin% names(head(sort(table(mydata$device_ip), decreasing = T),52)) ]]$device_ip<- "Other"
#device_type
Test$device_type<- as.character(Test$device_type)
Test[Test[,device_type %notin% names(head(sort(table(mydata$device_type), decreasing = T),1)) ]]$device_type<- "Other"
#device_conn_type
Test$device_conn_type <- as.character(Test$device_conn_type)
Test[Test[,device_conn_type %notin% names(head(sort(table(mydata$device_conn_type), decreasing = T),1)) ]]$device_conn_type<- "Other"
#C15
Test$C15<- as.character(Test$C15)
Test[Test[,C15 %notin% names(head(sort(table(mydata$C15), decreasing = T),1)) ]]$C15<- "Other"
#C16
Test$C16 <- as.character(Test$C16)
Test[Test[,C16 %notin% names(head(sort(table(mydata$C16), decreasing = T),1)) ]]$C16<- "Other"
#C18
Test$C18 <- as.character(Test$C18)
Test[Test[,C18 %notin% names(head(sort(table(mydata$C18), decreasing = T),3)) ]]$C18<- "Other"
#site
Test[Test[,site%notin% names(head(sort(table(mydata$site), decreasing = T),27)) ]]$site<- "Other"
#app
Test[Test[,app%notin% names(head(sort(table(mydata$app), decreasing = T),10)) ]]$app<- "Other"
#weekday
Test[Test[,weekday %notin% names(head(sort(table(mydata$weekday), decreasing = T),6)) ]]$weekday<- "Other"
#time
Test[Test[,time %notin% names(head(sort(table(mydata$time), decreasing = T),17)) ]] <- "Other"
#userid
Test[Test[,userid %notin% names(head(sort(table(mydata$userid), decreasing = T),1)) ]]$userid<- "Other"
#C14
Test$C14<- as.character(Test$C14)
Test[Test[,C14 %notin% names(head(sort(table(mydata$C14), decreasing = T),52)) ]]$C14<- "Other"

#C17
Test$C17 <- as.character(Test$C17)
Test[Test[,C17 %notin% names(head(sort(table(mydata$C17), decreasing = T),52)) ]]$C17<- "Other"

#C19
Test$C19 <- as.character(Test$C19)
Test[Test[,C19 %notin% names(head(sort(table(mydata$C19), decreasing = T),10)) ]]$C19<- "Other"

#C20
Test$C20<- as.character(Test$C20)
Test[Test[,C20 %notin% names(head(sort(table(mydata$C20), decreasing = T),10)) ]]$C20<- "Other"

#C21
Test$C21 <- as.character(Test$C21)
Test[Test[,C21 %notin% names(head(sort(table(mydata$C21), decreasing = T),11)) ]]$C21<- "Other"

Catval<- c("banner_pos","app","site", "device_id", "device_type","site_id","site_domain","site_category","app_id","app_domain",
           "app_category","device_conn_type",
           "device_conn_type","C15","C16","C18" , "C19" ,"C20","C21","C14","device_ip",
           "device_model","C17","weekday","time","userid","C1")

for(v in Catval) {
  Test[[v]] <- as.factor(Test[[v]])
}


write.csv(Test, file = "test_cleaned.csv")


#-------------------------------------------------------------------------------------------------------------

# variables that will be used to train the model 
vars <-c("click","banner_pos","app","site","C18" , "C19" ,"C20","C21","C14",
         "device_model","C17","weekday","time","count_device_ip") 

xvars<-c("banner_pos","app","site","C18" , "C19" ,"C20","C21","C14",
         "device_model","C17","weekday","time","count_device_ip") 

#randomly split the dataset into 10 files with each a size of 100k records. 
mydata1<-fread(file="cleanmillionsplit1-aa.csv",header=T)
mydata1<-mydata1[,-1]
colnames(mydata1)
#check type of each column
str(mydata1)
Catval<- c("banner_pos","app","site", "device_id", "device_type","site_id","site_domain","site_category","app_id","app_domain",
           "app_category","device_conn_type",
           "device_conn_type","C15","C16","C18" , "C19" ,"C20","C21","C14","device_ip",
           "device_model","C17","weekday","time","userid")

for(v in Catval) {
  mydata1[[v]] <- as.factor(mydata1[[v]])
}

mydata1$count_device_ip<-as.numeric(mydata1$count_device_ip)


## 75% of the sample size
smp_size <- floor(0.75 * nrow(mydata1))

## set the seed 
set.seed(123)
# take 75% of 100K records as train data1 and the rest 25% as validation set1 
train_ind <- sample(seq_len(nrow(mydata1)), size = smp_size)

TrainData<- mydata1[train_ind, ]
colnames(TrainData)

#TrainData<-TrainData[,c(3,6,15,18,21:29,31)]

ValData <- mydata1[-train_ind, ]


### building model ###

# Logloss Function
LogLoss <- function(actual, prediction) {
  epsilon <- .00000001
  yhat <- pmin(pmax(prediction, epsilon), 1-epsilon)
  logloss <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat))
  return(logloss)
}


#(1) logistic regression using stepwise 
SmallFm <- click ~ 1
BigFm <-  as.formula(paste("click~ ", paste(xvars,collapse = "+")))
OutSmall <- glm(SmallFm,data=TrainData,family = binomial(link = "logit"))
sc<-list(lower=SmallFm,upper=BigFm)
out <- step(OutSmall,scope=sc,direction="forward")
#predict on validation set 
model_lr <- glm(formula(out),data = TrainData,family = binomial(link='logit'))
pred_lr <- predict(model_lr, newdata=ValData,type = 'response')
LogLoss(ValData$click,pred_lr)
#0.435

# (2) Randomforest
library(randomForest)
fml <- as.formula(paste("click~ ", paste(xvars,collapse = "+")))
fml
model.rf=randomForest(fml, data = TrainData,mtry=7,ntree=500,maxnodes=500)

rf_pred <- predict(model.rf, newdata =ValData)
LogLoss(ValData$click,rf_pred)
#0.428

# Ensemble Learning
ensamble_avr <- (pred_lr+rf_pred)/2
LogLoss(ValData$click,ensamble_avr)
#0.432



