#installing packages
install.packages('class')
install.packages("corrplot")
install.packages("ggplot2")
#loading library
library(class)
library(corrplot)
library(ggplot2)
library(caret)

#reading the data file
Adult <- read.table('data/adult.data', sep = ",",strip.white = TRUE)
#used sep = "," to remove the "," from the end of the data set
#used strip.white = TRUE to remove white space from all the data

#to view first n lines of Adult Variable
head(Adult)
#to produce summary of the Adult data set
summary(Adult)
#to display the internal structure of Adult
str(Adult)

############################ Data Cleaning #############################

#As we can see "?" in the dataset
#changing "?" with NA
Adult[Adult == "?"] <- NA
#checking null values in Adult data set
sum(is.na(Adult))
#to count total number of row
nrow(Adult)
#Omitting NA values
Adult <- na.omit(Adult)
#count how much we left after we removed NA values
nrow(Adult)
#providing column names
colnames(Adult) <- c('age','workclass','fnlwgt','education', 'education.num'
                     ,'marital.status', 'occupation', 'relationship', 'race'
                     ,'sex','capital.gain', 'capital.loss', 'hours.per.week'
                     ,'native.country', 'income')
str(Adult)
#'Re-factoring' variables to exclude the unwanted levels created due to "?"
Adult$workclass <- factor(Adult$workclass)
Adult$occupation <- factor(Adult$occupation)
Adult$native.country <- factor(Adult$native.country)
str(Adult)

################################# END ###################################

##################### Correlation Matrix ################################

#to show how continous data are dependend on each others
#creating a data frame or all continous data in Adult dataset
#Changing income to 0, 1
Adult$income <- as.numeric(Adult$income)
ContinousData<- data.frame(Adult$age, Adult$fnlwgt,Adult$education.num
                           ,Adult$capital.gain,Adult$capital.loss
                           ,Adult$hours.per.week,Adult$income)
Dependency <- cor(ContinousData) # get correlations
corrplot(Dependency, method = "color") 
#Re-factoring income
Adult$income <- factor(Adult$income, labels=c("<=50k", ">50k"))
#Checking the levels
levels(Adult$income)
str(Adult)

################################# END ###################################

#assigning background color and border line
fill <- "gold1"
line <- "goldenrod2"

######################## AGE DISTRIBUTION ###################################

#calling ggplot functions to create a histogram
ggplot(Adult, aes(x=age)) + 
  geom_histogram(aes(fill = income), color = "black", binwidth = 1)+
  ggtitle("Histogram of Age Distribution") +
  xlab("Age") +
  ylab("Count")


################################ END ######################################


############# NO. of Adult working in Different Work Class ################

#calling ggplot function to create a bar graph
ggplot(data=Adult, aes(x=workclass)) +
  geom_bar() +
  geom_bar(fill = fill, colour = line) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Different Work Class")+
  xlab("Work Class") +
  ylab("Count")

################################ END ######################################

###################### NO. of Adult and Education #########################

#calling ggplot function to create a bar graph
ggplot(data=Adult, aes(x=education)) +
  geom_bar() +
  geom_bar(fill = fill, colour = line) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Different Education")+
  xlab("Education") +
  ylab("Count")

################################ END ######################################

################################ Marital Status ###########################

#calling ggplot function to create a bar graph
ggplot(data=Adult, aes(x=marital.status)) +
  geom_bar() +
  geom_bar(fill = fill, colour = line) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Marital Status")+
  xlab("Marital Status") +
  ylab("Count")

################################ END ######################################

############# NO. of Adult working in Different Occupation ################

#calling ggplot function to create a bar graph
ggplot(data=Adult, aes(x=occupation)) +
  geom_bar() +
  geom_bar(fill = fill, colour = line) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Different Occupation")+
  xlab("Occupations") +
  ylab("Count")

################################ END ######################################


######################### Adult Relationship #############################

#calling ggplot function to create a bar graph
ggplot(data=Adult, aes(x=relationship)) +
  geom_bar() +
  geom_bar(fill = fill, colour = line) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Different Relationship")+
  xlab("Relationship") +
  ylab("Count")

################################ END ######################################

########################### No. of Adult Race #############################

#calling ggplot function to create a bar graph
ggplot(data=Adult, aes(x=race)) +
  geom_bar() +
  geom_bar(fill = fill, colour = line) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Different Race")+
  xlab("Race") +
  ylab("Count")

################################ END ######################################

################################# Gender #################################

#calling ggplot function to create a bar graph
ggplot(data=Adult, aes(x=sex)) +
  geom_bar() +
  geom_bar(fill = fill, colour = line) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Different Gender")+
  xlab("Gender") +
  ylab("Count")

################################ END ######################################

################# Impact of Work Class on Income ##########################
ggplot(data=Adult, aes(x=workclass,fill = income)) +
  geom_bar(position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  ggtitle("Work Class Vs Income") +
  theme_minimal()+
  xlab("Work Class") +
  ylab("Count")

################################ END ######################################

################# Impact of Education on Income ##########################
ggplot(data=Adult, aes(x=education,fill = income)) +
  geom_bar(position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  ggtitle("Education Vs Income") +
  theme_minimal()+
  xlab("Education") +
  ylab("Count")

################################ END ######################################

################# Impact of Occupation on Income ##########################
ggplot(data=Adult, aes(x=occupation,fill = income)) +
  geom_bar(position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  ggtitle("Occupation Vs Income") +
  theme_minimal()+
  xlab("Occupation") +
  ylab("Count")

################################ END ######################################


############ relationship between Education vs Years of Education #########
ggplot(Adult, aes(x=education.num, y=education)) + 
  geom_point()+
  geom_smooth(method=lm) +
  ylab("Education") +
  xlab("Years of Education")

################################ END ######################################

############################## K-NN Model #################################

#set random seed for the results to be reproducible
set.seed(3)
#creating a variable for income value
income <- Adult$income
#dividing data into train and test part
#diving data into 70% and 30%
id <-   sample(2, nrow(Adult), prob = c(0.7,0.3), replace = TRUE )
#making categorical data into continous data 
Adult.model <- model.matrix(income~.-1,Adult)
#creating training and test data
TrainAdult <- Adult.model[id ==1,]
TestAdult <- Adult.model[id ==2,]
#calculating rows in train and test data set
nrow(TrainAdult)
nrow(TestAdult)
#summary for test and train data
summary(TrainAdult)
summary(TestAdult)
#to display the internal structure of training and test dataset
str(TrainAdult)
str(TestAdult)
#creating income test and train variable to pass in Knn
TrainIncome <- income[id==1]
TestIncome <- income[id==2]


#calling KNN function
#using k value as square root of no. of rows in Train Adult i.e. 145.8
knn.144 <- knn(train = TrainAdult, test = TestAdult, TrainIncome, k=144)
#calculating Accuracy
Accuracy <- 100 * sum(TestIncome == knn.144)/NROW(TestIncome)
Accuracy 

#calling KNN function
knn.145 <- knn(train = TrainAdult, test = TestAdult, TrainIncome, k=145)
#calculating Accuracy
Accuracy <- 100 * sum(TestIncome == knn.145)/NROW(TestIncome)
Accuracy 

#calling KNN function
knn.146 <- knn(train = TrainAdult, test = TestAdult, TrainIncome, k=146)
#calculating Accuracy
Accuracy <- 100 * sum(TestIncome == knn.146)/NROW(TestIncome)
Accuracy 

#making confusion matrix for most accurate model
table(knn.146,TestIncome)
confusionMatrix(table(knn.146,TestIncome))

#to check best value of K
i = 1 #declaration to initate loop
k.optm = 1 #declaration to initate loop

#for loop
#it takes some time for iteration
for(i in 1:20){
  knn.mod <- knn(train = TrainAdult, test = TestAdult, TrainIncome,k=i)
  k.optm[i]<- 100 * sum(TestIncome == knn.mod)/NROW(TestIncome)
  k=i
  cat(k," = ",k.optm[i],'\n')  # print % accuracy
}

#to plot % accuracy wrt to k-value
plot(k.optm,type = "b", xlab = "K-Value", ylab = "Accuarcy Level")
