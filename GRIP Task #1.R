#GRIP TASK 1
#NAME: Insha Shah
#Data Science and Business Analytics
#Prediction using Supervised ML

#Importing the data------
student_Data<- read.xlsx("C:/Users/salim/Desktop/Grip/Student percentage.xlsx", 1, T)

head(student_Data)
tail(student_Data)
dim(student_Data)

#Check for outliers----
boxplot(student_Data$Scores)
#No outliers in the data

#Plot the data----

base<- ggplot(student_Data, aes(x=Hours, y=Scores, col = "Red"))
base+geom_point()+stat_smooth(method = "lm")

#Scores and No of hours spent studying shows a direct linear relationship

#Split the data----
set.seed(100)
data1 = sample.split(student_Data, SplitRatio = 2/3)

train<- student_Data[data1==1,]
test<- student_Data[data1==0,]

#Fitting the model to train dataset----

m1<- lm(Scores~Hours, data = train)

summary(m1)
 
#Data is Trained

#Comparing the Actual V/s Predicted ones----
predicted_value=predict(m1, test)

df<- data.frame("Actual"= test[,2], "Predicted"=predicted_value)
df

#What will be predicted score if a student studies for 9.25 hrs/ day?----
hours = data.frame("Hours" = 9.25)
predicted_score<-predict(m1, hours)
predicted_score

#Predicted Score is 91.259
