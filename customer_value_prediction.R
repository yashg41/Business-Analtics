install.packages('corrgram')
data <- read.csv('Transactions_Customer.csv',stringsAsFactors = TRUE)
head(data,10)
data<-data[data$Estimated_Age>=18,]
dim(data)

library(ggplot2)
library(corrgram)



colnames(data)
dim(data)
summary(data)

sum(complete.cases(data))
sum(!complete.cases(data))

cor(data)
corrgram(data)
#Dark shade indicate strong correlation, blue indicates positive and red indicate negative between two variables.


library(dplyr)
model <- lm(Revenue ~., data)
summary(model)

model <- lm(Revenue ~Seen_Voucher+Estimated_Income+Advertisement_Channel, data)
summary(model)

#customers > 45
model <- lm(Revenue ~., data[data$Estimated_Age>45,])
summary(model)


dim(data[data$Estimated_Age>45,])
dim(data[data$Estimated_Age<=45,])
summary(data[data$Estimated_Age>45,])
summary(data[data$Estimated_Age<45,])
summary(data)


#infleuncers

summary(data[data$Advertisement_Channel!=4,])
summary(data[data$Advertisement_Channel==1,])
summary(data[data$Advertisement_Channel==2,])
summary(data[data$Advertisement_Channel==3,])
summary(data[data$Advertisement_Channel==4,])


model <- lm(Revenue ~., data[data$Advertisement_Channel==4,])
summary(model)

#voucher seen
summary(data[data$Seen_Voucher==0,])
summary(data[data$Seen_Voucher==1,])
model <- lm(Revenue ~., data[data$Seen_Voucher==1,])
summary(model)
dim(data[data$Seen_Voucher==0,])



model <- lm(Estimated_Income ~., data)
summary(model)
