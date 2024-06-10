install.packages('corrgram')
data <- read.csv('Transactions_Customer.csv',stringsAsFactors = TRUE)
head(data,10)

library(ggplot2)
library(corrgram)

#data
data$Seen_Voucher<-as.factor(data$Seen_Voucher)
data$Advertisement_Channel<-as.factor(data$Advertisement_Channel)

colnames(data)
dim(data)
summary(data)

sum(complete.cases(data))
sum(!complete.cases(data))

cor(data)
corrgram(data)
#Dark shade indicate strong correlation, blue indicates positive and red indicate negative between two variables.


plot(data$Estimated_Age,data$Revenue,asp=0.2)
abline(lm(Revenue ~ Estimated_Age, data = data), col = "blue")

plot(data$Time_On_Site ,data$Revenue,asp=0.5)
abline(lm(Revenue ~ Time_On_Site, data = data), col = "blue")

plot(data$Seen_Voucher,data$Revenue,asp=0.1)
abline(lm(Revenue ~ Seen_Voucher, data = data), col = "blue")

plot(data$Estimated_Income,data$Revenue,asp=100)
abline(lm(Revenue ~ Estimated_Income, data = data), col = "blue")

plot(data$Advertisement_Channel,data$Revenue,asp=.1)
abline(lm(Revenue ~ Advertisement_Channel, data = data), col = "blue")



#estimated Age
ggplot(data) + geom_histogram(aes(Estimated_Age ),binwidth = 0.5,fill='#cb4154') +
  labs(title = "Histogram of Estimated Age",x = "Estimated age",y = "# Of Customers") +
  theme(legend.position="none")

ggplot(data=data) + geom_point(aes(Revenue,Estimated_Age))+
  labs(title = "Estimated Age",y = "Age",x = "Revenue")

ggplot(data=data) + geom_point(aes(Revenue,Estimated_Age,color=Seen_Voucher))+
  labs(title = "Revenue and Estimated Age with Seen Voucher information",x = "Revenue",y = "Age")+scale_colour_hue(l = 45)+theme(legend.position="top")

ggplot(data=data) + geom_point(aes(Revenue,Estimated_Age,color=Advertisement_Channel))+
  labs(title = "Revenue and Estimated Age with Advertisement Channel information",x = "Revenue",y = "Age")+scale_colour_hue(l = 45)+theme(legend.position="top")

#seen voucher
ggplot(data) + geom_histogram(aes( as.integer(Seen_Voucher)),binwidth = 0.5,fill='#cb4154') +
  labs(title = "Histogram of People who has seen and not seen voucher",x = "1: not_seen     2:Seen",y = "# Of customers") +
  theme(legend.position="none")

ggplot(data=data) + geom_point(aes(Seen_Voucher,Revenue))+
  labs(title = "Revenue and seen_voucher",y = "Revenue",x= "Seen_voucher")+scale_colour_hue(l = 45)+theme(legend.position="top")

ggplot(data=data) + geom_point(aes(Seen_Voucher,Revenue,color=Advertisement_Channel))+
  labs(title = "Revenue and seen_voucher with Advertisement Channel information",y = "Revenue",x= "Seen_voucher")+scale_colour_hue(l = 45)+theme(legend.position="top")

ggplot(data=data) + geom_boxplot(aes(Seen_Voucher,Revenue))+
  labs(title = "Revenue and seen_voucher",y = "Revenue",x= "Seen_voucher")

#Estimated Income
ggplot(data) + geom_histogram(aes( Estimated_Income),binwidth = 200,fill='#cb4154') +
  labs(title = "Histogram of Estimated Income",x = "Income",y = "# Of customers") +
  theme(legend.position="none")

ggplot(data=data) + geom_point(aes(Revenue,Estimated_Income))+
  labs(title = "Estimated Income",y = "Income",x = "Revenue")

ggplot(data=data) + geom_point(aes(Revenue,Estimated_Income,color=Seen_Voucher))+
  labs(title = "Revenue and Estimated_Income with Seen Voucher information",x = "Revenue",y = "Income")+scale_colour_hue(l = 45)+theme(legend.position="top")

ggplot(data=data) + geom_point(aes(Revenue,Estimated_Income,color=Advertisement_Channel))+
  labs(title = "Revenue and Estimated Income with Advertisement Channel information",x = "Revenue",y = "Income")+scale_colour_hue(l = 45)+theme(legend.position="top")


#ADVERTISEMENT CHANNEL
ggplot(data) + geom_histogram(aes( as.integer(Advertisement_Channel)),binwidth = 0.1,fill='#cb4154') +
  labs(title = "Histogram of Type of Advertisement Channel",x = "1:Leaflet     2:Social Media     3:Search Engine     4:Influencer",y = "# Of customers") +
  theme(legend.position="none")

ggplot(data=data) + geom_point(aes(Advertisement_Channel,Revenue))+
  labs(title = "Revenue and Advertisement_Channel",y = "Revenue",x= "Advertisement_Channel")+scale_colour_hue(l = 45)+theme(legend.position="top")

ggplot(data=data) + geom_point(aes(Advertisement_Channel,Revenue,color=Seen_Voucher))+
  labs(title = "Revenue and Advertisement_Channel with Seen Voucher Information",y = "Revenue",x= "Advertisement_Channel")+scale_colour_hue(l = 45)+theme(legend.position="top")

ggplot(data=data) + geom_boxplot(aes(Advertisement_Channel,Revenue))+
  labs(title = "Revenue and Advertisement_Channel",y = "Revenue",x= "Advertisement_Channel")+geom_abline(slope=12, intercept=50)
#time on site
ggplot(data) + geom_histogram(aes(Time_On_Site),binwidth = 5,fill='#cb4154') +
  labs(title = "Histogram of Time spent by customer on website",x = "Time_On_Site",y = "# Of customers") +
  theme(legend.position="none")

ggplot(data=data) + geom_point(aes(Revenue,Time_On_Site))+
  labs(title = "Time_On_Site",y = "Time On Site",x = "Revenue")

ggplot(data=data) + geom_point(aes(Revenue,Time_On_Site,color=Seen_Voucher))+
  labs(title = "Revenue and Time On Site with Seen Voucher information",x = "Revenue",y = "Time On Site")+scale_colour_hue(l = 45)+theme(legend.position="top")

ggplot(data=data) + geom_point(aes(Revenue,Time_On_Site,color=Advertisement_Channel))+
  labs(title = "Revenue and Time On Site with Advertisement Channel information",x = "Revenue",y = "Time On Site")+scale_colour_hue(l = 45)+theme(legend.position="top")
