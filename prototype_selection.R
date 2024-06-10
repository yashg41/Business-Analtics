install.packages("./MCDA_0.1.0.tar.gz")
install.packages(c('Rglpk','triangle','plyr','ggplot2','glpkAPI','combinat'))

install.packages('MCDA')
library('MCDA')

data <- read.csv('Robot_Info.csv')
data

data$weights=c(0.07,0.2,0.13,0.27,0.33)
data


performanceTable <- data[,c(1,2,3,4,5)] # Retain only column that is related to alternative characteristiperformanceTable
performanceTable

### Weighted Sum Method
performanceTable <- data.frame(t(performanceTable))
colnames(performanceTable) <- performanceTable[1,]
performanceTable <- performanceTable[2:5,]
performanceTable
performanceTable <- sapply(performanceTable,as.numeric)
performanceTable


weights <- data[,c(1,6)] # Retain only column that is related to alternative characteristic
weights
weights <- data.frame(t(weights))
weights
weights <- as.numeric(weights[2,])
weights

performanceTable[,4] <- performanceTable[,4]^-1
performanceTable

performanceTable <- performanceTable/colSums(performanceTable)[col(performanceTable)]
performanceTable

overall1 <- weightedSum(performanceTable, weights)
names(overall1) <- colnames(data[,c(2:5)])
overall1



par(mfrow=c(1,1))
barplot(overall1, main="WSM Score for robot",
        xlab="Robot Prototype", col=c("darkblue","red","green","yellow"))
par(mfrow=c(1,2))
barplot(overall1, main="WSM Score for robot",
        xlab="Robot Prototype", col=c("darkblue","red","green","yellow"))

#model 2 "KInda sensitivity analysis"
data <- read.csv('Robot_Info.csv')
data

data$weights=c(0.33,0.2,0.27,0.13,0.07)
data


performanceTable2 <- data[,c(1,2,3,4,5)] # Retain only column that is related to alternative characteristiperformanceTable
performanceTable2

### Weighted Sum Method
performanceTable2 <- data.frame(t(performanceTable2))
colnames(performanceTable2) <- performanceTable2[1,]
performanceTable2 <- performanceTable2[2:5,]
performanceTable2 <- sapply(performanceTable2,as.numeric)
performanceTable2


weights2 <- data[,c(1,6)] # Retain only column that is related to alternative characteristic
weights2 <- data.frame(t(weights2))

weights2 <- as.numeric(weights2[2,])
weights2

performanceTable2[,4] <- performanceTable2[,4]^-1
performanceTable2

performanceTable2 <- performanceTable2/colSums(performanceTable2)[col(performanceTable2)]
performanceTable2

overall2 <- weightedSum(performanceTable2, weights2)
names(overall2) <- colnames(data[,c(2:5)])
overall2

barplot(overall2, main="WSM Score for robot(Equal weightage to all criteria)",
        xlab="robot Model", col=c("darkblue","red","green","yellow"))

# DEVIANT IS THE BEST ROBOT


install.packages('goalp')
library('goalp')

data$Deviant[4]
goals<- "cost: (XG+XC+XS)*7100 + 1600*XG+1000*XC+600*XS = 250000
TECHNICIAN: 10*XG+7*XC+5*XS = 250
NO OF ORDERS: 9*XG+6*XC+4*XS >=95|1#
XG lBound 5
XC lBound 5
XS lBound 5"

# Solve problem
gp <- goalp(goals)

#> Solving underlying linear optimisation problem.
# Print results to screen
summary(gp)


#220 orders if 1000 pounds added
library(goalp)
# Write goals to a text variable
goals <- "cost: (XG+XC+XS)*7100 + 1600*XG+1000*XC+600*XS = 250000|#2 #2
TECHNICIAN: (10*XG+7*XC+5*XS)*4 = 250*4|#1 #1
NO OF ORDERS: 9*XG+6*XC+4*XS =95|#4 #4
XG lBound 5
XC lBound 5
XS lBound 5"
goals

# Solve problem
gp <- goalp(goals)
summary(gp)



#getting the rakshan's answer
library(goalp)
# Write goals to a text variable
goals <- "cost: (XG+XC+XS)*7100 + 1600*XG+1000*XC+600*XS <= 250000|#4 
TECHNICIAN: (10*XG+7*XC+5*XS)*4 = 250*4|#1 #1
NO OF ORDERS: 9*XG+6*XC+4*XS >=0|
XG lBound 5
XC lBound 5
XS lBound 5"
goals

# Solve problem
gp <- goalp(goals)
summary(gp)

library(goalp)
# Write goals to a text variable
goals <- "cost: (XG+XC+XS)*7100 + 1600*XG+1000*XC+600*XS = 250000|#1 #4
TECHNICIAN: (10*XG+7*XC+5*XS)*4 = 250*4|#1 #4
NO OF ORDERS: 9*XG+6*XC+4*XS >=221|
XG lBound 5
XC lBound 5
XS lBound 5"
goals

# Solve problem
gp <- goalp(goals)
summary(gp)

# increasing cost by 3000 pounds and 10 technician hours per week we get more orders
library(goalp)
# Write goals to a text variable
goals <- "cost: (XG+XC+XS)*7100 + 1600*XG+1000*XC+600*XS <= 253000
TECHNICIAN: (10*XG+7*XC+5*XS) = 260
NO OF ORDERS: 9*XG+6*XC+4*XS >=0
XG lBound 5
XC lBound 5
XS lBound 5"
goals

# Solve problem
gp <- goalp(goals)
summary(gp)

#