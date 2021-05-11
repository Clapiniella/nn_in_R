#########################
#     Implementando     #
# Redes Neuronales en R #
#########################
# Deep Neural Network #
#########################
# BINARY CLASSIFICATION #
#########################

#install.packages("ISLR")

library(neuralnet)
library(ISLR) #data

data = College
head(data)

max_data = apply(data[,2:18], 2, max) ##maximum value
min_data = apply(data[,2:18], 2, min) ##minimum value

##normalize data
data_scaled = scale(data[,2:18], center = min_data, scale = max_data - min_data)

Private = as.numeric(College$Private)-1
data_scaled = cbind(Private, data_scaled)
head(data_scaled)

index = sample(1:nrow(data), round(0.70*nrow(data))) #create 70:30 split 
#train_set
train_data <- as.data.frame(data_scaled[index,])
#test_set
test_data <- as.data.frame(data_scaled[-index,])

n = names(train_data)
#we first recover all the variable names using the names function
head(n)

f <- as.formula(paste("Private~", paste(n[!n %in% "Private"], collapse = " + ")))

### simple deep neural network

deep_net = neuralnet(f, data = train_data, hidden= c(5,3), linear.output = F)
#we set the hidden layer to contain the vector (5,3)
#which corresponds to two hidden levels with respective
#five neurons in the first hidden layer and 
#three neurons in the second

plot(deep_net)

predicted_data <- compute(deep_net, test_data[,2:18])
print(head(predicted_data$net.result))

predicted_data$net.result <- sapply(predicted_data$net.result, round, digits = 0)

# use the sapply() function to round these off to either zero or one class

table(test_data$Private, predicted_data$net.result) #confussion matrix

#oa = (60+157)/(60+157+7+1)
#install.packages("e1071")
confusionMatrix(factor(test_data$Private), factor(predicted_data$net.result)) #caret





