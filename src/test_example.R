#########################
#     Implementando     #
# Redes Neuronales en R #
################################
# MLP - BINARY CLASSIFICATION  #
################################

# install package
#install.packages("neuralnet")

# creating training data set

TKS = c(20,10,30,20,80,30)
CSS = c(90,20,40,50,50,80)
Placed = c(1,0,0,0,1,1)

# Here, you will combine multiple columns or features into a single set of data

df = data.frame(TKS, CSS, Placed)

#load library

require(neuralnet)

# lets build a NN classifier model using the neuralnet library

nn = neuralnet(Placed~TKS+CSS, data = df, hidden = 3, act.fct = "logistic", linear.output = FALSE)

#######################################################################
#   Placed~TKS+CSS, Placed is label annd TKS and CSS are features.    #
#   hidden=3: represents single layer with 3 neurons respectively.    #
#   act.fct = "logistic" used for smoothing the result.               #
#   linear.ouput=FALSE: set FALSE for apply act.fct otherwise TRUE    #
#######################################################################

# plot nn
plot(nn)


# creating test set

TKS=c(30,40,85)
CSS=c(85,50,40)
test=data.frame(TKS,CSS)

## Prediction using neural network

Predict=compute(nn,test)

Predict$net.result


# Converting probabilities into binary classes setting threshold level 0.5

prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred


# how they work?
# https://playground.tensorflow.org/#activation=tanh&batchSize=10&dataset=circle&regDataset=reg-plane&learningRate=0.03&regularizationRate=0&noise=0&networkShape=&seed=0.14894&showTestData=false&discretize=false&percTrainData=50&x=true&y=false&xTimesY=false&xSquared=false&ySquared=false&cosX=false&sinX=false&cosY=false&sinY=false&collectStats=false&problem=classification&initZero=false&hideText=false

