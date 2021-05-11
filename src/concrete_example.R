#########################
#     Implementando     #
# Redes Neuronales en R #
#########################
# MLP - REGRESSION  #
#####################

#install.packages("readxl")

require(caret)
require(neuralnet)
require(readxl)

con = read_xls("data/Concrete_Data.xls")

head(con)

colnames(con) = c("cement", "slag", "ash", "water", "superplastic", "coarseagg", "fineagg", "age", "strength")

con = as.data.frame(con)

head(con)

# normalize all variables
# all values are between 0 to 1
# standard normalization function

normal = function(x){
  return((x-min(x))/(max(x)-min(x)))}

# lapply apply normal funtion to every column
con_norm = as.data.frame(lapply(con, normal))

head(con_norm)

#pre-process
#caret for pre-processing
trainIndex = createDataPartition(con$strength,p = .75, list = F)
train_set = con[trainIndex,]
test_set = con[-trainIndex,]

#simple ANN with only a single hidden neuron

concrete_model <- neuralnet(formula = strength ~ cement + slag + ash +
                              water + superplastic + coarseagg + fineagg + age,
                            data = train_set)

plot(concrete_model)

con_r = compute(concrete_model, test_set[1:8])
#implement nn in the testing set

predicted_strength = con_r$net.result

srmse = sqrt(mean((predicted_strength - test_set$strength)^2))

##IMPROVE THE MODEL
##add more neurons, no more layers

# simple ANN with only 5 hidden neurons 
# more neurons fits better complex and nonlinear data, but not huge number of neurons

concrete_model2 = neuralnet(formula = strength ~ cement + slag + ash +
                              water + superplastic + coarseagg + fineagg + age,
                            data = train_set, hidden = 5)

plot(concrete_model2)

con_r2 = compute(concrete_model2, test_set[1:8])

predicted_strength2 = con_r2$net.result

srmse2 = sqrt(mean((predicted_strength2 - test_set$strength)^2))
#no improvements... 


