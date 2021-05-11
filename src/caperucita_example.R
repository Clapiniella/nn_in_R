#########################
#     Implementando     #
# Redes Neuronales en R #
#########################

#install.packages("NeuralNetTools")

library(neuralnet)
library(NeuralNetTools)

# code qualities and actions
qualities <- matrix (c(1, 1, 1, 0, 0, 0,
                       0, 1, 0, 1, 1, 0,
                       1, 0, 0, 1, 0, 1), byrow = TRUE, nrow = 3)
colnames(qualities) <- c("big_ears", "big_eyes", "big_teeth", "kindly", "wrinkled", "handsome")
rownames(qualities) <- c("wolf", "grannie", "woodcutter")
qualities

actions <- matrix (c(1, 1, 1, 0, 0, 0, 0,
                     0, 0, 0, 1, 1, 1, 0,
                     0, 0, 0, 1, 0, 1, 1), byrow = TRUE, nrow = 3)
colnames(actions) <- c("run_away", "scream", "look_for_woodcutter", "kiss_on_cheek", "approach", "offer_food", "flirt_with")
rownames(actions) <- rownames(qualities)
actions


data <- cbind(qualities, actions)

# train the neural network (NN)
set.seed(123) # for reproducibility
neuralnetwork <- neuralnet(run_away + scream + look_for_woodcutter + kiss_on_cheek + approach + 
                             offer_food + flirt_with ~ 
                             big_ears + big_eyes + big_teeth + kindly + wrinkled + handsome,
                           data = data, hidden = 3, linear.output = FALSE)

# plot the NN
par_bkp <- par(mar = c(0, 0, 0, 0)) # set different margin to minimize cutoff text
plotnet(neuralnetwork, bias = FALSE)

par(par_bkp)

# which actions were learned by the net?
round(neuralnetwork$net.result[[1]])
