death = read.csv("/Users/skywalker/Downloads/DeathPenalty.csv")
table(death$Death)

##########
install.packages("caTools")
library(caTools)
split <- sample.split(death$Death, SplitRatio = 0.75)
split

##########
train <- subset(death, split = TRUE)
test <- subset(death, split = FALSE)

prop.table(table(death$Death))*100

prop.table(table(train$Death))*100

prop.table(table(test$Death))*100

##########
logit = glm(Death~., data = death, family = binomial)

summary(logit)

#evaluating the coefficients or calculating the odds ratio
exp(1.5397)
exp(1.8106)


#########
pred = predict(logit, newdata = test, type = "response")
?predict
pred[1:3]

#########
install.packages("InformationValue")
library(InformationValue)
pred1 = ifelse(pred>0.5,1,0)
table(test$Death, pred1)

sensitivity(test$Death, pred1)

specificity(test$Death, pred)

precision(test$Death, pred)
#sensitivity+specifisity-1
youdensIndex(test$Death, pred)

#choosing optimal cutoff
ones = optimalCutoff(test$Death, pred, "Ones")
ones
table(test$Death, pred>ones)

zeros = optimalCutoff(test$Death, pred, "Zeros")
zeros
table(test$Death,pred>zeros)

both = optimalCutoff(test$Death, pred, "Both")
both
table(test$Death, pred>both)

############ ROC ###########

install.packages('ROCR')
library(ROCR)

#false positive rate = 1 - specificty
#true positive rate  = sensitivity

ROCpred <- prediction(pred, test$Death)
ROCperf <- performance(ROCpred, "tpr", "fpr")
plot(ROCperf)
plot(ROCperf, colorize = T,
     print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# we want a treshold for which sensitivity and specificity is highest
# if we have a model which makes random predictions then the ROC curve of that model will 
# be along the diagonal
# of this plot. Better your model is more will its ROC curve deviate away from the diagonal
# line. Note that the area of this square is 1. The area below the diagonal is 0.5. So, if you
# have a model which makes random predictions, then the area under the ROC curve will be
# approximately equal to 0.50. If you have a model which performs better, then the ROC
# curve will deviate away from the diagonal line and its AUROC value will be closer to 1.
# This is the reason AUROC curve can used for comapring the performace of various models.

auroc = auc(test$Death, pred)
auroc #closer to 1, better the model

#------------------------------------------------------------------------------------------------
#concordance
# -A pair is concordent, if one has higher predicted probability than 0.
# -A pair is dicordent, if 0 has a higher predicted probability than 1.
# -A pair is ties, if one has same predicted probability as 0.

#percent concordance
# Concordance is defines as the ratio of number of pairs where the 1 had a higher predicted
# probability that zero in percentage terms.
# -A higher value of concordance like 60-70% indicates a better fitted model.
# -A very high value 80-90%, could suggest that the model is over-fiited.

# Goodman-Kruskels Gamma(r) = (C-D)/(C+D+T), where C=%concordance, D=%DIscordance, T=%tied
# it measures how well the model is able to distinguish b/w concordent and discordent pairs.

# SOMER'S D(D) = (C-D)/(C+D)












