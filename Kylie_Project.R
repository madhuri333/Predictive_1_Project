setwd("/Users/Kylie/Desktop/Northwestern/Fall2016/MSiA401_Predictive_Analytics_I")
library(VIF)
donation <- read.csv("donation data.csv", header = T)
# save raw copy
donation_clean <- donation
# copy to mess with
donation_copy <- donation
codes <- read.csv("dmef1code.csv", header = T)
str(donation)
head(donation, 1)


donation$TARGDOL_bin <- ifelse(donation$TARGDOL > 0, 1 ,0)


############################################## GEOGRAPHIC REGIONS ###################################################################

# make state regions
# REGION 1 - Northeast
# REGION 2 - Midwest
# REGION 3 - South
# REGION 4 - West
# REGION 5 - US territories
regions <- numeric()
northeast <- numeric()
midwest <- numeric()
south <- numeric()
west <- numeric()
territories <- numeric()
statecodes <- donation$STATCODE
for(i in 1:length(donation$STATCODE)) {
  current <- statecodes[i]
  # REGION 1
  if(current == "CT" || current == "ME" || current == "MA" || current == "NH" || current == "RI" || current == "VT" || current == "NJ" ||
          current == "NY" || current == "PA") {
            regions[i] <- 1
            northeast[i] <- 1
            midwest[i] <- 0
            south[i] <- 0 
            west[i] <- 0
            territories[i] <- 0
  }
  # REGION 2
  else if(current == "IL" || current == "IN" || current == "MI" || current == "OH" || current == "WI" || current == "IA" || 
          current == "KS" || current == "MN" || current == "MO" || current == "NE" || current == "ND" || current == "SD") {
            regions[i] <- 2
            northeast[i] <- 0
            midwest[i] <- 1
            south[i] <- 0 
            west[i] <- 0
            territories[i] <- 0
  }
  # REGION 3
  else if(current == "DE" || current == "FL" || current == "GA" || current == "MD" || current == "NC" || current == "SC" || 
          current == "VA" || current == "DC" || current == "WV" || current == "AL" || current == "KY" || current == "MS" ||
          current == "TN" || current == "AR" || current == "LA" || current == "OK" || current == "TX") {
           regions[i] <- 3
           northeast[i] <- 0
           midwest[i] <- 0
           south[i] <- 1
           west[i] <- 0
           territories[i] <- 0
  }
  # REGION 4
  else if(current == "AZ" || current == "CO" || current == "ID" || current == "MT" || current == "NV" || current == "NM" || 
          current == "UT" || current == "WY" || current == "AK" || current == "CA" || current == "HI" || current == "OR" || current == "WA") {
          regions[i] <- 4
          northeast[i] <- 0
          midwest[i] <- 0
          south[i] <- 0 
          west[i] <- 1
          territories[i] <- 0
  }
  # REGION 5
  else if(current == "AA" || current == "AE" || current == "AP" || current == "AS" || current == "DI" || current == "GU" || 
          current == "MP" || current == "MW" || current == "PR" || current == "VI") {
           regions[i] <- 5
           northeast[i] <- 0
           midwest[i] <- 0
           south[i] <- 0 
           west[i] <- 0
           territories[i] <- 1
   }
}


donation$regions <- regions
donation$regions <- as.factor(donation$regions)

donation$regions <- relevel(donation$regions, ref="2")

##################### ADD FACTOR VAR FOR # DONATIONS #######################################

## Update main donation DF
donation$one_donation <- ifelse(is.na(donation$CNCOD2), 1, 0)
#donation$two_donations <- ifelse((!is.na(donation$CNCOD2) && is.na(donation$CNCOD3)), 1 ,0)
donation$three_donations <- ifelse(!is.na(donation$CNCOD3), 1, 0)

out_of_last_three <- numeric()
for(i in 1:nrow(donation)) {
  if(donation$one_donation[i] == 1) {
    out_of_last_three[i] <- 1
  } 
  else if(donation$three_donations[i] == 1) {
    out_of_last_three[i] <- 3
  }
  else {
    out_of_last_three[i] <- 2
  }
}
donation$out_of_last_three <- as.factor(out_of_last_three)
donation$out_of_last_three <- relevel(donation$out_of_last_three, ref="1")


######################################################## TRAINING/TESTING ################################################################

## every 3rd observation is in testing, the rest is training
test_ind <- seq(3, nrow(donation),3)
testing <- donation[test_ind,]
training <- donation[-test_ind,]

testing_copy <- testing
training_copy <- training


############################################################ EDA #############################################################################

# subset of data, people who only contributed once
one_donation <- subset(donation, is.na(donation$CNCOD2))

# subset of data, people who contributed exactly twice
has_two <- subset(donation, !is.na(donation$CNCOD2))
two_donations <- subset(has_two, is.na(has_two$CNCOD3))

# subset of data, people who contributed three time
three_donations <- subset(donation, !is.na(donation$CNCOD3))

# gender breakdown doesn't really change for different donation instances
table(one_donation$SEX)/sum(table(one_donation$SEX))
table(two_donations$SEX)/sum(table(two_donations$SEX))
table(three_donations$SEX)/sum(table(three_donations$SEX))

nrow(one_donation[one_donation$TARGDOL > 0,]) / nrow(one_donation)

nrow(two_donations[two_donations$TARGDOL > 0,]) / nrow(two_donations)

nrow(three_donations[three_donations$TARGDOL > 0,]) / nrow(three_donations)

############################## MODEL FITTING ###############################################################################


binary_mod <- glm(TARGDOL_bin~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONF + CNMONL , data = training_copy, family = "binomial")
summary(binary_mod)
# correlations between predictor vars without SEX
cor(data.frame(CNDOL1 = training_copy$CNDOL1, CNTRLIF = training_copy$CNTRLIF, 
                 CONLARG = training_copy$CONLARG, CONTRFST = training_copy$CONTRFST, 
                 CNTMLIF = training_copy$CNTMLIF, CNMON1 = training_copy$CNMON1, 
               CNMONF = training_copy$CNMONF, CNMONL = training_copy$CNMONL))

predicted_prob <- predict(binary_mod, newdata = testing_copy, type = "response")

donation_prediction <- numeric()
for(i in 1:nrow(testing_copy)) {
    if(predicted_prob[i] > 0.45) { donation_prediction[i] <- 1} else {donation_prediction[i] <- 0}
}

tab1 <- table(donation_prediction, testing_copy$TARGDOL_bin)
sum(diag(tab1))/sum(tab1)
# 75.04% overall classification rate

library(pROC)
plot.roc(training_copy$TARGDOL_bin, binary_mod$fitted.values, xlab="1-Specificity")

##################### ADD REGIONS TO BINARY MOD #####################################################

binary_mod_with_regions <- glm(TARGDOL_bin~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONF + CNMONL + regions , data = training_copy, family = "binomial")
summary(binary_mod_with_regions)
predicted_prob_regions <- predict(binary_mod_with_regions, newdata = test_regions, type = "response")

donation_prediction_regions <- numeric()
for(i in 1:nrow(test_regions)) {
  if(predicted_prob_regions[i] > 0.5) { donation_prediction_regions[i] <- 1} else {
    donation_prediction_regions[i] <- 0}
}

tab2 <- table(donation_prediction_regions, test_regions$TARGDOL_bin)
sum(diag(tab2))/sum(tab2)
# 74.90% overall classification rate


binary_mod3 <- glm(TARGDOL_bin ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONF + CNMONL + regions + out_of_last_three, data = training_copy, family = "binomial")
summary(binary_mod3)

predicted_probs3 <- predict(binary_mod3, newdata = testing_copy, type = "response")

donation_prediction3 <- numeric()
for(i in 1:nrow(testing_copy)) {
  if(predicted_probs3[i] > 0) { donation_prediction3[i] <- 1} 
  else {
    donation_prediction3[i] <- 0}
}

tab3 <- table(donation_prediction3, testing_copy$TARGDOL_bin)
sum(diag(tab3))/sum(tab3)
# 75.19% overall ---- with p* = 0.50
## 75.47 % overall classification


######################### OPTIMAL P STAR FUNCTION #############################

optimal_pStar <- function(mod, test) {

    P_stars <- seq(0, 1, by = 0.05)
    overall_RATE <- numeric()
    pred_vals <- predict(mod, newdata = test, type = "response") 

    for(i in 1:length(P_stars)) {
      
      decision <- numeric()
      
      for(j in 1:nrow(test)) {
        
          if(pred_vals[j] > P_stars[i]) { 
              decision[j] <- 1
          } 
          else {
              decision[j] <- 0
          }
      }

        TAB <- table(decision, test$TARGDOL_bin)
        if(dim(TAB)[1] == 1) {
          overall_RATE[i] <- TAB[2]/sum(TAB)
        } else {
          overall_RATE[i] <- sum(diag(TAB))/sum(TAB)
        }
    }
    result_DF <- data.frame(p_stars = P_stars, class_rate = overall_RATE)
    return(result_DF)
}

### model 1 
mod1_rates <- optimal_pStar(binary_mod, testing_copy)
mod1_rates[mod1_rates$class_rate == max(mod1_rates$class_rate),]
plot(mod1_rates$p_stars, mod1_rates$class_rate, type = "l")

### model 2
mod2_rates <- optimal_pStar(binary_mod_with_regions, testing_copy)
mod2_rates[mod2_rates$class_rate == max(mod2_rates$class_rate),]
plot(mod2_rates$p_stars, mod2_rates$class_rate, type = "l")

### model 3 - regions + out_of_three
mod3_rates <- optimal_pStar(binary_mod3, testing_copy)
mod3_rates[mod3_rates$class_rate == max(mod3_rates$class_rate),]
plot(mod3_rates$p_stars, mod3_rates$class_rate, type = "l")


############################### MULTIPLE REGRESSION MODEL #################################################

training_over0 <- subset(training_copy, training_copy$TARGDOL > 0)
testing_over0 <- subset(testing_copy, testing_copy$TARGDOL > 0)

model1 <- lm(TARGDOL ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONF + CNMONL + regions + out_of_last_three , data = training_over0)
summary(model1)


model1_preds <- predict(model1, newdata = testing_over0)
diffs1 <- model1_preds - testing_over0$TARGDOL
mean(model1_preds)
# mean squared error
sum(diffs^2) / nrow(testing_over0)
plot(model1)



log_model1 <- lm(log(TARGDOL) ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONF + CNMONL + regions + out_of_last_three , data = training_over0)
summary(log_model1)
plot(log_model1)
log_model1_preds <- predict(log_model1, newdata = testing_over0)
diffs_log1 <- log_model1_preds - testing_over0$TARGDOL
# mean squared error
sum(diffs_log1^2) / nrow(testing_over0)


model2 <- lm(TARGDOL ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + CNMON1 + CNMONF + CNMONL + regions + out_of_last_three , data = training_over0)
summary(model2)

#predicting
model2_preds <- predict(model2, newdata = testing_over0)
diffs2 <- model2_preds - testing_over0$TARGDOL
mean(model2_preds)
# mean squared error
sum(diffs2^2) / nrow(testing_over0)




