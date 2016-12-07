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

donation$SEXC <- ifelse(donation$SEX == "C", 1, 0)
donation$SEXF <- ifelse(donation$SEX == "F", 1, 0)
donation$SEXM <- ifelse(donation$SEX == "M", 1, 0)
donation$SEXU <- ifelse(donation$SEX == "U", 1, 0)
donation$SEXB <- ifelse(donation$SEX == "B", 1, 0)


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


donation$northeast <- northeast
donation$midwest <- midwest
donation$south <- south
donation$west <- west
donation$territories <- territories

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

training_copy$SEXC <- ifelse(training_copy$SEX == "C", 1, 0)
training_copy$SEXF <- ifelse(training_copy$SEX == "F", 1, 0)
training_copy$SEXM <- ifelse(training_copy$SEX == "M", 1, 0)
training_copy$SEXU <- ifelse(training_copy$SEX == "U", 1, 0)
training_copy$SEXB <- ifelse(training_copy$SEX == "B", 1, 0)

testing_copy$SEXC <- ifelse(testing_copy$SEX == "C", 1, 0)
testing_copy$SEXF <- ifelse(testing_copy$SEX == "F", 1, 0)
testing_copy$SEXM <- ifelse(testing_copy$SEX == "M", 1, 0)
testing_copy$SEXU <- ifelse(testing_copy$SEX == "U", 1, 0)
testing_copy$SEXB <- ifelse(testing_copy$SEX == "B", 1, 0)



training_copy$northeast <- ifelse(training_copy$regions == 1, 1, 0)
training_copy$midwest <- ifelse(training_copy$regions == 2, 1, 0)
training_copy$south <- ifelse(training_copy$regions == 3, 1, 0)
training_copy$west <- ifelse(training_copy$regions == 4, 1, 0)
training_copy$territories <- ifelse(training_copy$regions == 5, 1, 0)

testing_copy$northeast <- ifelse(testing_copy$regions == 1, 1, 0)
testing_copy$midwest <- ifelse(testing_copy$regions == 2, 1, 0)
testing_copy$south <- ifelse(testing_copy$regions == 3, 1, 0)
testing_copy$west <- ifelse(testing_copy$regions == 4, 1, 0)
testing_copy$territories <- ifelse(testing_copy$regions == 5, 1, 0)


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

############################## BINARY MODEL FITTING ###############################################################################


binary_mod <- glm(TARGDOL_bin~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONF + CNMONL , data = training_copy, family = "binomial")
summary(binary_mod)


binary_mod_with_regions <- glm(TARGDOL_bin~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONF + CNMONL + regions , data = training_copy, family = "binomial")
summary(binary_mod_with_regions)


binary_mod3 <- glm(TARGDOL_bin ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONF + CNMONL + regions + out_of_last_three, data = training_copy, family = "binomial")
summary(binary_mod3)

binary_mod4 <- glm(TARGDOL_bin~ CNDOL1 + CNTRLIF  + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONF + CNMONL , data = training_copy, family = "binomial")

binary_mod5 <- glm(TARGDOL_bin~ CNDOL1 + CNTRLIF  + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONL, data = training_copy, family = "binomial")


###################### CALCULATE AUC #########################

library(pROC)

plot.roc(training_copy$TARGDOL_bin, binary_mod$fitted.values, xlab="1-Specificity")
### AUC = 0.7116

plot.roc(training_copy$TARGDOL_bin, binary_mod_with_regions$fitted.values, xlab="1-Specificity")
### AUC = 0.7119

plot.roc(training_copy$TARGDOL_bin, binary_mod3$fitted.values, xlab="1-Specificity")
### AUC = 0.7105

plot.roc(training_copy$TARGDOL_bin, binary_mod4$fitted.values)
### AUC = 0.7116

plot.roc(training_copy$TARGDOL_bin, binary_mod5$fitted.values)
### AUC = 0.7046


######################## SCRATCH ########################################################################

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

donate_trim <- donation[,c(1,2,4,5,11,12,13,14,15,17,19,20,24)]

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

# get observations where TARGDOL was over 0
training_over0 <- subset(training_copy, training_copy$TARGDOL > 0)
testing_over0 <- subset(testing_copy, testing_copy$TARGDOL > 0)

training_over0$SEXC <- ifelse(training_over0$SEX == "C", 1, 0)
training_over0$SEXF <- ifelse(training_over0$SEX == "F", 1, 0)
training_over0$SEXM <- ifelse(training_over0$SEX == "M", 1, 0)
training_over0$SEXU <- ifelse(training_over0$SEX == "U", 1, 0)
training_over0$SEXB <- ifelse(training_over0$SEX == "B", 1, 0)

testing_over0$SEXC <- ifelse(testing_over0$SEX == "C", 1, 0)
testing_over0$SEXF <- ifelse(testing_over0$SEX == "F", 1, 0)
testing_over0$SEXM <- ifelse(testing_over0$SEX == "M", 1, 0)
testing_over0$SEXU <- ifelse(testing_over0$SEX == "U", 1, 0)
testing_over0$SEXB <- ifelse(testing_over0$SEX == "B", 1, 0)


training_over0$northeast <- ifelse(training_over0$regions == 1, 1, 0)
training_over0$midwest <- ifelse(training_over0$regions == 2, 1, 0)
training_over0$south <- ifelse(training_over0$regions == 3, 1, 0)
training_over0$west <- ifelse(training_over0$regions == 4, 1, 0)
training_over0$territories <- ifelse(training_over0$regions == 5, 1, 0)

testing_over0$northeast <- ifelse(testing_over0$regions == 1, 1, 0)
testing_over0$midwest <- ifelse(testing_over0$regions == 2, 1, 0)
testing_over0$south <- ifelse(testing_over0$regions == 3, 1, 0)
testing_over0$west <- ifelse(testing_over0$regions == 4, 1, 0)
testing_over0$territories <- ifelse(testing_over0$regions == 5, 1, 0)


model1 <- lm(TARGDOL ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONF + CNMONL + regions + out_of_last_three , data = training_over0)
summary(model1)
# r2 = 0.6148
plot(model1)
preds1 <- predict(model1, newdata = testing_over0)
SSE1 <- sum((testing_over0$TARGDOL - preds1)^2)
SST <- sum((testing_over0$TARGDOL - mean(testing_over0$TARGDOL))^2)
1 - SSE1/SST
# 0.4785665

sstep(model1)

model2 <- lm(TARGDOL ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + CNMON1 + CNMONF + CNMONL + out_of_last_three , data = training_over0)
summary(model2)
preds2 <- predict(model2, newdata = testing_over0)
SSE2 <- sum((testing_over0$TARGDOL - preds2)^2)
1 - SSE2/SST
# 0.4785689

# based on step regression of model1
model3 <- lm(TARGDOL ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + SEX + CNMONF + CNMONL + out_of_last_three, data = training_over0)
summary(model3)
preds3 <- predict(model3, newdata = testing_over0)
SSE3 <- sum((testing_over0$TARGDOL - preds3)^2)
1 - SSE3/SST
#  0.4786364


model4 <- lm(TARGDOL ~ CNDOL1:CONLARG + CNTRLIF + CONTRFST + CNTMLIF + SEX + CNMONF + CNMONL + out_of_last_three, data = training_over0)
summary(model4)
preds4 <- predict(model4, newdata = testing_over0)
SSE4 <- sum((testing_over0$TARGDOL - preds4)^2)
1 - SSE4/SST
# 0.5250738

model5 <- lm(TARGDOL ~ CNDOL1:CONLARG + CNDOL1 + CONLARG + CNTRLIF + CONTRFST + CNTMLIF:CNMONF + SEX + CNMONL + out_of_last_three, data = training_over0)
summary(model5)
preds5 <- predict(model5, newdata = testing_over0)
SSE5 <- sum((testing_over0$TARGDOL - preds5)^2)
1 - SSE5/SST
# 0.6062119

model6 <- lm(TARGDOL ~ CNDOL1:CONLARG + CNDOL1 + CONLARG + CNTRLIF + CONTRFST + CNTMLIF:CNMONF + CNTMLIF + CNMONF +SEX + CNMONL + out_of_last_three, data = training_over0)
summary(model6)
preds6 <- predict(model6, newdata = testing_over0)
SSE6 <- sum((testing_over0$TARGDOL - preds6)^2)
1 - SSE6/SST
# 0.6084811


model8 <- lm(TARGDOL ~ CNDOL1:CONLARG + CNDOL1:CONLARG + CNTMLIF:CNMONL + CNTRLIF:out_of_last_three + CNDOL1 + CONLARG + CNTRLIF + CONTRFST + CNTMLIF + CNMONF + CNMONL + out_of_last_three, data = training_over0)
summary(model8)
preds8 <- predict(model8, newdata = testing_over0)
SSE8 <- sum((testing_over0$TARGDOL - preds8)^2)
1 - SSE8/SST



# dummy variables for sex and num of previous donations
B <- ifelse(training_over0$SEX == "B", 1, 0)
C <- ifelse(training_over0$SEX == "C", 1, 0)
sex_F <- ifelse(training_over0$SEX == "F", 1, 0)
M <- ifelse(training_over0$SEX == "M", 1, 0)
U <- ifelse(training_over0$SEX == "U", 1, 0)

one_don <- ifelse(training_over0$out_of_last_three == 1, 1, 0)
two_don <- ifelse(training_over0$out_of_last_three == 2, 1, 0)
three_don <- ifelse(training_over0$out_of_last_three == 3, 1, 0)

########### identify outliers/multicollinearity issues ###########

x_matrix <- matrix(c(rep(1,nrow(training_over0)), training_over0$CNDOL1, training_over0$CNTRLIF, training_over0$CONLARG, training_over0$CONTRFST, training_over0$CNTMLIF, training_over0$CNMON1,training_over0$CNMONF,training_over0$CNMONL), ncol = 9)

h_matrix <- x_matrix %*% (solve((t(x_matrix) %*% x_matrix))) %*% t(x_matrix)

cor(data.frame(CNDOL1 = training_over0$CNDOL1, CNTRLIF = training_over0$CNTRLIF, 
               CONLARG = training_over0$CONLARG, CONTRFST = training_over0$CONTRFST, 
               CNTMLIF = training_over0$CNTMLIF, CNMON1 = training_over0$CNMON1, 
               CNMONF = training_over0$CNMONF, CNMONL = training_over0$CNMONL)) 


######## make vector of standardized resids

mse <- mean(model2$residuals^2)
# square root of mse
s <- sqrt(mse)
standardized_resids <- numeric()

for(i in 1:nrow(training_over0)) {
  num <- model2$residuals[i]
  denom <- s * sqrt(1 - diag(h_matrix)[i])
  standardized_resids[i] <- num/denom
}

##

don_outliers <- numeric(nrow(training_over0))
# if ei# > 3 is greater than critical, record which obs. # is an outlier
# else, mark NA 
for(i in 1:nrow(training_over0)) {if(abs(standardized_resids[i]) > 4) { don_outliers[i] <- i } else {don_outliers[i] <- NA}}
length(which(!is.na(don_outliers)))

training_over0_rm <- data.frame(TARGDOL = numeric(0), CNDOL1 = numeric(0), CNTRLIF = numeric(0), CONLARG = numeric(0), 
                             CONTRFST = numeric(0), CNTMLIF = numeric(0), CNMON1 = numeric(0),
                             CNMONF = numeric(0), CNMONL = numeric(0))

for(i in 1:nrow(training_over0)) {
  # if not marked as outlier
  if(is.na(don_outliers[i])) {
    newrow = data.frame(TARGDOL = training_over0$TARGDOL[i],
                        CNDOL1 = training_over0$CNDOL1[i],
                        CNTRLIF = training_over0$CNTRLIF[i],
                        CONLARG = training_over0$CONLARG[i],
                        CONTRFST = training_over0$CONTRFST[i],
                        CNTMLIF = training_over0$CNTMLIF[i],
                        CNMON1 = training_over0$CNMON1[i],
                        CNMONF = training_over0$CNMONF[i],
                        CNMONL = training_over0$CNMONL[i])
    training_over0_rm <- rbind(training_over0_rm, newrow)
  }
  
}

############################# outliers part 2 #######################

# CNDOL1:CONLARG + CNDOL1 + CONLARG + CNTRLIF + CONTRFST + 
# CNTMLIF:CNMONF + CNTMLIF + CNMONF +SEX + CNMONL + out_of_last_three

x_matrix2 <- model.matrix(model6)

h_matrix2 <- x_matrix2 %*% (solve((t(x_matrix2) %*% x_matrix2))) %*% t(x_matrix2)

mse2 <- mean(model6$residuals^2)
# square root of mse
s2 <- sqrt(mse2)
standardized_resids2 <- numeric()

for(i in 1:nrow(training_over0)) {
  num <- model6$residuals[i]
  denom <- s2 * sqrt(1 - diag(h_matrix2)[i])
  standardized_resids2[i] <- num/denom
}

##############

don_outliers2 <- numeric(nrow(training_over0))
# if ei# > 3 is greater than critical, record which obs. # is an outlier
# else, mark NA 
for(i in 1:nrow(training_over0)) {if(abs(standardized_resids2[i]) > 3) { don_outliers2[i] <- i } else {don_outliers2[i] <- NA}}
length(which(!is.na(don_outliers2)))

training_over0_rm2 <- data.frame(TARGDOL = numeric(0), CNDOL1_CONLARG = numeric(0), CNDOL1 = numeric(0), CONLARG = numeric(0), CNTRLIF = numeric(0), 
                                CONTRFST = numeric(0), CNTMLIF_CNMONF = numeric(0), CNTMLIF = numeric(0), CNMONF = numeric(0), SEXC = numeric(0), SEXF = numeric(0),
                                SEXM = numeric(0), SEXU = numeric(0), CNMONL = numeric(0), out_of_last_three = numeric(0))

for(i in 1:nrow(training_over0)) {
  # if not marked as outlier
  if(is.na(don_outliers2[i])) {
    newrow = data.frame(TARGDOL = training_over0$TARGDOL[i],
                        CNDOL1_CONLARG = training_over0$CNDOL1[i] * training_over0$CONLARG[i],
                        CNDOL1 = training_over0$CNDOL1[i],
                        CONLARG = training_over0$CONLARG[i],
                        CNTRLIF = training_over0$CNTRLIF[i],
                        CONTRFST = training_over0$CONTRFST[i],
                        CNTMLIF_CNMONF = training_over0$CNTMLIF[i] * training_over0$CNMONF[i],
                        CNTMLIF = training_over0$CNTMLIF[i],
                        CNMONF = training_over0$CNMONF[i],
                        SEXC = ifelse(training_over0$SEX[i] == "C", 1, 0),
                        SEXF = ifelse(training_over0$SEX[i] == "F", 1, 0),
                        SEXM = ifelse(training_over0$SEX[i] == "M", 1, 0),
                        SEXU = ifelse(training_over0$SEX[i] == "U", 1, 0),
                        CNMONL = training_over0$CNMONL[i],
                        out_of_last_three = training_over0$out_of_last_three[i])
    training_over0_rm2 <- rbind(training_over0_rm2, newrow)
  }
  
}






model2_rm <- lm(TARGDOL ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + CNMON1 + CNMONF + CNMONL, data = training_over0_rm)
summary(model2_rm)
preds2_rm <- predict(model2_rm, newdata = testing_over0)
SSE2_rm <- sum((testing_over0$TARGDOL - preds2_rm)^2)
1 - SSE2_rm/SST
# 0.6020157


model5_rm <- lm(TARGDOL ~ CNDOL1:CONLARG + CNDOL1 + CONLARG + CNTRLIF + CONTRFST + CNTMLIF:CNMONF + CNMONL, data = training_over0_rm)
summary(model5_rm)
preds5_rm <- predict(model5_rm, newdata = testing_over0)
SSE5_rm <- sum((testing_over0$TARGDOL - preds5_rm)^2)
1 - SSE5_rm/SST
## 0.5865041

model5_rm2 <- lm(TARGDOL ~ CNDOL1:CONLARG + CNDOL1 + CONLARG + CNTRLIF + CONTRFST + CNTMLIF:CNMONF + CNMONL, data = training_over0_rm2)
summary(model5_rm2)
preds5_rm2 <- predict(model5_rm2, newdata = testing_over0)
SSE5_rm2 <- sum((testing_over0$TARGDOL - preds5_rm2)^2)
1 - SSE5_rm2/SST
##  0.5984846 -- removing outliers with standard resids > 4
## 0.5972769 --- removing outliers with standard resids > 3

model6_rm2 <- lm(TARGDOL ~ CNDOL1:CONLARG + CNDOL1 + CONLARG + CNTRLIF + CONTRFST + CNTMLIF:CNMONF + CNTMLIF + CNMONF + SEXC + SEXF + SEXM + SEXU + CNMONL + out_of_last_three, data = training_over0_rm2)
summary(model6_rm2)
preds6_rm2 <- predict(model6_rm2, newdata = testing_over0)
SSE6_rm2 <- sum((testing_over0$TARGDOL - preds6_rm2)^2)
1 - SSE6_rm2/SST
##  0.6044021 --- removing outliers with standard resids > 4
## 0.6031412 --- removing outliers with standard resids > 3












#################################### bad attempts######################

log_model1 <- lm(log(TARGDOL) ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONF + CNMONL + regions + out_of_last_three , data = training_over0)
summary(log_model1)
## r2 = 0.4035
plot(log_model1)

training_over0_rmO <- training_over0[training_over0$TARGDOL < 1500,]

model1_rmO <- lm(TARGDOL ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + SEX + CNMON1 + CNMONF + CNMONL + regions + out_of_last_three , data = training_over0_rmO)
summary(model1_rmO)
plot(model1_rmO)
# r2 = 0.6148

model2_rmO <- lm(TARGDOL ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNTMLIF + CNMON1 + CNMONF + CNMONL  + out_of_last_three , data = training_over0_rmO)
summary(model2_rmO)
# r2 = 0.6143, adj = 0.6141


####################### E[Y] ######################

exp_TARGDOL <- numeric()

prob <- nrow(training_over0)/nrow(donation)

get_exp_targol <- function(mr.model, bin.model, is.log = FALSE) {
  
  ## predict donations using MULTIPLE REGRESSION MODEL
  pred.vals <- predict(mr.model, newdata = testing_copy) 
  if(is.log == TRUE) {
    pred.vals <- exp(pred.vals)
  }
  # change negative donations to 0
  pred.vals[pred.vals < 0] <- 0
  
  ## predict P(y > 0) using LOGISTIC REGRESSION MODEL
  binary_pred.vals <- predict(bin.model, newdata = testing_copy, type = "response")
  
  # calculate 
  for(i in 1:length(pred.vals)) {
    exp_TARGDOL[i] <- pred.vals[i] * binary_pred.vals[i]
  }
  
  copy1 <- testing_copy
  copy1$exp_TARGDOL <- exp_TARGDOL
  
  # calculate RMSE
  rmse <- sqrt(sum((copy1$exp_TARGDOL - copy1$TARGDOL)^2)/nrow(testing_copy))
  
  copy1 <- copy1[order(-copy1$exp_TARGDOL),]
  
  top1000 <- head(copy1, 1000)
  # total actual donation
  tsum <- sum(top1000$TARGDOL)
  
  paste("RMSE is --", rmse, "Actual Sum of Expected Top 1000 Donors --  $", tsum)
}

exp1 <- get_exp_targol(log_model1, binary_mod, is.log = TRUE)
exp1
# 8580.66

exp2 <- get_exp_targol(log_model1, binary_mod_with_regions, is.log = TRUE)
exp2
# 8497.66

exp3 <- get_exp_targol(model2, binary_mod, is.log = FALSE)
exp3
# 9082.84

exp4 <- get_exp_targol(model2, binary_mod, is.log = FALSE)
exp4
# 9082.84
# 9058.84 with regions

exp5 <- get_exp_targol(model2_rm, binary_mod, is.log = FALSE)
exp5
# 9157.84
# 9097 with regions

exp6 <- get_exp_targol(model2_rm, binary_mod, is.log = FALSE)
exp6
# 9126.84

########### when you remove outliers wit standard resids > 4
exp7 <- get_exp_targol(model2_rm, binary_mod, is.log = FALSE)
exp7
#### 9157.84

exp8 <- get_exp_targol(model5, binary_mod, is.log = FALSE)
exp8
#### 9009.34

exp9 <- get_exp_targol(model6, binary_mod_with_regions, is.log = FALSE)
exp9
#### 9056.34
## 9081.34 with regions

exp10 <- get_exp_targol(model5_rm, binary_mod_with_regions, is.log = FALSE)
exp10
#### 9270.84
## 9334.84 with regions


exp_5rm2 <- get_exp_targol(model5_rm2, binary_mod_with_regions, is.log = FALSE)
exp_5rm2
### 9133
### 9170 with regions


exp_6rm2 <- get_exp_targol(model6_rm2, binary_mod_with_regions, is.log = FALSE)
exp_6rm2
#### 8999.839988705
##### 9079 with regions