exp_TARGDOL <- numeric()

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
  
  paste("RMSE is --", rmse, "Actual Sum of Expected Top 100 Donors --  $", tsum)
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

exp4 <- get_exp_targol(model2, binary_mod_with_regions, is.log = FALSE)
exp4
# 9038.84

##### outlier removed
exp5 <- get_exp_targol(model1_rmO, binary_mod_with_regions, is.log = FALSE)
exp5
# 8951.34

exp6 <- get_exp_targol(model2_rmO, binary_mod_with_regions, is.log = FALSE)
exp6
# 8951.34
