#The following data manipulations were performed in Excel:
#Contribution codes and solicitation codes were imputed according to provided table
#CNDOL2 and CNDOL3 missing values imputed as 0
#States assigned to 1 of 9 regions as follows (Bureau of Economic Analysis)
##Region 1 - New England - CT, ME, MA, NH, RI, VT
##Region 2 - Mideast - DE, DC, MD, NJ, NY, PA
##Region 3 - Great Lakes - IL, IN, MI, OH, WI
##Region 4 - Plains - IA, KS, MN, MO, NE, ND, SD
##Region 5 - Southeast - AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV
##Region 6 - Southwest - AZ, NM, OK, TX
##Region 7 - Rocky Mountain - CO, ID, MT, UT, WY
##Region 8 - Far West - AK, CA, HI, NV, OR, WA
##Region 9 - Territories - All others
#Added a column TARGDOL_BIN, where =0 if TARGDOL=0, otherwise =1

#Missing values are all in CNDAT2, CNDAT3, CNMON2, CNMON3, CNCOD2, CNCOD3, SLCOD2, SLCOD3
summary(donation.data.cc)

#create training set and testing set
every3 <- seq(3,99200,3)
testset <- donation.data.cc[every3,]
trainset <- donation.data.cc[-every3,]

#Examine which variables have interactions
#CNDOL1 and CONLARG highly correlated (since many people only donate once, highest/latest contribution is only contribution)
#CNDOL1 and CNTRLIF
#CNDOL1 and CONTRFST
#CNTRLIF and CONLARG
#CNTRLIF and CNTMLIF
#CNTRLIF and CNMONF
#CONLARG and CONTRFST

trainset$CNTMLIF <- as.numeric(trainset$CNTMLIF)
trainset$CNDAT1 <- as.numeric(trainset$CNDAT1)
trainset$CNMON1 <- as.numeric(trainset$CNMON1)
trainset$CNMONF <- as.numeric(trainset$CNMONF)
trainset$CNMONL <- as.numeric(trainset$CNMONL)
numericvars <- c(2,3,4,5,9,14,23,26,27)
cor(data.frame(trainset[,numericvars]))

#Logistic regression - estimating the probability of someone donating
fit1 <- glm(TARGDOL_BIN ~ CNDOL1 + CNCOD1 + CNDAT1 + CNTMLIF + SLCOD1 + REGION + SEX + CNMONL, data = trainset, family = binomial)
pred1 <- predict(fit1, data=testset)

