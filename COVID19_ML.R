#Packages used
library(caret)
library(pdp)

#Upload data and develop random split for PHQ-9 (75/25)
covidstudyphq9 <- read_excel("Dropbox/covidstudy_5.21.xlsx", sheet = "phq9")
View(covidstudyphq9) 
set.seed(5796)
trainIndexphq9<-createDataPartition(covidstudyphq9$phq9tot, p = .75, list=FALSE, times=1)

#Generate train and test splits for Component
train_phq9<-covidstudyphq9[trainIndexphq9,]
test_phq9<-covidstudyphq9[-trainIndexphq9,]

#Training
set.seed(5796)
covid_trcontrol<-trainControl(verboseIter=TRUE, savePrediction=TRUE, method="repeatedcv", number=10, repeats=5, search="random")

#Random Forest for PHQ-9
set.seed(5796)
phq9_ranger<-train(phq9tot ~., data = train_phq9, method="ranger", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude, num.trees=1000, importance="permutation")

#Extreme Gradient Boosting for PHQ-9
set.seed(5796)
phq9_xgboost<-train(phq9tot ~., data = train_phq9, method="xgbTree", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude, nthread=1, objective="reg:squarederror")

#Support Vector Machine with Radial Basis Function for PHQ-9
set.seed(5796)
phq9_svm<-train(phq9tot ~., data = train_phq9, method="svmRadial", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude)

#Elastic Net for PHQ-9
set.seed(5796)
phq9_glmnet<-train(phq9tot ~., data = train_phq9, method="glmnet", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude)

#Predict external performance for PHQ-9
phq9_pred_ranger<-predict(phq9_ranger, test_phq9)
phq9_pred_xgboost<-predict(phq9_xgboost, test_phq9)
phq9_pred_svm<-predict(phq9_svm, test_phq9)
phq9_pred_glmnet<-predict(phq9_glmnet, test_phq9)

#Upload data and develop random split for GAD-7 (75/25)
covidstudygad7 <- read_excel("Dropbox/covidstudy_5.21.xlsx", sheet = "gad7")
View(covidstudygad7) 
set.seed(5796)
trainIndexgad7<-createDataPartition(covidstudygad7$gadtot, p = .75, list=FALSE, times=1)

#Generate train and test splits for Component
train_gad7<-covidstudygad7[trainIndexgad7,]
test_gad7<-covidstudygad7[-trainIndexgad7,]

#Random Forest for GAD-7
set.seed(5796)
gad7_ranger<-train(gadtot ~., data = train_gad7, method="ranger", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude, num.trees=1000, importance="permutation")

#Extreme Gradient Boosting for GAD-7
set.seed(5796)
gad7_xgboost<-train(gadtot ~., data = train_gad7, method="xgbTree", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude, nthread=1, objective="reg:squarederror")

#Support Vector Machine with Radial Basis Function for GAD-7
set.seed(5796)
gad7_svm<-train(gadtot ~., data = train_gad7, method="svmRadial", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude)

#Elastic Net for GAD-7
set.seed(5796)
gad7_glmnet<-train(gadtot ~., data = train_gad7, method="glmnet", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude)

#Upload data and develop random split for PCL-COVID (75/25)
covidstudypclcovid <- read_excel("Dropbox/covidstudy_5.21.xlsx", sheet = "pclcovid")
View(covidstudypclcovid) 
set.seed(5796)
trainIndexpclcovid<-createDataPartition(covidstudypclcovid$pclcovidtot, p = .75, list=FALSE, times=1)

#Generate train and test splits for Component
train_pclcovid<-covidstudypclcovid[trainIndexpclcovid,]
test_pclcovid<-covidstudypclcovid[-trainIndexpclcovid,]

#Random Forest for PCL-COVID
set.seed(5796)
pclcovid_ranger<-train(pclcovidtot ~., data = train_pclcovid, method="ranger", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude, num.trees=1000, importance="permutation")

#Extreme Gradient Boosting for PCL-COVID
set.seed(5796)
pclcovid_xgboost<-train(pclcovidtot ~., data = train_pclcovid, method="xgbTree", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude, nthread=1, objective="reg:squarederror")

#Support Vector Machine with Radial Basis Function for PCL-COVID
set.seed(5796)
pclcovid_svm<-train(pclcovidtot ~., data = train_pclcovid, method="svmRadial", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude)

#Elastic Net for PCL-COVID
set.seed(5796)
pclcovid_glmnet<-train(pclcovidtot ~., data = train_pclcovid, method="glmnet", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude)

#Predict external performance for PCL-COVID
pclcovid_pred_ranger<-predict(pclcovid_ranger, test_pclcovid)
pclcovid_pred_xgboost<-predict(pclcovid_xgboost, test_pclcovid)
pclcovid_pred_svm<-predict(pclcovid_svm, test_pclcovid)
pclcovid_pred_glmnet<-predict(pclcovid_glmnet, test_pclcovid)

#Upload data and develop random split for SSS-8 (75/25)
covidstudysss <- read_excel("Dropbox/covidstudy_5.21.xlsx", sheet = "sss")
View(covidstudysss) 
set.seed(5796)
trainIndexsss<-createDataPartition(covidstudysss$ssstot, p = .75, list=FALSE, times=1)

#Generate train and test splits for SSS-8
train_sss<-covidstudysss[trainIndexsss,]
test_sss<-covidstudysss[-trainIndexsss,]

#Random Forest for SSS-8
set.seed(5796)
sss_ranger<-train(ssstot ~., data = train_sss, method="ranger", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude, num.trees=1000, importance="permutation")

#Extreme Gradient Boosting for SSS-8
set.seed(5796)
sss_xgboost<-train(ssstot ~., data = train_sss, method="xgbTree", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude, nthread=1, objective="reg:squarederror")

#Support Vector Machine with Radial Basis Function for SSS-8
set.seed(5796)
sss_svm<-train(ssstot ~., data = train_sss, method="svmRadial", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude)

#Elastic Net for SSS-8
set.seed(5796)
sss_glmnet<-train(ssstot ~., data = train_sss, method="glmnet", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude)

#Predict external performance for SSS-8
sss_pred_ranger<-predict(sss_ranger, test_sss)
sss_pred_xgboost<-predict(sss_xgboost, test_sss)
sss_pred_svm<-predict(sss_svm, test_sss)
sss_pred_glmnet<-predict(sss_glmnet, test_sss)

#Evaluate external performance for PHQ-9
postResample(pred = phq9_pred_ranger, obs = test_phq9$phq9tot)
postResample(pred = phq9_pred_xgboost, obs = test_phq9$phq9tot)
postResample(pred = phq9_pred_svm, obs = test_phq9$phq9tot)
postResample(pred = phq9_pred_glmnet, obs = test_phq9$phq9tot)

#Permutation-based p-values for Random Forest (PHQ-9)
importance_pvalues(phq9_ranger$finalModel, method = "altmann", formula = phq9tot ~., data = covidstudyphq9)
varImp(phq9_ranger)$importance

#Predict external performance for GAD-7
gad7_pred_ranger<-predict(gad7_ranger, test_gad7)
gad7_pred_xgboost<-predict(gad7_xgboost, test_gad7)
gad7_pred_svm<-predict(gad7_svm, test_gad7)
gad7_pred_glmnet<-predict(gad7_glmnet, test_gad7)

#Evaluate external performance for GAD-7
postResample(pred = gad7_pred_ranger, obs = test_gad7$gadtot)
postResample(pred = gad7_pred_xgboost, obs = test_gad7$gadtot)
postResample(pred = gad7_pred_svm, obs = test_gad7$gadtot)
postResample(pred = gad7_pred_glmnet, obs = test_gad7$gadtot)

#Permutation-based p-values for Random Forest (GAD-7)
importance_pvalues(gad7_ranger$finalModel, method = "altmann", formula = gadtot ~., data = covidstudygad7)
varImp(gad7_ranger)$importance

#Evaluate external performance for PCL-COVID
postResample(pred = pclcovid_pred_ranger, obs = test_pclcovid$pclcovidtot)
postResample(pred = pclcovid_pred_xgboost, obs = test_pclcovid$pclcovidtot)
postResample(pred = pclcovid_pred_svm, obs = test_pclcovid$pclcovidtot)
postResample(pred = pclcovid_pred_glmnet, obs = test_pclcovid$pclcovidtot)

#Permutation-based p-values for Random Forest (PCL-COVID)
importance_pvalues(pclcovid_ranger$finalModel, method = "altmann", formula = pclcovidtot ~., data = covidstudypclcovid)
varImp(pclcovid_ranger)$importance

#Evaluate external performance for SSS-8
postResample(pred = sss_pred_ranger, obs = test_sss$ssstot)
postResample(pred = sss_pred_xgboost, obs = test_sss$ssstot)
postResample(pred = sss_pred_svm, obs = test_sss$ssstot)
postResample(pred = sss_pred_glmnet, obs = test_sss$ssstot)

#Permutation-based p-values for Random Forest (SSS-8)
importance_pvalues(sss_ranger$finalModel, method = "altmann", formula = ssstot ~., data = covidstudysss)
varImp(sss_ranger)$importance

#Partial dependence plots for PHQ-9
pancsephq <- pdp::partial(phq9_ranger, pred.var = "pancsetot", plot = FALSE, rug = FALSE) %>% plotPartial(phq9_ranger, smooth = FALSE, ylab = expression("PHQ-9"), xlab = "Pandemic Coping Self-Efficacy")
pactaphq <- pdp::partial(phq9_ranger, pred.var = "pact_factorA", plot = FALSE, rug = FALSE)  %>% plotPartial(phq9_ranger, smooth = FALSE, ylab = expression("PHQ-9"), xlab = "PACT Forward-Focused")
hopephq <- pdp::partial(phq9_ranger, pred.var = "hopetot", plot = FALSE, rug = FALSE)  %>% plotPartial(phq9_ranger, smooth = FALSE, ylab = expression("PHQ-9"), xlab = "Perceived Hope")
substancephq<-pdp::partial(phq9_ranger, pred.var = "cope_substance", plot = FALSE, rug = FALSE)  %>% plotPartial(phq9_ranger, smooth = FALSE, ylab = expression("PHQ-9"), xlab = "COPE Substances")
otherpsychphq <- pdp::partial(phq9_ranger, pred.var = "otherpsychdx", plot = FALSE, rug = FALSE)  %>% plotPartial(phq9_ranger, smooth = FALSE, ylab = expression("PHQ-9"), xlab = "Other Mental Health Diagnoses")
thoughtcovidphq<-pdp::partial(phq9_ranger, pred.var = "thoughthadcovid", plot = FALSE, rug = FALSE)  %>% plotPartial(phq9_ranger, smooth = FALSE, ylab = expression("PHQ-9"), xlab = "Thought had COVID")
plot(pancsephq)
plot(pactaphq)
plot(hopephq)
plot(otherpsychphq)
plot(substancephq)
plot(thoughtcovidphq)
grid.arrange(pancsephq, pactaphq, hopephq, substancephq, otherpsychphq, thoughtcovidphq)

#Partial dependence plots for GAD-7
pancsegad <- pdp::partial(gad7_ranger, pred.var = "pancsetot", plot = FALSE, rug = FALSE)  %>% plotPartial(gad7_ranger, smooth = FALSE, ylab = expression("GAD-7"), xlab = "Pandemic Coping Self-Efficacy")
pactagad <- pdp::partial(gad7_ranger, pred.var = "pact_factorA", plot = FALSE, rug = FALSE) %>% plotPartial(gad7_ranger, smooth = FALSE, ylab = expression("GAD-7"), xlab = "PACT Forward-Focused")
hopegad <- pdp::partial(gad7_ranger, pred.var = "hopetot", plot = FALSE, rug = FALSE) %>% plotPartial(gad7_ranger, smooth = FALSE, ylab = expression("GAD-7"), xlab = "Perceived Hope")
anxietygad<-pdp::partial(gad7_ranger, pred.var = "anxietydisorderselfreport", plot = FALSE, rug = FALSE) %>% plotPartial(gad7_ranger, smooth = FALSE, ylab = expression("GAD-7"), xlab = "Anxiety Disorder")
substancegad<-pdp::partial(gad7_ranger, pred.var = "cope_substance", plot = FALSE, rug = FALSE) %>% plotPartial(gad7_ranger, smooth = FALSE, ylab = expression("GAD-7"), xlab = "COPE Substances")
plot(pancsegad)
plot(pactagad)
plot(hopegad)
plot(anxietygad)
plot(substancegad)
grid.arrange(pancsegad, pactagad, hopegad, anxietygad, substancegad)

#Partial dependence plots for PCL-COVID
pancsepcl <- pdp::partial(pclcovid_ranger, pred.var = "pancsetot", plot = FALSE, rug = FALSE) %>% plotPartial(pclcovid_ranger, smooth = FALSE, ylab = expression("PCL-COVID"), xlab = "Pandemic Coping Self-Efficacy") 
pactapcl <- pdp::partial(pclcovid_ranger, pred.var = "pact_factorA", plot = FALSE, rug = FALSE) %>% plotPartial(pclcovid_ranger, smooth = FALSE, ylab = expression("PCL-COVID"), xlab = "PACT Forward-Focused") 
plot(pancsepcl)
plot(pactapcl)
grid.arrange(pancsepcl, pactapcl)

#Partial dependence plots for SSS-8
pancsesss <- pdp::partial(sss_ranger, pred.var = "pancsetot", plot = FALSE, rug = FALSE)  %>% plotPartial(sss_ranger, smooth = FALSE, ylab = expression("SSS-8"), xlab = "Pandemic Coping Self-Efficacy") 
anxietysss<-pdp::partial(sss_ranger, pred.var = "anxietydisorderselfreport", plot = FALSE, rug = FALSE) %>% plotPartial(sss_ranger, smooth = FALSE, ylab = expression("SSS-8"), xlab = "Anxiety Disorder") 
otherpsychsss <- pdp::partial(sss_ranger, pred.var = "otherpsychdx", plot = FALSE, rug = FALSE) %>% plotPartial(sss_ranger, smooth = FALSE, ylab = expression("SSS-8"), xlab = "Other Mental Health Diagnoses") 
impactsss <-pdp::partial(sss_ranger, pred.var = "impact_finance", plot = FALSE, rug = FALSE) %>% plotPartial(sss_ranger, smooth = FALSE, ylab = expression("SSS-8"), xlab = "COVID Financial Impact") 
lungsss<-pdp::partial(sss_ranger, pred.var = "lungprobs", plot = FALSE, rug = FALSE) %>% plotPartial(sss_ranger, smooth = FALSE, ylab = expression("SSS-8"), xlab = "Lung Problems") 
heartsss<-pdp::partial(sss_ranger, pred.var = "heartcondition", plot = FALSE, rug = FALSE) %>% plotPartial(sss_ranger, smooth = FALSE, ylab = expression("SSS-8"), xlab = "Heart Condition") 
plot(pancsesss)
plot(anxietysss)
plot(otherpsychsss)
plot(impactsss)
plot(lungsss)
plot(heartsss)
grid.arrange(pancsesss, anxietysss, otherpsychsss, impactsss, lungsss, heartsss)
