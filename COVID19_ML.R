#Upload data and develop random split (75/25)
covidstudycomponent <- read_excel("Dropbox/covidstudy.xlsx", sheet = "component")
View(component) 
set.seed(5796)
trainIndexcomponent<-createDataPartition(component$component, p = .75, list=FALSE, times=1)

#Generate train and test splits for Component
train_component<-covidstudycomponent[trainIndexcomponent,]
test_component<-covidstudycomponent[-trainIndexcomponent,]

#Training
set.seed(5796)
covid_trcontrol<-trainControl(verboseIter=TRUE, savePrediction=TRUE, method="repeatedcv", number=10, repeats=5, search="random")

#Random Forest for Component
set.seed(5796)
component_ranger<-train(component ~., data = train_component, method="ranger", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude, num.trees=1000, importance="permutation")

#Extreme Gradient Boosting for for Component
set.seed(5796)
component_xgboost<-train(component ~., data = train_component, method="xgbTree", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude, nthread=1, objective="reg:squarederror")

#Support Vector Machine with Radial Basis Function for Component
set.seed(5796)
component_svm<-train(component ~., data = train_component, method="svmRadial", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude)

#Elastic Net for Component
set.seed(5796)
component_glmnet<-train(component ~., data = train_component, method="glmnet", trControl=covid_trcontrol, tuneLength=20, preProcess=c("center", "scale"), metric="Rsquared", na.action=na.exclude)

#Predict external performance for Component
component_pred_ranger <- predict(component_ranger, test_component)
component_pred_xgboost <- predict(component_xgboost, test_component)
component_pred_svm <- predict(component_svm, test_component)
component_pred_glmnet <- predict(component_glmnet, test_component)

#Evaluate external performance for Component
postResample(pred = component_pred_ranger, obs = test_component$component)
postResample(pred = component_pred_xgboost, obs = test_component$component)
postResample(pred = component_pred_svm, obs = test_component$component)
postResample(pred = component_pred_glmnet, obs = test_component$component)

#Permutation-based p-values for Random Forest (Component)
importance_pvalues(component_ranger$finalModel, method = "altmann", formula = component ~., data = covidstudycomponent)
varImp(component_ranger)$importance

#Partial dependence plots
pancse <- pdp::partial(component_ranger, pred.var = "pancsetot", plot = TRUE, rug = FALSE)
pacta <- pdp::partial(component_ranger, pred.var = "pact_factorA", plot = TRUE, rug = FALSE)
hopetot <- pdp::partial(component_ranger, pred.var = "hopetot", plot = TRUE, rug = FALSE)
anymh <- pdp::partial(component_ranger, pred.var = "anymh", plot = TRUE, rug = FALSE)
plot(pancse)
plot(pacta)
plot(hopetot)
plot(anymh)
grid.arrange(pancse, pacta, hopetot, anymh)
