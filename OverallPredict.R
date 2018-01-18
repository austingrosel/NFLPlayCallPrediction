### This is a similar process as the other two predict files except taking in the entire dataset.
ovr_pbp = pbp_game_info[pbp_game_info$qtr == 1,c(13:ncol(pbp_game_info))]
ovr_pbp[is.na(ovr_pbp)] = 0
ovr_pbp$sg = as.factor(ovr_pbp$sg)
ovr_pbp$nh = as.factor(ovr_pbp$nh)

set.seed(123)
train_ind = sample(seq_len(nrow(ovr_pbp)), size = 0.8 * nrow(ovr_pbp))
train = ovr_pbp[train_ind,]
test = ovr_pbp[-train_ind,]
train_control = trainControl(method = "boot", number = 100)

print("GLM")
glm_fit = train(as.factor(pass_bool)~., data = train, trControl = train_control, method = "glm")
glm_fit
pred_glm = predict(glm_fit, test)
tbl_glm = table(test$pass_bool, pred_glm)
acc_glm = sum(diag(tbl_glm))/sum(tbl_glm)
s_glm = sqrt(sum((as.numeric(pred_glm)-1 - mean(as.numeric(pred_glm)-1))^2)/(nrow(test) - 1))
  
tree_grid = expand.grid(cp = c(0.01, 0.02, 0.03, 0.04, 0.05))
print("Tree")
tree_fit = train(as.factor(pass_bool)~., data = train, trControl = train_control, tuneGrid = tree_grid, method = "rpart")
pred_tree = predict(tree_fit, test)
tbl_tree = table(test$pass_bool, pred_tree)
acc_tree = sum(diag(tbl_tree))/sum(tbl_tree)
s_tree = sqrt(sum((as.numeric(pred_tree)-1 - mean(as.numeric(pred_tree)-1))^2)/(nrow(test) - 1))

svm_grid = expand.grid(sigma = c(0.01, 0.02),
                       C = c(0.75, 1, 1.25))
print("SVM")
svm_fit = train(as.factor(pass_bool)~., data = train, trControl = train_control, method = "svmLinear")
pred_svm = predict(svm_fit, test)
tbl_svm = table(test$pass_bool, pred_svm)
acc_svm = sum(diag(tbl_svm))/sum(tbl_svm)
  
print("RF")
rf = randomForest(as.factor(pass_bool) ~ ., data = train, controls=cforest_unbiased(ntree=1000, mtry=3))
pred_mod_rf = predict(rf, test)
s = table(test$pass_bool, pred_mod_rf)
confusionMatrix(s)
acc_rf = sum(diag(s))/sum(s)
s_rf = sqrt(sum((as.numeric(pred_mod_rf)-1 - mean(as.numeric(pred_mod_rf)-1))^2)/(nrow(test) - 1))

t_test = function(a1, a2, s1, s2, N) {
  t = (as.numeric(a1) - as.numeric(a2))/(sqrt((s1^2)/N+(s2^2)/N))
  return(t)
}

varImpPlot(rf)
accuracies = data.frame(method = c("GLM", "SVM", "TREE", "RANDOM FOREST"),
                        acc = c(acc_glm, acc_svm, acc_tree, acc_rf))
accuracies = accuracies[order(-accuracies$acc),]
