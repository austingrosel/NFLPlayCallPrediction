coach_pbp = pbp_game_info[pbp_game_info$PlayCaller == "Kyle Shanahan" & pbp_game_info$qtr == 1,c(13:ncol(pbp_game_info))]
coach_pbp[is.na(coach_pbp)] = 0
coach_pbp$sg = as.factor(coach_pbp$sg)
coach_pbp$nh = as.factor(coach_pbp$nh)
model_type = "GLM"

set.seed(123)
train_ind = sample(seq_len(nrow(coach_pbp)), size = 0.8 * nrow(coach_pbp))
train = coach_pbp[train_ind,]
test = coach_pbp[-train_ind,]
train_control = trainControl(method = "boot", number = 100)

glm_fit = train(as.factor(pass_bool)~., data = train, trControl = train_control, method = "glm")
pred_glm = predict(glm_fit, test)
tbl_glm = table(test$pass_bool, pred_glm)
acc_glm = sum(diag(tbl_glm))/sum(tbl_glm)
s_glm = sqrt(sum((as.numeric(pred_glm)-1 - mean(as.numeric(pred_glm)-1))^2)/(nrow(test) - 1))
1 - pt(t_test(acc_rf, acc_glm, s_rf, s_glm, nrow(test)), df = nrow(test) + 2)

tree_grid = expand.grid(cp = c(0.01, 0.02, 0.03, 0.04, 0.05))

tree_fit = train(as.factor(pass_bool)~., data = train, trControl = train_control, tuneGrid = tree_grid, method = "rpart")
pred_tree = predict(tree_fit, test)
tbl_tree = table(test$pass_bool, pred_tree)
acc_tree = sum(diag(tbl_tree))/sum(tbl_tree)


svm_grid = expand.grid(sigma = c(0.01, 0.15, 0.02),
                       C = c(0.75, 1, 1.25, 1.5))

svm_fit = train(as.factor(pass_bool)~., data = train, trControl = train_control, tuneGrid = svm_grid, method = "svmRadial")
pred_svm = predict(svm_fit, test)
tbl_svm = table(test$pass_bool, pred_svm)
acc_svm = sum(diag(tbl_svm))/sum(tbl_svm)

rf = randomForest(as.factor(pass_bool) ~ ., data = train, controls=cforest_unbiased(ntree=1000, mtry=3))
pred_mod_rf = predict(rf, test)
s = table(test$pass_bool, pred_mod_rf)
acc_rf = sum(diag(s))/sum(s)

accuracies = data.frame(method = c("GLM", "KNN", "SVM", "TREE", "RANDOM FOREST"),
                        acc = c(acc_glm, acc_knn, acc_svm, acc_tree, acc_rf))
accuracies = accuracies[order(-accuracies$acc),]

if(nrow(pbp_coach) > 500) {
  accs_fac = c(accs_fac, accuracies$acc[1])
  print(paste(coach, quarter, accuracies$acc[1], nrow(train)))
  coaches = c(coaches, coach)
  coach_pred_df = rbind(coach_pred_df, data.frame(qtr = quarter, coach = coach, accuracy = accuracies$acc[1], n = nrow(coach_pbp), method = accuracies$method[1]))
}


test$pred = pred_mod_rf
