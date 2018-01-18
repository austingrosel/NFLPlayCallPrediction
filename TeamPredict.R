team_accs = data.frame()
teams = c()
team_pred_df = data.frame(qtr = c(), team = c(), accuracy = c(), n = c(), method = c())

pbp_game_info$off = ifelse(pbp_game_info$off == "STL", "LA", pbp_game_info$off)

### Similar for loop as in CoachPredict.R, however it creates two different dataframes: team_accs and team_pred_df
for(team in levels(as.factor(pbp_game_info$off))) {
  for(quarter in 1:1) {
    team_pbp = pbp_game_info[pbp_game_info$off == team & pbp_game_info$qtr == 1,c(13:ncol(pbp_game_info))]
    team_pbp[is.na(team_pbp)] = 0
    team_pbp$sg = as.factor(team_pbp$sg)
    team_pbp$nh = as.factor(team_pbp$nh)
    
    set.seed(123)
    train_ind = sample(seq_len(nrow(team_pbp)), size = 0.8 * nrow(team_pbp))
    train = team_pbp[train_ind,]
    test = team_pbp[-train_ind,]
    train_control = trainControl(method = "boot", number = 100)
    print(paste(team, nrow(team_pbp)))
    if(nrow(team_pbp) >= 260) {
      print("GLM")
      glm_fit = train(as.factor(pass_bool)~., data = train, trControl = train_control, method = "glm")
      pred_glm = predict(glm_fit, test)
      tbl_glm = table(test$pass_bool, pred_glm)
      acc_glm = sum(diag(tbl_glm))/sum(tbl_glm)
      
      tree_grid = expand.grid(cp = c(0.01, 0.02, 0.03, 0.04, 0.05))
      print("Tree")
      tree_fit = train(as.factor(pass_bool)~., data = train, trControl = train_control, tuneGrid = tree_grid, method = "rpart")
      pred_tree = predict(tree_fit, test)
      tbl_tree = table(test$pass_bool, pred_tree)
      acc_tree = sum(diag(tbl_tree))/sum(tbl_tree)
      
      svm_grid = expand.grid(sigma = c(0.01, 0.15, 0.02),
                             C = c(0.75, 1, 1.25, 1.5))
      print("SVM")
      svm_fit = train(as.factor(pass_bool)~., data = train, trControl = train_control, tuneGrid = svm_grid, method = "svmRadial")
      pred_svm = predict(svm_fit, test)
      tbl_svm = table(test$pass_bool, pred_svm)
      acc_svm = sum(diag(tbl_svm))/sum(tbl_svm)
      
      print("RF")
      rf = randomForest(as.factor(pass_bool) ~ ., data = train, controls=cforest_unbiased(ntree=1000, mtry=3))
      pred_mod_rf = predict(rf, test)
      s = table(test$pass_bool, pred_mod_rf)
      acc_rf = sum(diag(s))/sum(s)
      
      accuracies = data.frame(method = c("GLM", "SVM", "TREE", "RANDOM FOREST"),
                              acc = c(acc_glm, acc_svm, acc_tree, acc_rf))
      accuracies = accuracies[order(-accuracies$acc),]
      
      team_accs = rbind(team_accs, data.frame(acc = accuracies$acc, method = accuracies$method, team = team))
      print(paste(team, quarter, accuracies$method[1], accuracies$acc[1], nrow(team_pbp)))
      teams = c(teams, team)
      team_pred_df = rbind(team_pred_df, data.frame(qtr = quarter, team = team, accuracy = accuracies$acc[1], n = nrow(team_pbp), method = accuracies$method[1]))
    }
  }
}