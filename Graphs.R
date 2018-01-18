library(ggplot2)

### File for my exploratory analysis and process graphs.

off_perc = pbp_game_info %>% group_by(off) %>% summarise(pass = sum(pass_bool == 1), run = sum(pass_bool == 0))
off_perc_t = t(off_perc[,-1])
colnames(off_perc_t) = off_perc$off

off_perc_melt = melt(cbind(off_perc_t), ind = rownames(off_perc_t), id.vars = c('ind'))

colnames(off_perc_melt) = c('type', 'team', 'percent')

ggplot(off_perc_melt, aes(x = team, y = percent, fill = factor(type, levels = c('run', 'pass')))) + geom_bar(position = 'fill', stat = 'identity') + labs(fill = "Play Type")

coach_perc = pbp_game_info %>% group_by(PlayCaller) %>% summarise(pass = sum(pass_bool == 1), run = sum(pass_bool == 0))
coach_perc_t = t(coach_perc[,-1])
colnames(coach_perc_t) = coach_perc$PlayCaller

coach_perc_melt = melt(cbind(coach_perc_t), ind = rownames(coach_perc_t), id.vars = c('ind'))

colnames(coach_perc_melt) = c('type', 'coach', 'percent')

pbp_game_info %>% filter(in_game_pass_ratio <= 0) %>% group_by(pass_bool) %>% summarise(n = n()) %>% mutate(ratio = n/sum(n))

ggplot(coach_perc_melt, aes(x = coach, y = percent, fill = factor(type, levels = c('run', 'pass')))) + geom_bar(position = 'fill', stat = 'identity') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(fill = "Play Type")

library(corrplot)
colnames(pbp_game_info)

for(i in 1:ncol(pbp_game_info)) {
  print(paste(i, colnames(pbp_game_info)[i], class(pbp_game_info[,i])))
}

lapply(pbp_game_info, class)

corrplot(cor(pbp_game_info[,c(30,16,17,21,22,28:29,31,33)]), method = 'ellipse')

library(grid)
library(gridExtra)

g1 = ggplot(pbp_game_info, aes(dwn, ..count..)) + geom_bar(aes(fill = as.factor(pass_bool)), position = "dodge") + labs(title = "Down", fill = "pass_bool")

g2 = ggplot(pbp_game_info, aes(cond, ..count..)) + geom_bar(aes(fill = as.factor(pass_bool)), position = "dodge") + labs(title = "Weather Condition", fill = "pass_bool")

g3 = ggplot(pbp_game_info, aes(as.factor(sg), ..count..)) + geom_bar(aes(fill = as.factor(pass_bool)), position = "dodge") + labs(title = "Shotgun", subtitle = "(1 for yes, 0 for no)", fill = "pass_bool")

g4 = ggplot(pbp_game_info, aes(as.factor(redzone_bool), ..count..)) + geom_bar(aes(fill = as.factor(pass_bool)), position = "dodge") + labs(title = "Redzone", subtitle = "(1 for yes, 0 for no)", fill = "pass_bool")

g5 = ggplot(pbp_game_info, aes(as.factor(prev_pass_succ), ..count..)) + geom_bar(aes(fill = as.factor(pass_bool)), position = "dodge") + labs(title = "Previous Pass Play Success", subtitle = "(1 for yes, 0 for no)", fill = "pass_bool")

g6 = ggplot(pbp_game_info, aes(as.factor(prev_rush_succ), ..count..)) + geom_bar(aes(fill = as.factor(pass_bool)), position = "dodge") + labs(title = "Previous Rush Play Success", subtitle = "(1 for yes, 0 for no)", fill = "pass_bool")

grid.arrange(g1,g2,g3,g4,g5,g6,nrow = 3)


ggplot(data = accs_fac, aes(acc, fill = method, color = method)) + geom_density(alpha = 0.2) + xlim(0.4, 0.9) 

ggplot(data = team_accs, aes(acc, fill = method, color = method)) + geom_density(alpha = 0.2) + xlim(0.4, 0.9) 