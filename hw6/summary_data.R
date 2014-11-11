print(parameters_comb)

avg_mean= mean(stats_data$Steps_Taken[stats_data$Gridlock == T]);

print(c("mean =", avg_mean));

std_sd = sd(stats_data$Steps_Taken[stats_data$Gridlock == T]);

print(c("sd =", std_sd));

how_many_gl = sum(as.numeric(stats_data$Gridlock));

print(c("How many hit GL = ", how_many_gl));

