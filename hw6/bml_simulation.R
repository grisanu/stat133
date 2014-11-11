#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

#sourcing bml_functions.R
source('bml_functions.R');

r = 300;
c = 300;
p = 0.5;

sample_times = 200;
max_no_steps = 15000;

parameters_comb = matrix(c(r,c,p,sample_times,max_no_steps), nrow = 1, ncol = 5);
colnames(parameters_comb) = c("r", "c", "p", "No of Times Sampled (n)", "Max Steps Limit");

# data frame columns
Steps_Taken = rep(0, sample_times);
Hit_Max_Step = rep(T, sample_times);
Gridlock = rep(T, sample_times);
Time_Taken = rep(0, sample_times);

stats_data = data.frame(Steps_Taken, Hit_Max_Step, Gridlock, Time_Taken);

for (i in 1:sample_times) {
  output = bml.sim(r, c, p, max_steps = max_no_steps, should_print_step = TRUE);
  
  print(i);
  #put data into data frame
  stats_data$Steps_Taken[i] = output[[2]];
  stats_data$Hit_Max_Step[i] = output[[3]];
  stats_data$Gridlock[i] = output[[4]];
  stats_data$Time_Taken[i] = output[[5]];
}

print(c("Finished:", r,"x",c, "p=", p), quote = FALSE);

save(parameters_comb, stats_data, file = "traffic_300_0.5.RData");

##################################################################################################

r = 300;
c = 300;
p = 0.4;

sample_times = 200;
max_no_steps = 15000;

parameters_comb = matrix(c(r,c,p,sample_times,max_no_steps), nrow = 1, ncol = 5);
colnames(parameters_comb) = c("r", "c", "p", "No of Times Sampled (n)", "Max Steps Limit");

# data frame columns
Steps_Taken = rep(0, sample_times);
Hit_Max_Step = rep(T, sample_times);
Gridlock = rep(T, sample_times);
Time_Taken = rep(0, sample_times);

stats_data = data.frame(Steps_Taken, Hit_Max_Step, Gridlock, Time_Taken);

for (i in 1:sample_times) {
  output = bml.sim(r, c, p, max_steps = max_no_steps, should_print_step = TRUE);
  
  print(i);
  #put data into data frame
  stats_data$Steps_Taken[i] = output[[2]];
  stats_data$Hit_Max_Step[i] = output[[3]];
  stats_data$Gridlock[i] = output[[4]];
  stats_data$Time_Taken[i] = output[[5]];
}

print(c("Finished:", r,"x",c, "p=", p), quote = FALSE);

save(parameters_comb, stats_data, file = "traffic_300_0.4.RData");

##################################################################################################

r = 300;
c = 300;
p = 0.3;

sample_times = 200;
max_no_steps = 15000;

parameters_comb = matrix(c(r,c,p,sample_times,max_no_steps), nrow = 1, ncol = 5);
colnames(parameters_comb) = c("r", "c", "p", "No of Times Sampled (n)", "Max Steps Limit");

# data frame columns
Steps_Taken = rep(0, sample_times);
Hit_Max_Step = rep(T, sample_times);
Gridlock = rep(T, sample_times);
Time_Taken = rep(0, sample_times);

stats_data = data.frame(Steps_Taken, Hit_Max_Step, Gridlock, Time_Taken);

for (i in 1:sample_times) {
  output = bml.sim(r, c, p, max_steps = max_no_steps, should_print_step = TRUE);
  
  print(i);
  #put data into data frame
  stats_data$Steps_Taken[i] = output[[2]];
  stats_data$Hit_Max_Step[i] = output[[3]];
  stats_data$Gridlock[i] = output[[4]];
  stats_data$Time_Taken[i] = output[[5]];
}

print(c("Finished:", r,"x",c, "p=", p), quote = FALSE);

save(parameters_comb, stats_data, file = "traffic_300_0.3.RData");

##################################################################################################

r = 256;
c = 256;
p = 0.5;

sample_times = 200;
max_no_steps = 15000;

parameters_comb = matrix(c(r,c,p,sample_times,max_no_steps), nrow = 1, ncol = 5);
colnames(parameters_comb) = c("r", "c", "p", "No of Times Sampled (n)", "Max Steps Limit");

# data frame columns
Steps_Taken = rep(0, sample_times);
Hit_Max_Step = rep(T, sample_times);
Gridlock = rep(T, sample_times);
Time_Taken = rep(0, sample_times);

stats_data = data.frame(Steps_Taken, Hit_Max_Step, Gridlock, Time_Taken);

for (i in 1:sample_times) {
  output = bml.sim(r, c, p, max_steps = max_no_steps, should_print_step = TRUE);
  
  print(i);
  #put data into data frame
  stats_data$Steps_Taken[i] = output[[2]];
  stats_data$Hit_Max_Step[i] = output[[3]];
  stats_data$Gridlock[i] = output[[4]];
  stats_data$Time_Taken[i] = output[[5]];
}

print(c("Finished:", r,"x",c, "p=", p), quote = FALSE);

save(parameters_comb, stats_data, file = "traffic_256_0.5.RData");

##################################################################################################

r = 256;
c = 256;
p = 0.4;

sample_times = 200;
max_no_steps = 15000;

parameters_comb = matrix(c(r,c,p,sample_times,max_no_steps), nrow = 1, ncol = 5);
colnames(parameters_comb) = c("r", "c", "p", "No of Times Sampled (n)", "Max Steps Limit");

# data frame columns
Steps_Taken = rep(0, sample_times);
Hit_Max_Step = rep(T, sample_times);
Gridlock = rep(T, sample_times);
Time_Taken = rep(0, sample_times);

stats_data = data.frame(Steps_Taken, Hit_Max_Step, Gridlock, Time_Taken);

for (i in 1:sample_times) {
  output = bml.sim(r, c, p, max_steps = max_no_steps, should_print_step = TRUE);
  
  print(i);
  #put data into data frame
  stats_data$Steps_Taken[i] = output[[2]];
  stats_data$Hit_Max_Step[i] = output[[3]];
  stats_data$Gridlock[i] = output[[4]];
  stats_data$Time_Taken[i] = output[[5]];
}

print(c("Finished:", r,"x",c, "p=", p), quote = FALSE);

save(parameters_comb, stats_data, file = "traffic_256_0.4.RData");

##################################################################################################

r = 256;
c = 256;
p = 0.3;

sample_times = 200;
max_no_steps = 15000;

parameters_comb = matrix(c(r,c,p,sample_times,max_no_steps), nrow = 1, ncol = 5);
colnames(parameters_comb) = c("r", "c", "p", "No of Times Sampled (n)", "Max Steps Limit");

# data frame columns
Steps_Taken = rep(0, sample_times);
Hit_Max_Step = rep(T, sample_times);
Gridlock = rep(T, sample_times);
Time_Taken = rep(0, sample_times);

stats_data = data.frame(Steps_Taken, Hit_Max_Step, Gridlock, Time_Taken);

for (i in 1:sample_times) {
  output = bml.sim(r, c, p, max_steps = max_no_steps, should_print_step = TRUE);
  
  print(i);
  #put data into data frame
  stats_data$Steps_Taken[i] = output[[2]];
  stats_data$Hit_Max_Step[i] = output[[3]];
  stats_data$Gridlock[i] = output[[4]];
  stats_data$Time_Taken[i] = output[[5]];
}

print(c("Finished:", r,"x",c, "p=", p), quote = FALSE);

save(parameters_comb, stats_data, file = "traffic_256_0.3.RData");

##################################################################################################
