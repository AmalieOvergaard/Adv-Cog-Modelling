# The matching pennies game

set.seed(1999)
library(tidyverse)
library(cmdstanr)

# Random agent, no learning, no feedback
RandomAgent_f <- function(rate){
  return(rate)
}

# Agent without noise
WSLSAgent_f <- function(prevChoice, feedback){
  
  if (feedback == 1){
    choice <- prevChoice
  } else {
    choice <- 1 - prevChoice
  }
  
  return(choice)
}

#introducing noise
# The agent with noise is probabilistic because the noise add randomness

WSLSAgentNoise_f <- function(prevChoice, feedback, noise){
  
  wsls_choice <- WSLSAgent_f(prevChoice, feedback)
  
  p_choice1 <- (1 - noise) * wsls_choice + noise * 0.5
  
  return(p_choice1)
}


# block agent

BlockAgent_f <- function(t){
  block <- floor((t-1)/10) # compute the block number, we can then see with even/odd what trial we're on
  choice <- block %% 2
  return(choice)
}


#---------
#SIMULATION 

trials <- 100
noise <- 0.1

simulate_game <- function(trials, noise){
  
  Self <- rep(NA, trials)
  Other <- rep(NA, trials)
  Feedback <- rep(NA, trials)
  P_self <- rep(NA, trials)
  

  P_self[1] <- RandomAgent_f(0.5)
  Self[1] <- rbinom(1, 1, P_self[1])
  Other[1] <- BlockAgent_f(1)
  Feedback[1] <- as.numeric(Self[1] == Other[1])
  P_self[1] <- 0.5
  Self[1]  <- RandomAgent_f(0.5)
  

  for (t in 2:trials){
    
    # Self[t] is the actual observed behaviour on trial t
    P_self[t] <- WSLSAgentNoise_f(Self[t-1], Feedback[t-1], noise)
    Self[t] <- rbinom(1, 1, P_self[t])
    
    
    # Block agent chooses based on current trial
    Other[t] <- BlockAgent_f(t)
    
    # Compute feedback
    Feedback[t] <- as.numeric(Self[t] == Other[t])
  }
  df <- tibble(
    trial = 1:trials,
    Self = Self,
    P_self = P_self,
    Other = Other,
    Feedback = Feedback
  )
  return(df)
  
  }


# now we have: 
# p_self: model prediction
# self: observed choice



df_all <- NULL

agents <- 100

for (agent in 1:agents){
  df_agent <- simulate_game(trials = 100, noise = 0.1)
  df_agent$agent_id <- agent
  df_all <- rbind(df_all, df_agent)
}

nrow(df_all)

# visualization, cumulative average per agent
df_all <- df_all %>%
  group_by(agent_id) %>%
  mutate(
    cumulative = cumsum(Feedback) / trial
  )

# Plotting
ggplot(df_all, aes(trial, cumulative, group = agent_id)) + theme_classic() +
  geom_line(alpha = 0.2)

# GET DATA READY FOR STAN

df_agent1 <- df_all %>%
  filter(agent_id == 1)

stan_data <- list(
  T = nrow(df_agent1),
  Self = df_agent1$Self,
  Other = df_agent1$Other
)


model <- cmdstan_model("wsls_noise_model.stan")

# cmdstanr::rebuild_cmdstan()

fit <- model$sample(data = stan_data)
fit$summary()








# PARAMETER RECOVERY
true_noise <- c(0.05, 0.1, 0.2, 0.3)

recovery_results <- data.frame(
  true_noise = true_noise,
  estimated_noise = NA
)

for(i in 1:length(true_noise)){
  
  noise_val <- true_noise[i]
  
  df_agent <- simulate_game(trials = 100, noise = noise_val)
  
  stan_data <- list(
    T = nrow(df_agent),
    Self = df_agent$Self,
    Other = df_agent$Other
  )
  
  fit <- model$sample(data = stan_data)
  summary_fit <- fit$summary(variables = "noise")
  
  recovery_results$estimated_noise[i] <- summary_fit$mean
}

print(recovery_results)

# plotting
ggplot(recovery_results, aes(true_noise, estimated_noise)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(
    x = "True noise",
    y = "Estimated noise",
    title = "Single-agent parameter recovery"
  )


# PARAMETER RECOVERY ACROSS TRIAL NUMBERS

true_noise <- c(0.05, 0.1, 0.2, 0.3)
trial_values <- c(20, 50, 100, 200)

recovery_results_trials <- expand.grid(
  true_noise = true_noise,
  trials = trial_values
)

recovery_results_trials$estimated_noise <- NA

for(i in 1:nrow(recovery_results_trials)){
  
  noise_val <- recovery_results_trials$true_noise[i]
  n_trials  <- recovery_results_trials$trials[i]
  
  df_agent <- simulate_game(trials = n_trials, noise = noise_val)
  
  stan_data <- list(
    T = nrow(df_agent),
    Self = df_agent$Self,
    Other = df_agent$Other
  )
  
  fit <- model$sample(data = stan_data)
  
  summary_fit <- fit$summary(variables = "noise")
  
  recovery_results_trials$estimated_noise[i] <- summary_fit$mean
}

print(recovery_results_trials)

#plot
ggplot(recovery_results_trials, aes(true_noise, estimated_noise)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ trials) +
  theme_classic() +
  labs(
    x = "True noise",
    y = "Estimated noise",
    title = "Single-agent parameter recovery across trial numbers"
  )


# ----------------------------
# PARAMETER RECOVERY
# ----------------------------

true_noise <- c(0.05, 0.1, 0.2, 0.3)

recovery_results <- data.frame(
  true_noise = true_noise,
  estimated_noise = NA
)

for(i in 1:length(true_noise)){
  
  noise_val <- true_noise[i]
  
  df_agent <- simulate_game(trials = 100, noise = noise_val)
  
  stan_data <- list(
    T = nrow(df_agent),
    Self = df_agent$Self,
    Other = df_agent$Other
  )
  
  fit <- model$sample(data = stan_data)
  
  summary_fit <- fit$summary(variables = "noise")
  
  recovery_results$estimated_noise[i] <- summary_fit$mean
}

print(recovery_results)

#plot
ggplot(recovery_results, aes(true_noise, estimated_noise)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(
    x = "True noise",
    y = "Estimated noise",
    title = "Single-agent parameter recovery"
  )

# ----------------------------
# PARAMETER RECOVERY
# ----------------------------

true_noise <- c(0.05, 0.1, 0.2, 0.3)

recovery_results <- data.frame(
  true_noise = true_noise,
  estimated_noise = NA
)

for (i in 1:length(true_noise)) {
  
  noise_val <- true_noise[i]
  
  df_agent <- simulate_game(trials = 100, noise = noise_val)
  
  stan_data <- list(
    T = nrow(df_agent),
    Self = df_agent$Self,
    Other = df_agent$Other
  )
  
  fit <- model$sample(data = stan_data)
  
  summary_fit <- fit$summary(variables = "noise")
  
  recovery_results$estimated_noise[i] <- summary_fit$mean
}

print(recovery_results)

#plot
ggplot(recovery_results, aes(true_noise, estimated_noise)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(
    x = "True noise",
    y = "Estimated noise",
    title = "Single-agent parameter recovery"
  )






# PRIOR PREDICTIVE CHECK

prior_noise <- rbeta(20, 1, 1)

prior_sim <- NULL

for(i in 1:length(prior_noise)){
  
  df_agent <- simulate_game(trials = 100, noise = prior_noise[i])
  
  df_agent$noise <- prior_noise[i]
  
  prior_sim <- rbind(prior_sim, df_agent)
}

prior_sim <- prior_sim %>%
  group_by(noise) %>%
  mutate(cumulative = cumsum(Feedback)/trial)

ggplot(prior_sim, aes(trial, cumulative, group = noise)) +
  geom_line(alpha = 0.3) +
  theme_classic() +
  labs(title = "Prior predictive simulations")






posterior_noise <- fit$draws("noise") |> as.vector()

# POSTERIOR PREDICTIVE CHECK


posterior_sim <- NULL

sample_noise <- sample(posterior_noise, 20)

for(i in 1:length(sample_noise)){
  
  df_agent <- simulate_game(trials = 100, noise = sample_noise[i])
  
  df_agent$noise <- sample_noise[i]
  
  posterior_sim <- rbind(posterior_sim, df_agent)
}

posterior_sim <- posterior_sim %>%
  group_by(noise) %>%
  mutate(cumulative = cumsum(Feedback)/trial)

ggplot(posterior_sim, aes(trial, cumulative, group = noise)) +
  geom_line(alpha = 0.3) +
  theme_classic() +
  labs(title = "Posterior predictive simulations")




# PRIOR VS POSTERIOR

prior_draws <- rbeta(1000,1,1)

posterior_draws <- posterior_noise

df_prior <- data.frame(noise = prior_draws, type = "Prior")
df_post <- data.frame(noise = posterior_draws, type = "Posterior")

df_plot <- rbind(df_prior, df_post)

ggplot(df_plot, aes(noise, fill = type)) +
  geom_density(alpha = 0.4) +
  theme_classic() +
  labs(title = "Prior vs Posterior")






posterior_noise <- fit$draws("noise") |> as.vector()

posterior_sim <- NULL

sample_noise <- sample(posterior_noise, 20)

for(i in 1:length(sample_noise)){
  
  df_agent <- simulate_game(trials = 100, noise = sample_noise[i])
  
  df_agent$noise <- sample_noise[i]
  
  posterior_sim <- rbind(posterior_sim, df_agent)
}

posterior_sim <- posterior_sim %>%
  group_by(noise) %>%
  mutate(cumulative = cumsum(Feedback)/trial)

ggplot(posterior_sim, aes(trial, cumulative, group = noise)) +
  geom_line(alpha = 0.3) +
  theme_classic() +
  labs(title = "Posterior predictive simulations")


