# The matching pennies game

set.seed(1999)
library(tidyverse)


# Random agent, no learning, no feedback
# "null" model
RandomAgent_f <- function(rate){
  choice <- rbinom(1,1,rate)
  return(choice)
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
  
  choice <- WSLSAgent_f(prevChoice, feedback)
  
  # noise 
  if (rbinom(1,1,noise) == 1){ # if noise = 0 it doesn't behave differently
    choice <- rbinom(1,1,0.5)
  }
  
  return(choice)
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
  

  Self[1]  <- RandomAgent_f(0.5)
  Other[1] <- BlockAgent_f(1)
  Feedback[1] <- as.numeric(Self[1] == Other[1])

  for (t in 2:trials){
    
    # WSLS chooses based on previous trial
    Self[t] <- WSLSAgentNoise_f(Self[t-1], Feedback[t-1], noise)
    
    # Block agent chooses based on current trial
    Other[t] <- BlockAgent_f(t)
    
    # Compute feedback
    Feedback[t] <- as.numeric(Self[t] == Other[t])
  }
  df <- tibble(
    trial = 1:trials,
    Self = Self,
    Other = Other,
    Feedback = Feedback
  )
  return(df)
  
  }


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

#plotting
ggplot(df_all, aes(trial, cumulative, group = agent_id)) + theme_classic() +
  geom_line(alpha = 0.2)


