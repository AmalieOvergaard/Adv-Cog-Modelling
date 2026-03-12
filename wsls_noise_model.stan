
// --- DATA BLOCK --- 
// the choices can only be either 0 or 1 so that is also my lower and upper bound
// Observed data that we will give stan from our R simulation
// --- DATA BLOCK ---
data {
  int<lower=1> T;  // number of trials

  array[T] int<lower=0, upper=1> Self;  // player choices
  array[T] int<lower=0, upper=1> Other; // opponent choices
}

// --- PARAMETER BLOCK ---
// Tells stan that noise is a probability between 0 and 1
parameters {
  real<lower=0, upper=1> noise;
}

// Stan will then try many possible values to see which of the noise values are the most likely for our data


// --- MODEL BLOCK ---
// WSLS model logic and Bernoulli likelihood
model {
  noise ~ beta(1, 1); //uniform, all values between 0-1 are equally likely

  for (t in 2:T) {
    int prevChoice;
    int feedback;
    int wsls_choice;
    real p_self;

    prevChoice = Self[t-1];
    feedback = (Self[t-1] == Other[t-1]);

    if (feedback == 1)
      wsls_choice = prevChoice;
    else
      wsls_choice = 1 - prevChoice;

    p_self = (1 - noise) * wsls_choice + noise * 0.5;

    Self[t] ~ bernoulli(p_self);
  }
}










