#functions not to use
#choose()
#dbinom()
#pbinom()
#qbinom()
#rbinom()

#This function checks if a variable (prob) is between 1 and 0
check_prob <- function(prob){
  if (prob <= 1 & prob >= 0){
    return(TRUE)
  }
  else {
    stop("p has to be a value between 0 and 1 inclusive")
  }
}

#This function checks to see if the number of trials is greater than or equal to 0
check_trials <- function(trials){
  if (trials >= 0 & floor(trials) == trials){
    return(TRUE)
  }
  else{
    stop("Invalid Trials Value")
  }
}

#This function checks if an input value (success) is less than total number of trials (n) and greater than 0
check_success <- function(success, trials){
  if (is.vector(success) == TRUE){
    if((sum(success <= trials) == length(success))){
      return(TRUE)
    }
    else{
      stop("success cannot be greater than trials")
    }
  }
  else{
    stop("invalid success value")
  }
}

#Make simple comments, don't use checker functions
#aux_mean() returns the expected value of a binomial distribution
aux_mean <- function(trials, probability){
  return(trials*probability)
}
#aux_variance() returns the variance of a binomial distribution
aux_variance <- function(trials, probability){
  return(trials*probability*(1-probability))
}
#aux_mode() returns the most likely number of successes
aux_mode <- function(n, p){
  if(floor(n*p+p) == (n*p+p)){
    return(n*p+p)
    return(n*p+p-1)
  }
  else(return(floor(n*p+p)))
}

#aux_skewness() returns a value that shows the asymmetry of a probability distribution
aux_skewness <- function(trials, probability){
  a <- 1-2*probability
  b <- sqrt(trials*probability*(1-probability))
  return(a/b)
}
#aux_kurtosis is the measure of the 'tailedness' of a probability distribution
aux_kurtosis <- function(trials, probability){
  a <- 1-6*probability*(1-probability)
  b <- trials*probability*(1-probability)
  return(a/b)
}

#' @title bin_choose
#' @param n The number of trials
#' @param k The number of successes
#' @description Calculates the number of combinations in which k successes can occur in n trials
#' @return The number of combinations in which k successes can occur in n trials
#' @export
bin_choose <- function(n, k){
  if(is.vector(k)){
    if(sum(k>n) == 0){
      return(factorial(n)/(factorial(k)*factorial(n-k)))
    }
    else{stop("k cannot be greater than n")}
  }
  else if(k > n){
    stop("k cannot be greater than  n")
  }
  else{
    return(factorial(n)/(factorial(k)*factorial(n-k)))
  }
}

#' @title bin_probability
#' @param success The number of successes
#' @param trials The number of trials
#' @param prob A number between 0 and 1 that expresses the probability of success
#' @description Calculates the likelihood of getting a given number of successes given a number of trials and a probability
#' @return The likelihood of getting a given number of successes given a number of trials and a probability
#' @export
bin_probability <- function(success, trials, prob){
  if(check_trials(trials) == FALSE){
    stop("invalid trials value")
  }
  if(check_prob(prob) == FALSE){
    stop("invalid prob value")
  }
  if(check_success(success, trials) == FALSE){
    stop("invalid success value")
  }
  return(bin_choose(trials, success)*prob^(success)*(1-prob)^(trials - success))
}

#' @title bin_distribution
#' @param trials A intiger that is the number of trials
#' @param prob The probability of getting a success, must be between 0 and 1
#' @description Return a table of the probabilities of each possible amounts of success for a given amount of trials
#' @return Return a dataframe with the number of successes in one column and the given probability in another column
bin_distribution <- function(trials, prob){
  success <- c()
  probability <- c()
  for(q in 0:trials){
    success <- c(success, q)
    probability <- c(probability, bin_probability(q, trials, prob))
  }
  a <- (data.frame("success" = success, "probability" = probability))
  class(a) <- c("bindis", "data.frame")
  return(a)
}

#' @export
plot.bindis <- function(bindis){
  if (any(class(bindis) == "bindis")){
    barplot(bindis$probability, names.arg = bindis$success, xlab = "successes", ylab = "probability")
  }
  else{
    stop("object must be of the bindis class")
  }
}

#' @title bin_cumulative
#' @param trials A intiger that is the number of trials
#' @param prob The probability of getting a success, must be between 0 and 1
#' @description Return a table of the probabilities of each possible amounts of success for a given amount of trials, along with a column showing the cumulative odds
#' @return Return a dataframe with the number of successes in one column and the given probability in another column, with another column showing the cumulative odds
#' @export
bin_cumulative <- function(trials, prob){
  cum <- bin_distribution(trials, prob)
  b <- c()
  for(q in 0:trials){
    b <- c(b, sum(b[q], cum$probability[q+1]))
  }
  a <- data.frame(cum, "cumulative" = b)
  class(a) <- c("bincum", "data.frame")
  return(a)
}

#' @export
plot.bincum <- function(data){
  if (any(class(data) == "bincum")){
    plot(data$success, data$cumulative, type = "b", xlab = "successes", ylab = "probability")
  }
  else(stop("class should be bincum"))
}

#' @title bin_variable
#' @param trials A intiger that is the number of trials
#' @param prob The probability of getting a success, must be between 0 and 1
#' @description Return a list of the number of trials and the probability of success
#' @return A binomial random variable object of class binvar
#' @export
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  a <- list("trials" = trials, "prob" = prob)
  class(a) <- "binvar"
  return(a)
}

#' @export
print.binvar <- function(binvar){
  if(any(class(binvar) == "binvar")){
    cat("Binomial variable", "\n", "\n", "Parameters", "\n", "- number of trials: ", binvar$trials, "\n", "- prob of success : ", binvar$prob)
  }
}

#' @export
summary.binvar <- function(binvar){
  if(any(class(binvar) == "binvar")){
    t <- binvar$trials
    p <- binvar$prob
    a <- data.frame(
      "trials" = t,
      "prob" = p,
      "mean" = aux_mean(t, p),
      "variance" = aux_variance(t, p),
      "mode" = aux_mode(t, p),
      "skewness" = aux_skewness(t, p),
      "kurtosis" = aux_kurtosis(t, p)
    )
    class(a) <- "summary.binvar"
    return(a)
  }
}

#' @export
print.summary.binvar <- function(binvar){
  if(any(class(binvar) == "summary.binvar")){
    cat("Binomial variable", "\n", "\n", "Parameters", "\n", "- number of trials: ", binvar$trials, "\n", "- prob of success : ", binvar$prob, "\n", "\n")
    cat("Measures", "\n", "- mean    : ", binvar$mean, "\n", "- variance: ", binvar$variance,
        "\n", "- mode    : ", binvar$mode, "\n", "- skewness: ", binvar$skewness,
        "\n", "- kurtosis: ", binvar$kurtosis)
  }
}

#Main Functions
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  aux_mean(trials, prob)
}

bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  aux_variance(trials, prob)
}

bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  aux_mode(trials, prob)
}

bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  aux_skewness(trials, prob)
}

bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  aux_kurtosis(trials, prob)
}
