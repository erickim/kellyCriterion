###############################################################################
#                                                                             #
#                          simulating kelly systems                           #
#                                                                             #
###############################################################################

initializeHorses <- function(numHorses = 10) {
  ### Initializes numHorses horses (returns a probability per horse).
  ### Arguments:
  ###       numHorses: the number of horses
  ### Returns:
  ###       probs    : a number between 0 and 1 for each horse
  counts <- rbinom(numHorses, 10, .5)
  probs <- counts / sum(counts)
  
  return(probs)
}

simulateRound <- function(horses) {
  ### Simulates one round of the horse races
  ### Arguments:
  ###         horses <- horses from initializeHorses()
  ### Returns:
  ###         one round of the simulation
  
  race <- sapply(horses,
                   function(horse) {
                     # numbers simulated for the horses per run
                     # to help determine which horse wins
                     rbinom(1, 100, horse)})
  
  return(race)
}

initializeRaceSimulation <- function(horses = initializeHorses(),
                                     numSimulations = 100) {
  ### Conduct the initial race numSimulation times for the
  ### horses in initializeHorses to collect data to make bets.
  ### Arguments:
  ###       horses: the horses from initializeHorses()
  ### Returns:
  ###       the results for Win, Tie, Loss for each horse
  
  # conduct the 100 races
  races <- t(sapply(1:numSimulations,
                    function(i) simulateRound(horses)))
  # rename columns as horses
  colnames(races) <- paste0("Horse", 1:length(horses))
  
  # collect the results
  results <- data.frame(matrix(0,
                               nrow = length(horses),
                               ncol = 3))
  # rename columns again
  colnames(results) <- c("Win", "Tie", "Loss")
  
  # for the simulations, find the winners, ties, and losers and collect
  for (i in 1:numSimulations) {
    result <- races[i,] == max(races[i,])
    
    if (sum(result) == 1) {
      results[result, 1] <- results[result, 1] + 1
      results[!result, 3] <- results[!result, 3] + 1
    } else if (sum(results) > 1) {
      results[result, 2] <- results[result, 2] + 1
      results[!result, 3] <- results[!result, 3] + 1
    }
  }
  
  # return the data frame
  return(results/rowSums(results))
}

betKellySingle <- function(horseToBet,
                           horses,
                           fortune = INITIAL_FORTUNE,
                           payout = 5) {
  ### For the horse specified, bet the optimal kelly proportion
  ### Arguments:
  ###       horseToBet:   the horses to bet on
  ###       horses:       the horses from `initializeHorses`
  ###       fortune:      the amount of money to start with
  ###       payout :      the amount of money to win
  ###                     (`a` as given in slides)
  ### Returns:
  ###       newFortune: your fortune after one round of simulation
  
  # proportion of capital to bet on horse
  # must be no less than 0 and no more than 1
  optProp <- min(max((payout*horseToBet$Win - horseToBet$Loss) / 
    (payout*(horseToBet$Win + horseToBet$Loss)), 0), 1)
  # how much to bet
  toBet <- fortune * optProp
  # simulate one round
  round <- simulateRound(horses)
  # collect results
  result <- round == max(round)
    
  if (sum(result) == 1) {
    # if you won
    if (round[as.numeric(rownames(horseToBet))] == max(round)) {
      # if you won, keep your fortune plus get the bet times the payout
      newFortune <- fortune + toBet * payout
    } else {
      # if you lost, lose the bet
      newFortune <- fortune - toBet
    }
  } else if (sum(result) > 1) {
    # if you tied
    if (round[as.numeric(rownames(horseToBet))] == max(round)) {
      # keep your fortune
      newFortune <- fortune
    } else {
      # if you lose, lose the bet
      newFortune <- fortune - toBet
    }
  }
  
  return(newFortune)
}

betKellySimul <- function(horses,
                          horseData,
                          w = 0 ,
                          fortune = INITIAL_FORTUNE,
                          payout = 5) {
  ### For the amount of capital withheld, bet the optimal kelly proportions
  ### Arguments:
  ###       horses:       the horses from `initializeHorses`
  ###       horseData:    the output from `initializeRaceSimulation`
  ###       w:            the amount of capital withheld
  ###       fortune:      the amount of money to start with
  ###       payout :      the amount of money to win
  ###                     (`a` as given in slides)
  ### Returns:
  ###       newFortune: your fortune after one round of simulation
  
  # no capital withheld
  if (w == 0) {
    # bet the winning proportions of each horse
    optProp <- horseData$Win / sum(horseData$Win)
    # how much to bet for each horse
    toBet <- fortune*optProp
    # simulate one round
    round <- simulateRound(horses)
    # collect results
    result <- round == max(round)
    
    # change the result vector to payouts for each horse
    result[result] <- payout
    result[!result] <- -1
    
    # the new fortune after summing payouts for each horse
    newFortune <- sum(toBet*result)
    
  } else if (w > 0 & w < 1) {
    # not yet implemented
    return(fortune)
    
  } else {
    print("Bad `w` provided.")
    return(fortune)
  }
  
  return(newFortune)
}







