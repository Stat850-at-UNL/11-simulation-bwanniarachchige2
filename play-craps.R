roll_dice <- function() {
  # Input: None
  # Output: an integer between 2 and 12
  # Description: Simulates a roll of two six-sided dice, returning the sum
  die1 <- sample(1:6, 1)
  die2 <- sample(1:6, 1)
  return(die1 + die2)
}

is_come_out_roll <- function(roll) {
  # Input: roll - an integer between 2 and 12
  # Output: Boolean (TRUE if the roll is a come-out roll; FALSE otherwise)
  # Description: Checks if the roll is a come-out roll (either win, loss, or sets a point)
  return(roll %in% c(2, 3, 7, 11, 12))
}
 
determine_come_out_outcome <- function(roll) {
  # Input: roll - an integer between 2 and 12
  # Output: Character ("win", "loss", or "continue")
  # Description: Determines the outcome of the come-out roll
  if (roll %in% c(7, 11)) {
    return("win")
  } else if (roll %in% c(2, 3, 12)) {
    return("loss")
  } else {
    return("continue")  # Set a point
  }
}

simulate_craps_game <- function() {
  # Input: None
  # Output: Data frame containing columns: id, roll, outcome
  # Description: Simulates a single game of Craps, returning a data frame 
  #              that tracks each roll, its total, and the game outcome
  
  rolls <- data.frame(id = integer(), roll = integer(), outcome = character(), stringsAsFactors = FALSE)
  
  roll_count <- 1
  roll <- roll_dice()
  
  # Check the come-out roll
  outcome <- determine_come_out_outcome(roll)
  rolls <- rbind(rolls, data.frame(id = roll_count, roll = roll, outcome = outcome))
  
  # Continue if point is set
  if (outcome == "continue") {
    point <- roll
    repeat {
      roll_count <- roll_count + 1
      roll <- roll_dice()
      if (roll == point) {
        outcome <- "win"
        rolls <- rbind(rolls, data.frame(id = roll_count, roll = roll, outcome = outcome))
        break
      } else if (roll == 7) {
        outcome <- "loss"
        rolls <- rbind(rolls, data.frame(id = roll_count, roll = roll, outcome = outcome))
        break
      } else {
        rolls <- rbind(rolls, data.frame(id = roll_count, roll = roll, outcome = "continue"))
      }
    }
  }
  
  return(rolls)
}

library(dplyr)

summarize_craps_game <- function(game_data) {
  # Input: game_data - Data frame from simulate_craps_game
  # Output: Data frame with summary columns: n_rolls, outcome, point
  # Description: Summarizes a single game’s data, returning a single-row data frame
  
  n_rolls <- nrow(game_data)
  outcome <- tail(game_data$outcome, 1)
  first_roll <- game_data$roll[1]
  
  point <- ifelse(first_roll %in% c(4, 5, 6, 8, 9, 10), first_roll, NA)
  
  return(data.frame(n_rolls = n_rolls, outcome = outcome, point = point))
}
run_craps_simulation <- function(N) {
  # Input: N - Integer specifying the number of games to simulate
  # Output: Data frame with summary information for each game
  # Description: Runs N games of Craps, summarizing each game and compiling results
  
  simulations <- data.frame(n_rolls = integer(), outcome = character(), point = integer(), stringsAsFactors = FALSE)
  
  for (i in 1:N) {
    game_data <- simulate_craps_game()
    summary <- summarize_craps_game(game_data)
    simulations <- rbind(simulations, summary)
  }
  
  return(simulations)
}

