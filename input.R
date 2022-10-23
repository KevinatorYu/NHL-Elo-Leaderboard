library(dplyr)

# Build table
elo_table <- data.frame(team =               c("ANA", "ARI", "BOS", "BUF", "CGY", "CAR", "COL", "CHI", "CBJ", "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL",
                                               "NSH", "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "SEA", "STL", "TBL", "TOR", "VAN", "VGK", "WSH", "WPG"),
                        division = as.factor(c("PAC", "CEN", "ATL", "ATL", "PAC", "MET", "CEN", "CEN", "MET", "CEN", "ATL", "PAC", "ATL", "PAC", "CEN", "ATL", 
                                               "CEN", "MET", "MET", "MET", "ATL", "MET", "MET", "PAC", "PAC", "CEN", "ATL", "ATL", "PAC", "PAC", "MET", "CEN")),
                        games = rep(0,32),
                        wins = rep(0, 32),
                        draws = rep(0,32),
                        losses = rep(0,32),
                        elo = rep(1500, 32))

# Generate list of winners, losers, and the k factors
win_list <- list()
lose_list <- list()
k_list <- list()
r_list <- list()

# Determine outcome function
for (i in 1:nrow(data)) {
  
  game <- data[i,]
  
  ## Determine game result
  
  if(pull(game[12]) == pull(game[13]) && pull(game[11]) == 0) {
    ## Shootout loss (gf = ga & so_win = 0)
    win = as.character(pull(game[4]))
    lose = as.character(pull(game[3]))
    outcome = "so-loss"
    
    
  } else if(pull(game[12]) == pull(game[13]) && pull(game[11]) == 1) {
    ## Shootout win (gf = ga & so_win = 1)
    win = as.character(pull(game[3]))
    lose = as.character(pull(game[4]))
    outcome = "so-win"
    
  } else if(pull(game[12]) != pull(game[13]) && pull(game[7]) == 1) {
    ## Overtime loss (gf != ga & ot = 1)
    win = as.character(pull(game[4]))
    lose = as.character(pull(game[3]))
    outcome = "ot-loss"
    
  } else if(pull(game[5]) == 1 && pull(game[9]) == 0 && pull(game[11]) == 0) {
    ## Overtime win (w = 1 & rw = 0 & row = 1)
    win = as.character(pull(game[3]))
    lose = as.character(pull(game[4]))
    outcome = "ot-win"
    
  } else if(pull(game[6]) == 1) {
    ## Regular Loss (l = 1)
    win = as.character(pull(game[4]))
    lose = as.character(pull(game[3]))
    outcome = "reg-loss"
    
  } else if(pull(game[5]) == 1) {
    ## Regular Loss (l = 1)
    win = as.character(pull(game[3]))
    lose = as.character(pull(game[4]))
    outcome = "reg-win"
    
  } else {stop("Error: setup() failed")}
  
  
  ## Determine k-factor
  
  #TODO: Expand
  if (outcome == "so-loss") {
    k = 8
  } else if (outcome == "so-win") {
    k = 8
  } else if (outcome == "ot-loss") {
    k = 24
  } else if (outcome == "ot-win") {
    k = 24
  } else if (outcome == "reg-loss") {
    
    if ((game[13] - game[12]) == 1) {
      k = 32
    } else if ((game[13] - game[12]) == 2) {
      k = 48
    } else {
      k = 64
    }
    
  } else if (outcome == "reg-win") {
    
    if ((game[12] - game[13]) == 1) {
      k = 32
    } else if ((game[12] - game[13]) == 2) {
      k = 48
    } else {
      k = 64
    }
    
  }
  # Introduce "r" with simply "so", "ot", or "reg" based on result of game
  if (outcome == "reg-win" || outcome == "reg-loss") {
    r <- "reg"
  } else if (outcome == "ot-loss" || outcome == "ot-win") {
    r <- "ot"
  } else {
    r <- "so"
  }
  
  # Add winner, loser, and k factor to separate lists
  win_list <- append(win_list, win)
  lose_list <- append(lose_list, lose)
  k_list <- append(k_list, k)
  r_list <- append(r_list, r)
  
}

# Combine the 3 lists to a table
results_table <- cbind(win = win_list,
                       lose = lose_list,
                       k = k_list,
                       r = r_list) %>% as.data.frame()

rm(list = c("win_list", "lose_list", "k_list", "r_list", "i", "k", "r", "lose", "win", "outcome", "game"))

# Add results to table function
for (i in 1:nrow(results_table)) {

  win = results_table[i,1]
  lose = results_table[i,2]
  k = as.numeric(results_table[i,3])
  result = results_table[i,4]

  # if result = so or ot, add game played + add draw to each team
  if (result == "so" || result == "ot") {
    
    for (j in 1:nrow(elo_table)) {
      
      if (elo_table[j,1] == win) {
        elo_table[j,3] = elo_table[j,3] + 1 # add game played
        elo_table[j,5] = elo_table[j,5] + 1 # add draw
      }
      
      if (elo_table[j,1] == lose) {
        elo_table[j,3] = elo_table[j,3] + 1 # add game played
        elo_table[j,5] = elo_table[j,5] + 1 # add draw
      }
      
    }
    
  } else {
    # result is reg, add game played to each team, add win for winner, add lose to loser 
    for (j in 1:nrow(elo_table)) {
      
      if (elo_table[j,1] == win) {
        elo_table[j,3] = elo_table[j,3] + 1 # add game played
        elo_table[j,4] = elo_table[j,4] + 1 # add win
      }
      
      if (elo_table[j,1] == lose) {
        elo_table[j,3] = elo_table[j,3] + 1 # add game played
        elo_table[j,6] = elo_table[j,6] + 1 # add loss
      }
      
    }
    
  }
  
  rm(list = c("i", "j"))

  
  # Get old Elo of win and losing team
  for (i in 1:nrow(elo_table)) {
    
    if (elo_table[i,1] == win) {
      win_old_elo <- elo_table[i,7]
    }
    
    if (elo_table[i,1] == lose) {
      lose_old_elo <- elo_table[i,7]
    }
    
  }
  
  # Calculate new Elos
  win_new_elo <- round(win_old_elo + (k * (1 - 1/(1 + (10^((lose_old_elo - win_old_elo)/400))))), 1)
  lose_new_elo <- round(lose_old_elo + (k * (0 - 1/(1 + (10^((win_old_elo - lose_old_elo)/400))))), 1)
  
  # Replace old Elo with new Elos
  for (j in 1:nrow(elo_table)) {
    
    if (elo_table[j,1] == win) {
      elo_table[j,7] = win_new_elo
    }
    
    if (elo_table[j,1] == lose) {
      elo_table[j,7] = lose_new_elo
    }
    
  }
  
  rm(list = c("i", "j", "win", "lose", "result", "k", "lose_new_elo", "win_new_elo", "lose_old_elo", "win_old_elo"))
  
}

elo_table <- arrange(elo_table,desc(elo))
