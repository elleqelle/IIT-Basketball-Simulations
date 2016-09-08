rand_perc <- function() {
      return(runif(1,0,1))
}

# IIT Squad Selection Code #

rand_poss <- function() {
      return(runif(1,0,10007))
}

squad.select <- function(df, col, num) {
      for (i in seq_along(df[,col])) {    # Enumerate-style function
            if (num <= df[,col][i]) {
                  return(i)
            }
      }   
}

turnover <- function(df, col, num) { # determins if there is a turnover
      if(rand_perc() <= df[,col][num]) {
            return(TRUE)
      }
      return(FALSE)
}

#####
# Opponent Squad Selection Code #
player.select <- function(df, col, num, squad) { # randomly selects one player based on player play time
      for (i in seq_along(df[,col])) {    # Enumerate-style function
            if (num <= df[,col][i] & (!(i %in% squad))) {
                  squad <- c(squad, i)
                  return(squad)
            }
      }   
}

opp.squad.select <- function(df) { # selects 5 person squad currently playing
      curr_squad <- vector()
      
      while(length(curr_squad) < 5) {
            curr_squad <- player.select(df, "CumPlayPercent", rand_perc(), curr_squad)
      }
      
      curr_squad_df <- df[curr_squad,]
      return(curr_squad_df)
}
#####

scores <- numeric(100000)

for(i in (1:100000)) {
      print(i)
      game.score <- 0
      squad <- 1
      num_poss <- round(rnorm(1, 64.3, 4.86)) # static (64) - Determine number of sim game possessions
      
      for (j in (1:num_poss)) { 
            
            ### IIT Possession
            if (rand_perc() <= 0.2284) { # variance - round(rnorm(1, 14.7, 4.99)/ # poss) [% of subs]
                  squad <- squad.select(clean_squad_data, "RNG.Stops", rand_poss())
            }
            # If not a turnover, adjust score based on Points Per Possession 
            # PPPoss roughly accounts for missed shots and fouls
            if (!turnover(clean_squad_data,"TOPPoss", squad)) {
                  game.score <- game.score + round(clean_squad_data[,"PPPoss"][squad])      
            }
            
            ### Opp Possession
            curr_squad_df <- opp.squad.select(Maranatha)
            if (rand_perc() > sum(curr_squad_df$TOPPoss)) {
                  game.score <- game.score - round(sum(curr_squad_df[,"PTSPoss"]))
            }
      }
      
      scores[i] <- round(game.score)
}
hist(scores)
mean(scores)
Maranatha.scores <- scores

std.error <- function(x) { sd(x)/ sqrt(length(x)) }
