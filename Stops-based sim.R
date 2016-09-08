rand_perc <- function() {
      return(runif(1,0,1))
}

DOR <- function(Opp, Team) {
      return(sum(Opp[,"OFF"])/(sum(Opp[,"OFF"])+sum(Team[,"DEF"])))
}

DFG <- function(Opp) {
      return(sum(Opp[,"FG"])/sum(Opp[,"FGA"]))
}

FMwt <- function(Opp, Team) {
      return((DFG(Opp) * (1 - DOR(Opp, Team))) 
             / (DFG(Opp) * (1 - DOR(Opp, Team)) + (1 - DFG(Opp)) * DOR(Opp, Team)))
}

Stops1 <- function(Player, Opp, Team) {
      return(Team[Player,"STL"] + Team[Player,"BLK"] 
             * FMwt(Opp, Team) 
             * (1 - 1.07 * DOR(Opp, Team)) + Team[Player,"DEF"] 
             * (1 - FMwt(Opp, Team)))
}

Stops2 <- function(Player, Opp, Team) { 
      return((((sum(Opp[,"FGA"]) - sum(Opp[,"FG"]) - sum(Team[,"BLK"])) / sum(Team[,"MIN"])) 
              * FMwt(Opp, Team) 
              * (1 - 1.07 * DOR(Opp, Team)) + ((sum(Opp[,"TO"]) - sum(Team[,"STL"])) / sum(Team[,"MIN"]))) 
              * Team[Player,"MIN"] + (Team[Player,"PF"] / sum(Team[,"PF"])) * 0.4 * sum(Opp[,"FTA"]) 
              * (1 - (sum(Opp[,"FT"]) / sum(Opp[,"FTA"])))^2)
}

Stops <- function(Player, Opp, Team) { return(Stops1(Player, Opp, Team) + Stops2(Player, Opp, Team)) }

Stop_Per <- function(Player, Opp, Team) {
      return((Stops(Player, Opp, Team) * sum(Opp[,"MIN"])) / (sum(Team[,"NumPoss"]) * Team[Player,"MIN"]))
}

#Stop_Per(1, Oberlin, UCSC) # stops percentage for player 1 vs UCSC

# Opponent Squad Selection Code #
player.select <- function(df, col, num, squad) { # randomly selects one player based on player play time
      for (i in seq_along(df[,col])) {    # Enumerate-style function
            if (num <= df[,col][i] & (!(i %in% squad))) {
                  squad <- c(squad, i)
                  return(squad)
            }
      }   
}

squad.select <- function(df) { # selects 5 person squad returning data frame row number
      curr_squad <- vector()
      
      while(length(curr_squad) < 5) {
            curr_squad <- player.select(df, "CumPlayPercent", rand_perc(), curr_squad)
      }
      
      return(curr_squad)
}

#####
stops.sim <- function(IIT, Opponent) {
      
      scores <- numeric(10000)
      
      for(i in (1:10000)) { #use 1000 to start
            print(i)
            game.score <- 0
            num_poss <- round(rnorm(1, 64.3, 4.86)) # static (64) - Determine number of sim game possessions
            
            for (j in (1:num_poss)) { #use num_poss
                  
                  iit.stops <- numeric(5)
                  opp.stops <- numeric(5)
                  
                  # Determine players on each squad
                  iit_squad <- squad.select(IIT)
                  opp_squad <- squad.select(Opponent)
                  
                  # Determine IIT & Opponent Stops value
                  for(k in 1:5) {
                        iit.stops[k] <- Stop_Per(iit_squad[k], Opponent, IIT)
                        opp.stops[k] <- Stop_Per(opp_squad[k], IIT, Opponent)
                  }
                  
                  # IIT Possession with Opp Stops numbers
                  if(rand_perc() > mean(opp.stops)) {
                        game.score <- game.score + round(sum(IIT[iit_squad,"PTSPoss"]))      
                  }
                  
                  # Opp Possession with IIT Stops numbers
                  if(rand_perc() > mean(iit.stops)) {
                        game.score <- game.score - round(sum(Opponent[opp_squad,"PTSPoss"]))
                  }
                  
            }
            
            scores[i] <- round(game.score)
      }
      return(scores)
}
Rose.Hullman.stops.scores <- stops.sim(IIT, Rose.Hullman)
hist(Rose.Hullman.stops.scores)
#mean(Rose.Hullman.stops.scores)

Maranatha.stops.scores <- stops.sim(IIT, Maranatha)
hist(Maranatha.stops.scores)
#mean(Maranatha.stops.scores)

Monmouth.stops.scores <- stops.sim(IIT, Monmouth)
hist(Monmouth.stops.scores)
#mean(Monmouth.stops.scores)

Oberlin.stops.scores <- stops.sim(IIT, Oberlin)
hist(Oberlin.stops.scores)
#mean(Oberlin.stops.scores)

Rockford.stops.scores <- stops.sim(IIT, Rockford)
hist(Rockford.stops.scores)
#mean(Rockford.stops.scores)

St.Marys.stops.scores <- stops.sim(IIT, St.Marys)
hist(St.Marys.stops.scores)
#mean(St.Marys.stops.scores)

UCSC.stops.scores <- stops.sim(IIT, UCSC)
hist(UCSC.stops.scores)
#mean(UCSC.stops.scores)
