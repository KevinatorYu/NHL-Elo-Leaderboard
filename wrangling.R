library(stringr)
library(dplyr)

# Break up the 'game' column to get date + visiting team
df[c("date", "visit_team")] <- str_split_fixed(df$game, ' ', 2)

# Remove the "vs" from the date
df$date <- df$date %>%
  str_replace("vs", '')

# Changing "--" to zeros for pp_pct, pk_pct, net_pp_pct, and net_pk_pct
df$pp_pct <- as.factor(df$pp_pct)
df$pp_pct <- recode(df$pp_pct, "--" = "0.0")
df$pp_pct <- as.double(as.character(df$pp_pct))

df$pk_pct <- as.factor(df$pk_pct)
df$pk_pct <- recode(df$pk_pct, "--" = "0.0")
df$pk_pct <- as.double(as.character(df$pk_pct))

# Change some columns to different types, remove some columns
data <- data_frame(index = as.numeric(df$index),
                   date = df$date,
                   team = as.factor(df$team),
                   visit_team = as.factor(df$visit_team),
                   w = as.numeric(df$w),
                   l = as.numeric(df$l),
                   ot = as.numeric(df$ot),
                   p = as.numeric(df$p),
                   rw = as.numeric(df$rw),
                   row = as.numeric(df$row),
                   so_win = as.numeric(df$so_win),
                   gf = as.numeric(df$gf),
                   ga = as.numeric(df$ga),
                   pp_pct = as.numeric(df$pp_pct),
                   pk_pct = as.numeric(df$pk_pct),
                   shots_gp = as.numeric(df$shots_gp),
                   sa_gp = as.numeric(df$sa_gp),
                   fow_pct = as.numeric(df$fow_pct)
)
rm("df")

# Change all team names to their 3 letter counterpart
data$team <- recode(data$team,
                    "Anaheim Ducks" = "ANA",
                    "Arizona Coyotes" = "ARI",
                    "Boston Bruins" = "BOS",
                    "Buffalo Sabres" = "BUF",
                    "Carolina Hurricanes" = "CAR",
                    "Columbus Blue Jackets" = "CBJ",
                    "Calgary Flames" = "CGY",
                    "Chicago Blackhawks" = "CHI",
                    "Colorado Avalanche" = "COL",
                    "Dallas Stars" = "DAL",
                    "Detroit Red Wings" = "DET",
                    "Edmonton Oilers" = "EDM",
                    "Florida Panthers" = "FLA",
                    "Los Angeles Kings" = "LAK",
                    "Minnesota Wild" = "MIN",
                    "Montréal Canadiens" = "MTL",
                    "New Jersey Devils" = "NJD",
                    "Nashville Predators" = "NSH",
                    "New York Islanders" = "NYI",
                    "New York Rangers" = "NYR",
                    "Ottawa Senators" = "OTT",
                    "Philadelphia Flyers" = "PHI",
                    "Pittsburgh Penguins" = "PIT",
                    "Seattle Kraken" = "SEA",
                    "San Jose Sharks" = "SJS",
                    "St. Louis Blues" = "STL",
                    "Tampa Bay Lightning" = "TBL",
                    "Toronto Maple Leafs" = "TOR",
                    "Vancouver Canucks" = "VAN",
                    "Vegas Golden Knights" = "VGK",
                    "Winnipeg Jets" = "WPG",
                    "Washington Capitals" = "WSH")
data$team <- as.factor(data$team)
