library(nhlapi)# The data for all tasks undertaken for this project are taken from nhlapi's nhl_player_allseasons() function, which retrieves and processes information based on player names and id's from 
# the open NHL API on statistics for each season of their careers. Information for Patrice Bergeron extracted from https://statsapi.web.nhl.com/api/v1/people/8470638/stats?stats=yearByYear. 
# Information for Patrick Kane extracted from https://statsapi.web.nhl.com/api/v1/people/8474141/stats?stats=yearByYear. Information for Steven Stamkos extracted from 
# https://statsapi.web.nhl.com/api/v1/people/8474564/stats?stats=yearByYear. Information for Sidney Crosby extracted from https://statsapi.web.nhl.com/api/v1/people/8471675/stats?stats=yearByYear. 
# Information on Connor McDavid extracted from https://statsapi.web.nhl.com/api/v1/people/8478402/stats?stats=yearByYear.

# Task 1: Creating line chart displaying the total number of cumulative points that Patrice Bergeron, Patrick Kane, Steven Stamkos, Sidney Crosby and Connor McDavid 
# have accumulated over the courses of their careers so far.
playerNames <- c("Patrice Bergeron", "Patrick Kane", "Steven Stamkos", "Sidney Crosby", "Connor McDavid") # Creating a vector of player names for players I chose to focus on for this project.
library(dplyr)
library(highcharter)
result1 <- nhl_players(playerNames) %>%
  left_join(
    nhl_players_allseasons(playerNames),
    by = c("id" = "playerId") # Joining the data from the nhl_players() function provided by nhlapi on the nhl_players_allseasons() function based on player_id.
  ) %>%
  filter(league.name == "National Hockey League") %>% # Filtering data for only data from the NHL and not any other leagues.
  select(fullName, seasonStart, stat.points) %>% # Selecting the information I want to gather (player name, season, and points recorded).
  as_tibble() %>% # Saving this filtered data to a tibble
  group_by(fullName) %>% # Grouping the data according to each player's full names.
  mutate(cumul.points = cumsum(stat.points)) %>% ungroup() # Mutating the data in order to create cumul.points, which tracks the cumulative number of points each player has accumulated over the course of their careers.
result1 # Displays tibble with all the data specified above called result1

result1 %>%
  hchart("line", hcaes(seasonStart, cumul.points, group = fullName)) %>%
  hc_title(
    text = "Cumulative Total Points During NHL Careers") %>%
  hc_add_theme(hc_theme_google()) # Pipes the data from the result1 tibble to a line chart displaying visually the cumulative points that each player has scored during their careers.

# Task 2: Creating a line chart displaying the cumulative number of goals each of these five players have scored during their NHL careers.
result2<- nhl_players(playerNames) %>%
  left_join(
    nhl_players_allseasons(playerNames),
    by = c("id" = "playerId") # Joining the data from the nhl_players() function provided by nhlapi on the nhl_players_allseasons() function based on player_id.
  )%>%
  filter(league.name == "National Hockey League") %>% # Filtering player data for only data from the NHL and not any other leagues
  select(fullName, seasonStart, stat.goals) %>% # Selecting the information I want to gather (player name, season, and goals recorded)
  as_tibble() %>% # Saving this filtered data to a tibble
  group_by(fullName) %>% # Grouping the data according to player name
  mutate(cumul.goals = cumsum(stat.goals)) %>% ungroup() # Mutating the data in order to create cumul.goals, which tracks the cumulative amount of goals each player has racked up over the course of their careers
result2 # Displays tibble with all the data specified above

result2 %>%
  hchart("line", hcaes(seasonStart, cumul.goals, group = fullName)) %>%
  hc_title(
    text = "Cumulative Goals Scored During NHL Careers") %>%
  hc_add_theme(hc_theme_google()) # Pipes the data from the result tibble to a line chart displaying visually the cumulative # of goals that each player has scored during their careers

# Task 3: Creating line chart displaying the cumulative number of assists each of these five players have recorded during their NHL careers.
result3<- nhl_players(playerNames) %>%
  left_join(
    nhl_players_allseasons(playerNames),
    by = c("id" = "playerId") # Joining the data from the nhl_players() function provided by nhlapi on the nhl_players_allseasons() function based on player_id.
  ) %>%
  filter(league.name == "National Hockey League") %>% # Filtering player data for only data from the NHL and not any other leagues
  select(fullName, seasonStart, stat.assists) %>% # Selecting the information I want to gather (player name, season, and assists recorded)
  as_tibble() %>% # Saving this filtered data to a tibble
  group_by(fullName) %>% # Grouping the data according to player name
  mutate(cumul.assists = cumsum(stat.assists)) %>% ungroup() # Mutating the data in order to create cumul.assists, which tracks the cumulative amount of assists each player has racked up over the course of their careers
result3 # Displays tibble with all the data specified above

result3 %>%
  hchart("line", hcaes(seasonStart, cumul.assists, group = fullName)) %>%
  hc_title(
    text = "Cumulative Assists Recorded During NHL Careers") %>%
  hc_add_theme(hc_theme_google()) # Pipes the data from the result tibble to a line chart displaying visually the cumulative # of assists that each player has recorded during their careers

# Task 4: Creating a column chart displaying the point per game rate for each player for each season from 2019 until 2021 (last full NHL season completed).
result4<- nhl_players(playerNames) %>%
  left_join(
    nhl_players_allseasons(playerNames),
    by = c("id" = "playerId") # Joining the data from the nhl_players() function provided by nhlapi on the nhl_players_allseasons() function based on player_id.
  ) %>%
  filter(league.name == "National Hockey League", seasonStart %in% c(2019, 2020, 2021)) %>% # Process is much the same as before, except now we are filtering for specific seasons (2019, 2020 & 2021).
  select(fullName, seasonStart, stat.points) %>% # Selecting the information I want to gather (player name, season, and points recored).
  as_tibble() %>%
  group_by(fullName) %>%
  mutate(pts.per.game = (stat.points/82)) %>% # Additionally, I created the pts.per.game stat by dividing the amount of points a player had during the season by 82 (total number of games).
  ungroup()
result4 # Displays tibble with all the data specified above

result4 %>%
  hchart("column", hcaes(seasonStart, pts.per.game, group = fullName)) %>% 
  hc_title(
    text = "Points Per Game Rate from 2019 to 2021") %>%
  hc_add_theme(hc_theme_google()) # Pipes data contained in result4 to a column chart that shows the pts.per.game for each player from the 2019 season until the 2021 season.

# Task 5: Examining the relationship between assist per game ratio and goals per game ratio for each player from the 2019 season until 2021 season (last full season completed) with points per game representing bubble size.
library(googleVis)
result5<- nhl_players(playerNames) %>%
  left_join(
    nhl_players_allseasons(playerNames),
    by = c("id" = "playerId") # Joining the data from the nhl_players() function provided by nhlapi on the nhl_players_allseasons() function based on player_id.
  ) %>%
  filter(league.name == "National Hockey League", seasonStart %in% c(2019, 2020, 2021)) %>% # Using same filter was used in result4.
  select(fullName, seasonStart, stat.goals, stat.points, stat.assists) %>% # Selecting the information I want to gather (player name, season, goals recorded, points recorded & assists recorded).
  as_tibble() %>%
  group_by(fullName) %>%
  mutate(goals.per.game = (stat.goals/82)) %>% # Creating goals.per.game stat by dividing the amount of goals a player had during the season by 82 (total number of games).
  mutate(seasonStart.char = as.character(seasonStart)) %>% # Creating seasonStart.char by converting seasonStart from an integer to a character. 
  mutate(points.per.game = (stat.points/82)) %>% # Creating points.per.game stat by dividing the amount of points a player had during the season by 82 (total number of games).
  mutate(assists.per.game = (stat.assists/82)) %>% # Creating assists.per.game stat by dividing the amount of assists a player had during the season by 82 (total number of games).
  ungroup()
result5 # Displays tibble with all the data specified above

chart1 <- gvisBubbleChart(result5,
                idvar = "fullName",
                xvar = "goals.per.game", yvar = "assists.per.game",
                colorvar = "seasonStart.char", sizevar= "points.per.game",
                options = list(
                  hAxis='{minValue:0, maxValue:0.75}',
                  width=800, height=700)) # Creates a Bubble Chart plotting goals.per.game against assists.per.game for each player using the data contained in result5. 

plot(chart1) # Function to create plot.

# Task 6: Examining via a histogram the distribution of the number of penalty minuets for each player from the 2019 season until the 2021 season (last full NHL season completed),
library(ggplot2)
result6<- nhl_players(playerNames) %>%
  left_join(
    nhl_players_allseasons(playerNames),
    by = c("id" = "playerId") # Joining the data from the nhl_players() function provided by nhlapi on the nhl_players_allseasons() function based on player_id.
  ) %>%
  filter(league.name == "National Hockey League", seasonStart %in% c(2019, 2020, 2021)) %>% # Same filter used in result4 and result5.
  select(fullName, seasonStart, stat.pim) %>% # Selecting the information I want to gather (player name, season & penalty minutes).
  as_tibble() %>%
  group_by(fullName) %>%
  ungroup()
result6 # Displays tibble with all the data specified above

chart2 <- hist(result6$stat.pim) # Creates a histogram of the data contained in stat.pim inside result6.

# Task 7: Scatterplot examining the relationship between penalty minutes and the plus/minus rate for each player from the 2019 season to the 2021 season (last full NHL season completed).
result7<- nhl_players(playerNames) %>%
  left_join(
    nhl_players_allseasons(playerNames),
    by = c("id" = "playerId") # Joining the data from the nhl_players() function provided by nhlapi on the nhl_players_allseasons() function based on player_id.
  ) %>%
  filter(league.name == "National Hockey League", seasonStart %in% c(2019, 2020, 2021)) %>% # Again, same filter as used in result4, result5 & result6.
  select(fullName, seasonStart, stat.pim, stat.plusMinus) %>% # Selecting the information I want to gather (player name, season, penalty minutes and plus/minus rate).
  as_tibble() %>%
  group_by(fullName) %>%
  ungroup()
result7 # Displays tibble with all the data specified above

model <- lm(result7$stat.plusMinus ~ result7$stat.pim, data = result7) # Creating linear model examining relationship of stat.plusMinus and stat.pim values contained in result7 object.
summary(model) # Creates a summary of the linear model.
lm(formula = result7$stat.pim ~ result7$stat.plusMinus, data = result7) # Calculates the intercept and coefficient value of result7$stat.plusMinus.
plot(result7$stat.pim, result7$stat.plusMinus, xlim = c(0,60), ylim = c(-30,30), main = "Scatterplot of # of Penalty Minutes vs. Player Plus/Minus",
               xlab = "Penalty Minutes", ylab = "Player Plus/Minus") # Plots scatterplot displaying the relationship of result7$stat.pim and result7$stat.plusMinus.
abline(model) # Adds line of best fit for the model to the scatterplot.
legend("topleft", legend=paste("R2 is", format(summary(model)$r.squared, digits = 3))) # Adds R2 value to the top left portion of the scatterplot to reveal correlation between the stat.pim and stat.plusMinus 
# values contained in the result7 object.

# Task 8: Figuring out who has had the most had the highest plus/minus for each season from 2019 until 2021 using a column chart.
result8<- nhl_players(playerNames) %>%
  left_join(
    nhl_players_allseasons(playerNames),
    by = c("id" = "playerId") # Joining the data from the nhl_players() function provided by nhlapi on the nhl_players_allseasons() function based on player_id.
  ) %>%
  filter(league.name == "National Hockey League", seasonStart %in% c(2019, 2020, 2021)) %>% # Same filter used in result4, result5, result6 and result7.
  select(fullName, seasonStart, stat.plusMinus) %>% # Selecting the information I want to gather (player name, season, and plus/minus rating).
  as_tibble() %>%
  group_by(fullName) %>%
  ungroup()
result8 # Displays tibble with all the data specified above
 
result8 %>%
  hchart("column", hcaes(seasonStart, stat.plusMinus, group = fullName)) %>% 
  hc_title(
    text = "Plus/Minus Rating from 2019 to 2021") %>%
  hc_add_theme(hc_theme_google()) # Pipes the data contained in result8 to a column chart showcasing the plus/minus rating for each player for the 2019, 2020 & 2021 seasons.

# Task 9: Figuring out who has recorded the highest number of penalty minutes last season (2021) using a pie chart.
result9<- nhl_players(playerNames) %>%
  left_join(
    nhl_players_allseasons(playerNames),
    by = c("id" = "playerId") # Joining the data from the nhl_players() function provided by nhlapi on the nhl_players_allseasons() function based on player_id.
  ) %>%
  filter(league.name == "National Hockey League", seasonStart %in% (2021)) %>% # This time we are filtering by the league being the NHL and by only the 2021 season.
  select(fullName, seasonStart, stat.pim) %>% # Selecting the information I want to gather (player name, season, and penalty minutes).
  as_tibble() %>%
  group_by(fullName) %>%
  ungroup()
result9 # Displays tibble with all the data specified above

slices <- result9$stat.pim # Creates penalty minute number associated with each slice of the pie chart.
labels <- result9$fullName # Creates the labels used for the pie chart
labels <- paste(labels,result9$stat.pim) # Pastes the labels to the pie slices
pie(slices, labels=labels, main= "Pie Chart Showing Number of Penalty Minutes in 2021") # Produces a pie chart showcasing the number of penalty minutes for each player during the 2021 season.


