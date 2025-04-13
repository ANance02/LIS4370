# Adding team fielding stats function
load_fielding_data <- function() {
  read.csv("data/Team Fielding Stats.csv", stringsAsFactors = FALSE)
}
load_fielding_data()

# Looking up a specific team's fielding stats
get_fielding_stats <- function(team) {
  data <- load_fielding_data()
  subset(data, Team == team)
}
get_fielding_stats("Cleveland Guardians")

# Comparing two teams (fielding)
compare_fielding <- function(team1, team2, stat) {
  data <- load_fielding_data()
  data <- subset(data, Team %in% c(team1, team2))
  data[, c("Team", stat)]
}
compare_fielding("Cleveland Guardians", "Oakland Athletics", "E")

# Sorting by Fielding Top Teams
top_fielding_teams <- function(stat, n = 5) {
  data <- load_fielding_data()
  data[order(-data[[stat]]), ][1:n, c("Team", stat)]
}
top_fielding_teams("E")
