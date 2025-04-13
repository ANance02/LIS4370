# Adding team batting stats function
load_batting_data <- function() {
  read.csv("data/Team Batting Stats.csv", stringsAsFactors = FALSE)
}
load_batting_data()

# Looking up a specific team's batting stats
get_batting_stats <- function(team) {
  data <- load_batting_data()
  subset(data, Team == team)
}
get_batting_stats("Cleveland Guardians")

# Comparing two teams (batting stats)
compare_batting <- function(team1, team2, stat) {
  data <- load_batting_data()
  data <- subset(data, Team %in% c(team1, team2))
  data[, c("Team", stat)]
}
compare_batting("Cleveland Guardians", "Oakland Athletics", "AB")

# Sorting by Batting Top Teams
top_batting_teams <- function(stat, n = 5) {
  data <- load_batting_data()
  data[order(-data[[stat]]), ][1:n, c("Team", stat)]
}
top_batting_teams("AB")
