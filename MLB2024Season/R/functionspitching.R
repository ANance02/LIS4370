# Adding team pitching stats function
load_pitching_data <- function() {
  read.csv("data/Team Pitching Stats.csv", stringsAsFactors = FALSE)
}
load_pitching_data()

# Looking up a specific team's pitching stats
get_pitching_stats <- function(team) {
  data <- load_pitching_data()
  subset(data, Team == team)
}
get_pitching_stats("Cleveland Guardians")

# Comparing two teams (pitching stats)
compare_pitching <- function(team1, team2, stat) {
  data <- load_pitching_data()
  data <- subset(data, Team %in% c(team1, team2))
  data[, c("Team", stat)]
}
compare_pitching("Cleveland Guardians", "Oakland Athletics", "LOB")

# Sorting by Pitching Top Teams
top_pitching_teams <- function(stat, n = 5) {
  data <- load_pitching_data()
  data[order(-data[[stat]]), ][1:n, c("Team", stat)]
}
top_pitching_teams("LOB")
