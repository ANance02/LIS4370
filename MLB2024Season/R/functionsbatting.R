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

# Show Batting Abbreviations, allowing for full list or specific searching.
batting_abbreviations <- function(abbreviation = NULL) {
  bat_abbrev <- data.frame(
    Abbreviation = c("Bat", "BatAge", "R/G", "G", "PA", "AB", "R", "H",
                     "X2B", "X3B", "HR", "RBI", "SB", "CS", "BB", "SO", "BA"),
    Meaning = c(
      "Number of batters used in game",
      "Average age of all batters",
      "Runs Scored Per Game",
      "Games played",
      "Plate Appearances",
      "At Bats",
      "Runs Scored",
      "Hits Allowed",
      "Doubles Hit",
      "Triples Hit",
      "Home Runs Hit",
      "Runs Batted In",
      "Stolen Bases",
      "Caught Stealing",
      "Bases on Balls or Walks",
      "Strikeouts",
      "Batting Average (Hits/At Bats)"
    ),
    stringsAsFactors = FALSE
  )

  if (is.null(abbreviation)) {
    print(bat_abbrev, row.names = FALSE)
    invisible(bat_abbrev)
  } else {
    match_row <- bat_abbrev[bat_abbrev$Abbreviation == abbreviation, ]
    if (nrow(match_row) == 0) {
      message("Abbreviation not found. Please check your input.")
      invisible(NULL)
    } else {
      print(match_row, row.names = FALSE)
      invisible(match_row)
    }
  }
}
batting_abbreviations()

# Plots Chosen Batting Data into a Simple Bar Plot
plot_batting_stat <- function(stat) {
  data <- load_batting_data()

  if (!(stat %in% names(data))) {
    stop(paste("Stat", stat, "not found in batting data."))
  }

  barplot(height = data[[stat]],
          names.arg = data$Team,
          las = 2,
          cex.names = 0.7,
          main = paste("Teams by Batting", stat),
          col = "lightgreen",
          ylab = stat)
}
plot_batting_stat("HR")

# Allows the ability to pull all data for a single team
team_summary <- function(team) {
  batting <- get_batting_stats(team)
  pitching <- get_pitching_stats(team)
  fielding <- get_fielding_stats(team)

  list(
    Batting = batting,
    Pitching = pitching,
    Fielding = fielding
  )
}
team_summary("Baltimore Orioles")
