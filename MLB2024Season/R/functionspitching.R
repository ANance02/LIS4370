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

# Show Pitching Abbreviations, allowing for full list or specific searching.
pitching_abbreviations <- function(abbreviation = NULL) {
  pitch_abbrev <- data.frame(
    Abbreviation = c("#P", "PAge", "RA/G", "W", "L", "W-L%", "ERA", "G", "GS", "GF",
                     "CG", "tSho", "cSho", "SV", "IP", "H", "R", "ER", "HR", "BB",
                     "SO", "HBP", "BK", "WP", "BF", "LOB"),
    Meaning = c(
      "Number of Pitchers Used in Game",
      "Average Age of All Pitchers",
      "Runs Allowed per Game",
      "Wins",
      "Losses",
      "Win-Loss Percentage",
      "Earned Run Average (9 * ER/IP)",
      "Games Played",
      "Games Started",
      "Games Finished",
      "Complete Game",
      "Shutouts by Team",
      "Shutouts by a Single Pitcher",
      "Saves",
      "Innings Pitched",
      "Hits Allowed",
      "Runs Allowed",
      "Earned Runs Allowed",
      "Home Runs Allowed",
      "Bases on Balls or Walks",
      "Strikeouts",
      "Times Hit by Pitch",
      "Balks",
      "Wild Pitches",
      "Batters Faced",
      "Runners Left on Base"
    ),
    stringsAsFactors = FALSE
  )

  if (is.null(abbreviation)) {
    print(pitch_abbrev, row.names = FALSE)
    invisible(pitch_abbrev)
  } else {
    match_row <- pitch_abbrev[pitch_abbrev$Abbreviation == abbreviation, ]
    if (nrow(match_row) == 0) {
      message("Abbreviation not found. Please check your input.")
      invisible(NULL)
    } else {
      print(match_row, row.names = FALSE)
      invisible(match_row)
    }
  }
}
pitching_abbreviations()

# Plots Chosen Pitching Data into a Simple Bar Plot
plot_pitching_stat <- function(stat) {
  data <- load_pitching_data()

  if (!(stat %in% names(data))) {
    stop(paste("Stat", stat, "not found in pitching data."))
  }

  barplot(height = data[[stat]],
          names.arg = data$Team,
          las = 2,
          cex.names = 0.7,
          main = paste("Teams by Pitching", stat),
          col = "lightblue",
          ylab = stat)
}
plot_pitching_stat("SO")
