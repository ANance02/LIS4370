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

# Show Fielding Abbreviations, allowing for full list or specific searching.
fielding_abbreviations <- function(abbreviation = NULL) {
  field_abbrev <- data.frame(
    Abbreviation = c("#Fld", "RA/G", "DefEff", "G", "GS", "CG", "Inn", "Ch",
                     "PO", "A", "E", "DP"),
    Meaning = c(
      "Number of Fielders",
      "Runs Allowed per Game",
      "Defensive Efficiency",
      "Games Played",
      "Games Started",
      "Complete Game",
      "Innings Played in Field",
      "Defensive Chances (PO + A + E)",
      "Putouts",
      "Assists",
      "Errors",
      "Double Plays"
    ),
    stringsAsFactors = FALSE
  )

  if (is.null(abbreviation)) {
    print(field_abbrev, row.names = FALSE)
    invisible(field_abbrev)
  } else {
    match_row <- field_abbrev[field_abbrev$Abbreviation == abbreviation, ]
    if (nrow(match_row) == 0) {
      message("Abbreviation not found. Please check your input.")
      invisible(NULL)
    } else {
      print(match_row, row.names = FALSE)
      invisible(match_row)
    }
  }
}
fielding_abbreviations()

# Plots Chosen Fielding Data into a Simple Bar Plot
plot_fielding_stat <- function(stat) {
  data <- load_fielding_data()

  if (!(stat %in% names(data))) {
    stop(paste("Stat", stat, "not found in fielding data."))
  }

  barplot(height = data[[stat]],
          names.arg = data$Team,
          las = 2,
          cex.names = 0.7,
          main = paste("Teams by Fielding", stat),
          col = "lightcoral",
          ylab = stat)
}
plot_fielding_stat("DP")
