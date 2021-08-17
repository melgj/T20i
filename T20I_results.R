library(data.table)
library(lubridate)
library(stringr)

mInfo <- fread("data/T20i/all_match_details.csv", header = FALSE)

colnames(mInfo) <- c("match_id", "description", "value")

mInfo[, match_id := str_remove_all(match_id, "[^0-9]")]

df <- dcast(mInfo, match_id ~ description, fun.aggregate = toString)

df[, c("Team_1", "Team_2") := tstrsplit(team, ",")]
setnames(df, "team", "fixture")

df[, loser := ifelse(winner == "", NA,
                     ifelse(winner == Team_1,
                            Team_2, Team_1))]

setcolorder(df, c("match_id", "date", "fixture", "Team_1", "Team_2",
                  "winner", "loser", "winner_runs", "winner_wickets", "method",
                  "outcome", "toss_winner", "toss_decision", "event"))

fwrite(df, "data/T20i/match_results_clean.csv")
