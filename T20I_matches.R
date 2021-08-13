library(stringr)
library(lubridate)
library(data.table)
library(jsonlite)

fls <- list.files("data/T20i/", pattern = "[0-9].csv", full.names = TRUE)


bbb <- do.call("rbind", lapply(fls, fread))

unique(bbb$wicket_type)
table(bbb$wicket_type)
class(bbb$start_date)

current_batsmen <- bbb[, .(last_played = max(start_date)), by = .(striker, batting_team)][last_played >= "2021-01-01"]
current_bowlers <- bbb[, .(last_played = max(start_date)), by = .(bowler, bowling_team)][last_played >= "2021-01-01"]

bbb[]

bowler_wicket_type <- c("bowled", "caught", "caught and bowled", "hit wicket", "lbw",
                   "stumped")

fielder_wicket_type <- c("obstructing the field", "run out", "retired out")

bbb[, c("over", "ball") := tstrsplit(ball, '[.]', keep = c(1,2))]
bbb[, `:=` (over = as.numeric(over),
            ball = as.numeric(ball))]
bbb[, over_number := over + 1L]
bbb[, powerplay := between(over_number, 1, 6)]
bbb[, over_ball := ball]
bbb[, ball := NULL]
bbb[, batter_runs_scored := cumsum(runs_off_bat), by = .(striker, batting_team, match_id)]
bbb[, bowler_runs_conceded := cumsum(runs_off_bat), by = .(bowler, bowling_team, match_id)]
bbb[, bowler_wicket := ifelse(wicket_type %in% bowler_wicket_type, 1L, 0L)]
bbb[, fielder_wicket := ifelse(wicket_type %in% fielder_wicket_type, 1L, 0L)]
bbb[, wicket_lost := ifelse(bowler_wicket + fielder_wicket > 0L, 1L, 0L)]


bbb[]

bbstats <- 
  bbb[striker %in% current_batsmen$striker, .(total_runs_scored = sum(runs_off_bat),
          balls_faced = .N,
          SR = sum((runs_off_bat / .N) * 100),
          innings = length(unique(match_id)),
          dismissed_by_bowler = sum(bowler_wicket),
          runs_per_bowler_dismissal = sum(runs_off_bat) / sum(bowler_wicket)), by = .(striker, batting_team)][
            innings >= 20][
              order(-runs_per_bowler_dismissal)]

bbstats


fldout <- bbb[, .(fielder_dismissal = sum(fielder_wicket)), by = player_dismissed]

batdf <- merge.data.table(bbstats, fldout, by.x = "striker", by.y = "player_dismissed", all.x = TRUE)


batdf[, batAvg := total_runs_scored / (dismissed_by_bowler + fielder_dismissal)]

setorder(batdf, -batAvg)
batsmenT50 <- batdf[1:50]

batsmenT50

bowlstats <- 
  bbb[bowler %in% current_bowlers$bowler, .(runs_conceded_bat = sum(runs_off_bat), 
          runs_conceded_extras = sum(wides, na.rm = TRUE) + sum(noballs, na.rm = TRUE) + sum(penalty, na.rm = TRUE),
          total_runs_conceded = sum(runs_off_bat) + sum(wides, na.rm = TRUE) + sum(noballs, na.rm = TRUE) + sum(penalty, na.rm = TRUE),
          balls_bowled = .N,
          ER = sum((runs_off_bat / .N) * 6),
          matches = length(unique(match_id)),
          wickets = sum(bowler_wicket),
          bowlAvg = sum(runs_off_bat) / sum(bowler_wicket)), by = .(bowler, bowling_team)][
            balls_bowled >= 500][
              order(bowlAvg)]

bowlersT50 <- bowlstats[1:50]
bowlersT50



#### Powerplay

bbpp <- 
  bbb[striker %in% current_batsmen$striker & powerplay == TRUE, .(total_runs_scored = sum(runs_off_bat),
                                              balls_faced = .N,
                                              SR = sum((runs_off_bat / .N) * 100),
                                              innings = length(unique(match_id)),
                                              dismissed_by_bowler = sum(bowler_wicket),
                                              pp_outbybowler_pct = sum(bowler_wicket) / length(unique(match_id)),
                                              runs_per_bowler_dismissal = sum(runs_off_bat) / sum(bowler_wicket)), 
      by = .(striker, batting_team)][
        innings >= 20][
          order(-runs_per_bowler_dismissal)]

bbpp


fldppout <- bbb[powerplay == TRUE, .(fielder_dismissal = sum(fielder_wicket)), by = player_dismissed]

batpp <- merge.data.table(bbpp, fldppout, by.x = "striker", by.y = "player_dismissed", all.x = TRUE)


batpp[, batAvg := total_runs_scored / (dismissed_by_bowler + fielder_dismissal)]

setorder(batpp, -batAvg)
batsmenPPT30 <- batpp[1:30]

batsmenPPT30

bowlpp <- 
  bbb[bowler %in% current_bowlers$bowler & powerplay == TRUE, .(runs_conceded_bat = sum(runs_off_bat), 
                                            runs_conceded_extras = sum(wides, na.rm = TRUE) + sum(noballs, na.rm = TRUE) + sum(penalty, na.rm = TRUE),
                                            total_runs_conceded = sum(runs_off_bat) + sum(wides, na.rm = TRUE) + sum(noballs, na.rm = TRUE) + sum(penalty, na.rm = TRUE),
                                            balls_bowled = .N,
                                            ER = sum((runs_off_bat / .N) * 6),
                                            matches = length(unique(match_id)),
                                            wickets = sum(bowler_wicket),
                                            bowlAvg = sum(runs_off_bat) / sum(bowler_wicket)), by = .(bowler, bowling_team)][
                                              balls_bowled >= 180][
                                                order(bowlAvg)]

bowlersPPT30 <- bowlpp[1:30]
bowlersPPT30


#### Excluding Powerplay

bbxp <- 
  bbb[striker %in% current_batsmen$striker & powerplay == FALSE, .(total_runs_scored = sum(runs_off_bat),
                                                                  balls_faced = .N,
                                                                  SR = sum((runs_off_bat / .N) * 100),
                                                                  innings = length(unique(match_id)),
                                                                  dismissed_by_bowler = sum(bowler_wicket),
                                                                  xp_outbybowler_pct = sum(bowler_wicket) / length(unique(match_id)),
                                                                  runs_per_bowler_dismissal = sum(runs_off_bat) / sum(bowler_wicket)), 
      by = .(striker, batting_team)][
        innings >= 20][
          order(-runs_per_bowler_dismissal)]

bbxp


fldxpout <- bbb[powerplay == FALSE, .(fielder_dismissal = sum(fielder_wicket)), by = player_dismissed]

batxp <- merge.data.table(bbxp, fldxpout, by.x = "striker", by.y = "player_dismissed", all.x = TRUE)


batxp[, batAvg := total_runs_scored / (dismissed_by_bowler + fielder_dismissal)]

setorder(batxp, -batAvg)
batsmenXPT30 <- batxp[1:30]

batsmenXPT30

bowlxp <- 
  bbb[bowler %in% current_bowlers$bowler & powerplay == FALSE, .(runs_conceded_bat = sum(runs_off_bat), 
                                                                runs_conceded_extras = sum(wides, na.rm = TRUE) + sum(noballs, na.rm = TRUE) + sum(penalty, na.rm = TRUE),
                                                                total_runs_conceded = sum(runs_off_bat) + sum(wides, na.rm = TRUE) + sum(noballs, na.rm = TRUE) + sum(penalty, na.rm = TRUE),
                                                                balls_bowled = .N,
                                                                ER = sum((runs_off_bat / .N) * 6),
                                                                matches = length(unique(match_id)),
                                                                wickets = sum(bowler_wicket),
                                                                bowlAvg = sum(runs_off_bat) / sum(bowler_wicket)), by = .(bowler, bowling_team)][
                                                                  balls_bowled >= 180][
                                                                    order(bowlAvg)]

bowlersXPT30 <- bowlxp[1:30]
bowlersXPT30
