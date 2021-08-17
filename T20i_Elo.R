library(data.table)
library(lubridate)
library(elo)

df <- fread("data/T20i/match_results_clean.csv")

unique(df$outcome)

edf <- df[outcome != "no result"]

edf <- edf[, .(date, Team_1, Team_2, winner, outcome)]

edf[, result := ifelse(winner == Team_1,
                       1, ifelse(winner == Team_2,
                                 0, 0.5))]

str(edf)

edf[, `:=`(winner = NULL,
           outcome = NULL)]
setorder(edf, date)

ratings <- elo.run(result ~ Team_1 + Team_2, data = edf, initial.elos = 1500, k = 30)

as.data.table(final.elos(ratings), keep.rownames = TRUE)[order(-V2)]

sort(final.elos(ratings), decreasing = TRUE)





