library(cricketr)
library(data.table)
library(robotstxt)
library(rvest)
library(stringr)

paths_allowed("https://stats.espncricinfo.com/ci/content/records/282912.html")
paths_allowed("https://stats.espncricinfo.com/ci/content/records/282920.html")

content <- read_html("https://stats.espncricinfo.com/ci/content/records/282912.html")
batting <- content |> html_table(fill = TRUE)

bStats <- as.data.table(batting[[1]])

content2 <- read_html("https://stats.espncricinfo.com/ci/content/records/282920.html")
batting2 <- content2 |> html_table(fill = TRUE)
bSR <- as.data.table(batting2[[1]])


content3 <- read_html("https://stats.espncricinfo.com/ci/content/records/283267.html")
bowl1 <- content3 |> html_table(fill = TRUE)
bowlEcon <- as.data.table(bowl1[[1]])

content4 <- read_html("https://stats.espncricinfo.com/ci/content/records/283276.html")
bowl2 <- content4 |> html_table(fill = TRUE)
bowlSR <- as.data.table(bowl2[[1]])

content5 <- read_html("https://stats.espncricinfo.com/ci/content/records/283258.html")
bowl3 <- content5 |> html_table(fill = TRUE)
bowlAvg <- as.data.table(bowl3[[1]])

batsmen <- merge.data.table(bSR, bStats, by = "Player", all.x = TRUE)
bowlers <- merge.data.table(bowlSR, bowlAvg, by = "Player", all.x = TRUE)
bowlers <- merge.data.table(bowlers, bowlEcon, by = "Player", all.x = TRUE)

head(bowlers)
head(batsmen)                            

batsmen[, c("Player", "Country") := tstrsplit(Player, "[()]",fill = TRUE, keep = c(1,2))]
batsmen[, c("From", "To") := tstrsplit(Span.x, "-",fill = TRUE, keep = c(1,2))]
bowlers[, c("Player", "Country") := tstrsplit(Player, "[()]", fill = TRUE, keep = c(1,2))]
bowlers[, c("From", "To") := tstrsplit(Span.x, "-",fill = TRUE, keep = c(1,2))]

batsmen[, `:=` (From = as.integer(From),
                To = as.integer(To))]

bowlers[, `:=` (From = as.integer(From),
                To = as.integer(To))]

batsmen <- batsmen[To >= 2020]
bowlers <- bowlers[To >= 2020]

xcols <- colnames(batsmen)[str_detect(colnames(batsmen), "[.]y$", negate = TRUE)]
xcols
batsmen <- batsmen[, ..xcols]

xcols <- colnames(bowlers)[str_detect(colnames(bowlers), "[.]y$", negate = TRUE)]

bowlers <- bowlers[, ..xcols]
bowlers <- bowlers[, !(Span:`5`)]

ctry <- c("SL", "AUS", "WI", "SA", "ENG", "BDESH", "AFG", "INDIA", "IRE", "ZIM", "PAK", "NZ", "NL")

batsmen <- batsmen[Country %in% ctry]
bowlers <- bowlers[Country %in% ctry]

setorder(bowlers, Econ.x, SR.x)
setorder(batsmen, -Ave.x, -SR.x)

#fwrite(bowlers, "topT20_bowlers.csv")
#fwrite(batsmen, "topT20_batsmen.csv")

merge.data.table(bowlers, batsmen, by = "Player", all.x = FALSE, all.y = FALSE)
