## Export Data into csv table

date = as.character(Sys.Date())

location = paste("./history/", date, ".csv", sep = "")

write.csv(elo_table, location)

rm(list = c("date", "location"))

