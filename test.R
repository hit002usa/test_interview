raw_data <- read.csv("Data2.csv")
indx <- dim(raw_data)[1]
names(raw_data) <- c("Date.Interval", "Reading")
date_data <- sapply(as.character(raw_data$Date.Interval), strsplit, split = " ")
dec_data <- data.frame(Date = vector("numeric", indx), Interval = vector("numeric", indx), Reading = vector("numeric", indx))
dec_data$Reading <- raw_data$Reading
for (i in 1:indx) {
       dec_data$Date[i] = date_data[[i]][1]
       dec_data$Interval[i] = date_data[[i]][2]
}

missing_reading <- dec_data[is.na(dec_data$Reading), ]

missing_count <- sum(!complete.cases(dec_data))

error_reading <- dec_data[(dec_data$Reading < 0)&(!is.na(dec_data$Reading)), ]

error_count <- sum(dec_data$Reading < 0, na.rm = TRUE

ave_data <- data.frame(day = vector("numeric", 31), ave.reading = vector("numeric", 31))

for (i in 1:31) {
png(paste0(i, "dec.png"))
date_index <- paste0("12/", paste0(i, "/07"))  
lab_dec_data <- dec_data[dec_data$Date == date_index, ]
ave_data$day[i] <- date_index
ave_data$ave.reading[i] <- mean(lab_dec_data$Reading[(lab_dec_data$Reading > 0) & !is.na(lab_dec_data$Reading)])     
plot(1:length(lab_dec_data$Interval), lab_dec_data$Reading, type = "l", col = "red", xlab = "5-min Interval", ylab = "Meter Reading", main = paste("Dec.", i, sep = " "))
dev.off()
}

ave_data$day <- strptime(ave_data$day, "%m/%d/%y")

week_ave_data <- ave_data
png("date_ave_reading.png")
qplot(day, ave.reading, data = ave_data, geom = "point", col = "red", xlab = "Day", ylab = "Average Reading (kWh)", main = "EnerNOC Meter Reading of Dec. 2007")
dev.off()


week_ave_data$day <- weekdays(week_ave_data$day, abbreviate = T)
png("week_ave_reading.png")
qplot(day, ave.reading, data = week_ave_data, geom = "point", col = "red", xlab = "Week", ylab = "Average Reading (kWh)", main = "EnerNOC Meter Reading of Dec. 2007")
dev.off()



