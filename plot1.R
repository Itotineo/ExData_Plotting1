d <- read.table("household_power_consumption.txt",
                header=TRUE, sep=";", na.strings = "?",
                colClasses = c('character','character',
                               'numeric','numeric','numeric',
                               'numeric','numeric','numeric','numeric'))

## Format date to Type Date
d$Date <- as.Date(d$Date, "%d/%m/%Y")

## Filter data set from Feb. 1, 2007 to Feb. 2, 2007
d <- subset(d,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## Remove incomplete observation
d <- d[complete.cases(d),]

## Combine Date and Time column
dateTime <- paste(d$Date, d$Time)

## Name the vector
dateTime <- setNames(dateTime, "DateTime")

## Remove Date and Time column
d <- d[ ,!(names(d) %in% c("Date","Time"))]

## Add DateTime column
d <- cbind(dateTime, d)

## Format dateTime Column
d$dateTime <- as.POSIXct(dateTime)

## Plot 1
hist(d$Global_active_power, main="Global Active Power",
     xlab = "Global Active Power (kilowatts)", col="red")

#Save file plot1.png and close device
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()