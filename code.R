install.packages("lubridate")
slice <- read.table("E:/household_power_consumption.txt", 
                header=TRUE, 
                sep=";", 
                na.strings = "?", 
                colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
# Convert the 'Date' column to Date format using the specified format
slice$Date <- as.Date(t$Date, "%d/%m/%Y")
# Filter the data to include only records between "2007-2-1" and "2007-2-2"
slice <- subset(slice, Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
# Remove the 'Date' and 'Time' columns as they are now redundant
slice <- slice[ ,!(names(slice) %in% c("Date","Time"))]
# Create a new column combining the Date and Time information
dateTime <- paste(t$Date, slice$Time)
dateTime <- setNames(dateTime, "DateTime")
# Add the new 'dateTime' column to the dataset
slice <- cbind(dateTime, slice)
# Convert the 'dateTime' column to POSIXct format (for datetime operations in R)
slice$dateTime <- as.POSIXct(dateTime)
# Create a histogram to visualize the distribution of 'Global_active_power'
hist(slice$Global_active_power, 
     main="Global Active Power", 
     xlab = "Global Active Power (kilowatts)", 
     col="red")
#PLOT2
plot(slice$Global_active_power~slice$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

#PLOT3
# Graph settings to display various Sub_metering metrics
plotData <- function(data, y_labels, main_label) {
  # Graph display for Sub_metering_1
  plot(data$dateTime, data$Sub_metering_1, type="l",
       ylab="Global Active Power (kilowatts)", xlab="", main=main_label)
  
  # Adding lines for Sub_metering_2 and Sub_metering_3
  lines(data$dateTime, data$Sub_metering_2, col='Red')
  lines(data$dateTime, data$Sub_metering_3, col='Blue')
  
  # Adding a legend
  legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
         legend=y_labels)
}

# Call function with data 'slice' and labels for legend
with(slice, plotData(slice, c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), "Sub_metering Data"))

#PLOT4
# Set the layout and margins for multiple plots
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))

# Function to create a line plot for a given data column against dateTime
createLinePlot <- function(data, column, y_label) {
  plot(data$dateTime, data[[column]], type="l", ylab=y_label, xlab="")
}

# Plot Global_active_power against dateTime
createLinePlot(slice, "Global_active_power", "Global Active Power (kilowatts)")

# Plot Voltage against dateTime
createLinePlot(slice, "Voltage", "Voltage (volt)")

# Plot Sub_metering_1 against dateTime and add lines for Sub_metering_2 and Sub_metering_3
createLinePlot(slice, "Sub_metering_1", "Global Active Power (kilowatts)")
lines(slice$dateTime, slice$Sub_metering_2, col='Red')
lines(slice$dateTime, slice$Sub_metering_3, col='Blue')

# Add legend for the lines in the plot
legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

# Plot Global_reactive_power against dateTime
createLinePlot(slice, "Global_reactive_power", "Global Reactive Power (kilowatts)")
