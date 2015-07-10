hist(as.numeric(data$Global_active_power)/1000 * 1.98, xlim=c(0,8), col='red', main="Global Active Power", xlab='Global Active Power (kilowatts)', xaxt='n', yaxt='n')
axis(1, at=c(0,2,4,6))
axis(2, at=c(0,200,400,600,800,1000,1200))

