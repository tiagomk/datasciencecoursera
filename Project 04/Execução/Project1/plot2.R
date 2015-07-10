plot(
  strptime(paste(as.character(data$Date), as.character(data$Time), sep=' '), format="%Y-%m-%d %H:%M:%S"),
  as.numeric(data$Global_active_power) / 1000 * 1.98,
  type='l'
)
