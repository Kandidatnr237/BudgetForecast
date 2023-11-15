# Test data
dat <- df1 |> filter( str_detect(cat, "samlet 50"))

# Graphical look
plot( x = dat$date, y = dat$value, "ln")

# Tesing for stationarity

## Arima auto
auto.arima( y = dat$value, seasonal = T)

model_diff <- auto.arima( y = diff(df$value) )

acf(model_diff$residuals)



# Testing -----------------------------------------------------------------

acf(dat$value)
acf(diff(dat$value))

walk( group_split(df1, cat), \(x) acf(x$value))


