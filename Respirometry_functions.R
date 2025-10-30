# Functions for respirometry analysis


#*************************************
#*
# Lag correction
#*
#*************************************


lag_correct_channels <- function(df,
                                 channels = c("O2", "CO2", "WVP"),
                                 ref = "FlowRate",
                                 window = c(2511, 12579)) {
  
  # Create new dataframe
  df_lagcorr <- df
  
  # Scale selected columns
  df_lagcorr$O2 <- scale(df_lagcorr$O2)
  df_lagcorr$CO2 <- scale(df_lagcorr$CO2)
  df_lagcorr$BP <- scale(df_lagcorr$BP)
  df_lagcorr$WVP <- scale(df_lagcorr$WVP)

  # Define a window to run cross correlation (in seconds)
  xcorr_df <- df_lagcorr[c(window[[1]]:window[[2]]),]
  
  
  # Empty df to store lags
  lags <- numeric(length(channels))
  names(lags) <- channels
  
  # Set up 3-panel figure
  par(mfrow = c(1, length(channels)))
  
  # Compute lag for each channel
  for(ch in channels){
    ccf_res <- ccf(x = as.numeric(xcorr_df[,"FlowRate"]), y = as.numeric(xcorr_df[,ch]), lag.max = 500, main = "")
    max_lag_index <- which.max(abs(ccf_res$acf))
    max_lag <- ccf_res$lag[max_lag_index];max_lag # find lag
    abline(v = max_lag, col = "red")
    lag_val <- ccf_res$lag[max_lag_index]
    lags[ch] <- lag_val
    title(paste0(ch," (lag = ", lag_val," seconds)") )
    
  }
  print(lags)

}

