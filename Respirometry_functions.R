# Functions for respirometry analysis


#*************************************
#*
#  1) Lag correction function using cross correlation ----
#*
#*************************************

lag_correct_channels <- function(df,
                                 channels = c("O2", "CO2", "WVP"),
                                 ref = "FlowRate",
                                 window = c(2511, 12579),
                                 lag.max = 200) {
  
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
  
  ccf_results <- list()
    
    par(mfrow = c(1, length(channels)))
    for(ch in channels){
      ccf_res <- ccf(x = as.numeric(xcorr_df[,"FlowRate"]), y = as.numeric(xcorr_df[,ch]), lag.max = lag.max, main = "")
      max_lag_index <- which.max(abs(ccf_res$acf))
      max_lag <- ccf_res$lag[max_lag_index];max_lag # find lag
      abline(v = max_lag, col = "red")
      lag_val <- ccf_res$lag[max_lag_index]
      lags[ch] <- lag_val
      title(paste0(ch," (lag = ", lag_val," seconds)") )
      ccf_results[[ch]] <- ccf_res
    }
    


  list(lags = lags, ccf = ccf_results)

}


#*************************************
#*
#  2) Find midpoint between B and 2 markers ----
#*
#*************************************

mid_B_to_2 <- function(x) {
  nms <- names(x)
  i <- which(head(nms, -1) == "B" & tail(nms, -1) == "2")
  round((x[i] + x[i + 1]) / 2)
}

#*************************************
#*
#  3) USELESS Drift correction ----
#*
#*************************************


correct_O2_drift <- function(df, time_col = "Seconds", O2_col = "O2", 
                             marker_col = "Marker", baseline_marker = 50, 
                             O2_ref = 20.95) {
  # Identify baseline points
  baseline_idx <- df[[marker_col]] == baseline_marker
  Seconds_baseline <- df[[time_col]][baseline_idx]
  O2_baseline <- df[[O2_col]][baseline_idx]
  
  # Fit Catmull-Rom spline through baseline points
  baseline_spline <- splinefun(Seconds_baseline, O2_baseline, method = "fmm")
  
  # Predict baseline for all times
  df$O2_drift <- baseline_spline(df[[time_col]])
  
  # Correct the O2 signal and shift to reference
  df$O2_corrected <- df[[O2_col]] - (df$O2_drift - O2_ref)
  
  return(df)
}
