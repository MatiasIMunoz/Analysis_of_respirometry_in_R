# Functions for respirometry analysis

library(pracma)

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
      ccf_res <- ccf(x = as.numeric(xcorr_df[,"FlowRate"]), y = as.numeric(xcorr_df[,ch]), lag.max = lag.max, main = "", plot = F)

      xcorr_negative <- cbind.data.frame(lag = ccf_res$lag, acf = (ccf_res$acf)^2) # Create data frame with lags and cross-correla
      xcorr_negative <-  subset(xcorr_negative, lag <= 0) # Keep only negative lags
      
      plot(xcorr_negative$lag, xcorr_negative$acf, type = "l", xlab = "lag (s)", ylab = "Cross-correlation squared")
      
      polygon(
        x = c(xcorr_negative$lag, rev(xcorr_negative$lag)),
        y = c(xcorr_negative$acf, rep(0, length(xcorr_negative$acf))),
        col = "black",
        border = NA
      )
      
      
      max_lag_index <- which.max(xcorr_negative$acf)
      max_lag <- xcorr_negative$lag[max_lag_index]
      max_lag # find lag
      abline(v = max_lag, col = "red")
      lag_val <- xcorr_negative$lag[max_lag_index]
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
#  3) Stop-flow analysis ----
#*
#*************************************
#df<- read.csv("/Users/matiasvumac/Downloads/Data_06-24-2025_001_DAY_corrected.csv")
#markers_df <- read.csv("/Users/matiasvumac/Downloads/Data_06-24-2025_001_DAY_markers.csv")
#i = 1

stop_flow_analysis <- function(df,
                               markers_df,
                               filename,
                               enclosure_time,
                               nfrogs){
  
  
  # a) Add "Minutes" column to data frame ----
  df$Minutes <- df$Seconds/60
  
  
  # b) Flow rate corrected by WVP ----
  
  # Correct flow rate for water vapor, and add new column called "FlowRate_d":
  # the "_d" stands for "dry", Lighton uses "_c" instead.
  # [FRd = (BP - WVP)/BP] - Eqn. 8.6 (p. 96) of Lighton 2008.
  df$FlowRate_d <- (df$FlowRate*(df$BP - df$WVP))/df$BP
  
  
  # c) O2 ----

  
  ## c.1) Divide by 100 and 2) flip (multiply by -1) ----
  # - Dividing by 100 gets FeO2 - FiO2.
  # - flipping gets FiO2 - FeO2.
  # FiO2 is the fractional concentration of O2 at the beginning [0, 1].
  # FeO2 is the fractional concentration of O2 at the end [0, 1].
  # See Lighton 2008 pg. 38, points 15-17.
  df$O2_div_flip <- (df$O2_lagdriftcorrected/100) * -1
  
  
  ## c.2) Enter respirometry transformation to compute VolO2 ----
  # VolO2 = FR(FiO2 - FeO2)/(1 - FeO2)
  # Note that: (FiO2 - FeO2) is 'O2_div_flip' in the script.
  # Note that: FeO2 = 0.2095 - O2_div_flip, when CO2 in absorbed in a push system.
  # This is what will be integrated later.
  df$VolO2  <- (df$FlowRate_d * df$O2_div_flip)/(1 - (0.2095 - df$O2_div_flip))
  
  
  
  
  # d) CO2 ----
  
  
  ## d.1) Divide by 100 to get FeCO2 - FiCO2 ----
  # FiCO2 is the fractional concentration of CO2 at the beginning [0, 1].
  # FeCO2 is the fractional concentration of CO2 at the end [0, 1].
  df$CO2_div <- (df$CO2_lagdriftcorrected/100)
  
  
  ## d.2) Calculate [FR(FeCO2 - FiCO2)] ----
  df$VolCO2 <- df$FlowRate_d * df$CO2_div
  
  
  
  
  # e) Integrate ----
  
  # Create an empty results data frame
  results <- data.frame(
    Repetition = integer(),
    Markers = integer(),
    Enclosure_time_m = integer(),
    Start_sample = numeric(),
    End_sample = numeric(),
    VolO2_integral = numeric(),
    VolCO2_integral = numeric(),
    Mean_CO2 = numeric(),
    Filename = character(),

    stringsAsFactors = FALSE
  )
  
  
  # Remove markers "B" and repetitions "0" (baseline) and "1" (first round) from the markers df.
  markers_df_subset <- subset(markers_df, Marker != "B" & Repetition != 0 & Repetition != 1)
  

  # Loop through channels 2 to (n_frogs+1)
  for (i in 1:nrow(markers_df_subset)) {
    
    # Obtain the begin and end time of each window i.
    begin_end <- markers_df_subset[i, c("Begin_time", "End_time")]
    
    # Subset the data frame of the window i.
    df_subset <- df[c(begin_end[[1]]:begin_end[[2]]), ]
    
    # Integrate vs. time in minutes and compute mean CO2.
    volO2_int <- trapz(x = df_subset$Minutes, y = df_subset$VolO2)
    volCO2_int <- trapz(x = df_subset$Minutes, y = df_subset$VolCO2)
    mean_CO2 <- mean(df_subset$CO2_lagdriftcorrected, na.rm = TRUE)
    
    # Add results to empty table
    results <- rbind(results, data.frame(
      Repetition = markers_df_subset[i, 4],
      Marker = markers_df_subset[i, 1],
      Enclosure_time_m = (enclosure_time*nfrogs)/60,
      Start_sample = begin_end[[1]],
      End_sample = begin_end[[2]],
      VolO2_integral = volO2_int,
      VolCO2_integral = volCO2_int,
      Mean_CO2 = mean_CO2,
      Filename = as.character(filename),
      stringsAsFactors = FALSE))
  }
  
  
  return(results)
  
  
  }
  

  

