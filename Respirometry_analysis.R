#test
###-###-###-###-###-###-###-###-###-###-
#
# 0) Script for the analysis of respirometry data.
#
###-###-###-###-###-###-###-###-###-###-

#Load libraries
library(ggplot2) # for general plotting
library(pracma) # for numerical integration
library(viridis) # for color blind friendly palettes
library(dplyr) # for data manipulation
#library(seewave)

source("Respirometry_functions.R")

my.theme <-theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold"), 
        legend.position="none",
        strip.background = element_rect(colour = "black", fill = "white"), 
        strip.text.x = element_text(colour = "black", face = "bold"),
        plot.tag = element_text(face = "bold"))


#******************************************
#
# 1) Load data frame ----
#
#******************************************

# Data frame has been lag and drift corrected in ExpeData. No other changes made.
# All channels were exported as .txt file.

# Folder containing the .txt files exported from ExpeData.
folder <- "/data/"

# Name of the file.
#filename <- read.csv("data/Data_06-24-2025_001_DAY_LagDrift.csv")
#str(filename_test)
filename <- "Data_06-24-2025_001_DAY.txt"
filename <- "Data_05-09-2025_001_NIGHT.txt"
#filename <- "Data_06-29-2025_001_DAY_LagDrift.txt" # n_frogs = 7, n_reps = 4
#filename <- "Data_06-24-2025_001_DAY_LagDrift.txt" #n_frogs = 5, n_reps = 6
#filename <- "Data_06-27-2025_001_DAY_LagDrift.txt" #n_frogs = 7, n_reps = 4
#filename <- "Data_06-26-2025_001_DAY_SETUP1_LagDrift.txt" #n_frogs = 7, n_reps = 4
#filename <- "Data_06-27-2025_001_NoPlaybackRMR_LagDrift.txt" #n_frogs = 5, n_reps = 3
#filename <- "Data_06-28-2025_001_LagDrift.txt" #n_frogs = 5, n_reps = 6
#filename <-  "Data_06-28-2025_001_SETUP2_LagDrift.txt" #n_frogs = 4, n_reps = 5
#filename <-  "Data_06-30-2025_001_RMROnly_LagDrift.txt" #n_frogs = 2, n_reps = 4

# Load data frame.
df <- read.table(paste0(getwd(),folder, filename), sep = "\t", header = TRUE)
str(df)
summary(df)

# Convert Seconds to Minutes and add new column called "Minutes"
df$Minutes <- df$Seconds/60

# Change marker at t=0 to 50, so its recognized as a marker "B" later.
df[1, 8] <- 50
head(df)
# Define number of frogs and repetitions
nfrogs <- 5  
nreps <- 7

marker_times <- df$Seconds[df$Marker != -1]
marker_names <- c(rep(c("B", as.character(2:(nfrogs+1))), nreps+1), "B", "B")
length(marker_times)
length(marker_names)

#marker_names[length(marker_names)] <- "B"  # set last one to "B"
names(marker_times) <- marker_names
marker_times


# chatgpt markers ----
mt <- marker_times   # your named vector

# Extract marker names and times
Marker      <- names(mt)
Begin_time  <- mt
End_time    <- c(mt[-1], NA)        # shift left for end times
Begin_time  <- Begin_time + c(0, rep(1, length(mt)-1))  # add +1 except first

# Build data frame
markers_df <- data.frame(
  Marker = Marker,
  Begin_time = Begin_time,
  End_time = End_time
)

rep(c(1:nfrogs), nreps)
# Remove last row (no end time)
markers_df <- markers_df[-nrow(markers_df), ]

markers_df

nrow(markers_df)

reps_vector <- c(rep(0:nreps, rep_len(c(nfrogs+1), nreps+1)), NA)
reps_vector

markers_df <- cbind.data.frame(markers_df, Repetition = reps_vector)

markers_df

unique(markers_df$Marker)

cols
cols <- setNames(cols, unique(markers_df$Marker))

length(marker_times)


ggplot(df)+
  #geom_vline(xintercept = marker_times, col = "grey50")+
  geom_line(aes(x = Seconds, y = O2), color = "black") +
  geom_rect(data = markers_df, 
            aes(xmin = Begin_time, xmax = End_time, ymin = -Inf, ymax = Inf, fill = Marker), 
            alpha = 0.2) +
  scale_fill_manual(values = c("B" = "red", "2" = "blue", "3" = "green", 
                               "4" = "purple", "5" = "orange", "6" = "yellow")) +
  my.theme


ggplot(df)+
  geom_vline(xintercept = marker_times, col = "grey50")+
  geom_line(aes(x = Seconds, y = O2), color = "black") +
  my.theme

#********************************
#*
# 2) Correct lag / cross-correlation ----
#*
#********************************

#first, automatically select a window for the cross-correlation
b_idx <- which(names(marker_times) == "B") # Find indices of all "B" markers
xcorr_window <- marker_times[c((b_idx[3]),(b_idx[4]))]# Select 2nd and 3rd "B"


ggplot(df)+
  geom_vline(xintercept = marker_times, col = "grey80")+
  annotate(geom = "text",x = marker_times,y = 21.6,label = names(marker_times),vjust = -0,hjust = 0, size = 4,color = "grey80") +
  geom_line(aes(x = Seconds, y = O2), color = alpha("black", 0.5)) +
  #geom_line(aes(x = Seconds + lags_ch[[1]], y = O2), color = alpha("red", 0.5)) +
  lims(x = c(min(df$Seconds), max(df$Seconds)))+
  labs(x = "Time (s)", y = "O2")+
  annotate("rect", xmin =  xcorr_window[[1]], xmax = xcorr_window[[2]], ymin = -Inf, ymax = Inf,alpha = 0.2, fill = "red")+
  my.theme
  
xcorr_test <- ccf(x = as.numeric(df[,"FlowRate"]), y = as.numeric(df[,"O2"]), lag.max = 200, main = "", plot = TRUE)

xcorr_df <- cbind.data.frame(lag = xcorr_test$lag, acf = (xcorr_test$acf)^2)
xcorr_df <-  subset(xcorr_df, lag <= 0)

max_lag_index <- which.max((xcorr_df$acf))
max_lag <- xcorr_test$lag[max_lag_index];max_lag # find lag

plot(acf ~lag,data = xcorr_df, type = "l")

abline(v = max_lag, col = "red")



lags_ch <- lag_correct_channels(df)

ggplot(df)+
  geom_vline(xintercept = marker_times, col = "grey80")+
  annotate(geom = "text",x = marker_times,y = 21.6,label = names(marker_times),vjust = -0,hjust = 0, size = 5,color = "grey80") +
  geom_line(aes(x = Seconds, y = O2), color = alpha("black", 0.5)) +
  geom_line(aes(x = Seconds + lags_ch[[1]], y = O2), color = alpha("red", 0.5)) +
  lims(x = c(xcorr_window[[1]], xcorr_window[[2]]))+
  labs(x = "Time (s)", y = "O2")+
  theme_bw()


lags_ch$lags
df$O2_lagcorr <- df$O2 + lags_ch[[1]]



# Create df_2 with scaled variables
df_lagcorr <- df

# Scale selected columns
cols_to_scale <- c("O2", "CO2", "BP", "WVP", "FlowRate")
df_lagcorr[cols_to_scale] <- scale(df[cols_to_scale])

# Define a window to run cross correlation (in seconds)
xcorr_ini <-2511
xcorr_end <-12579

xcorr_df <- df_lagcorr[c(xcorr_ini:xcorr_end),]

## Lag correct O2 ----
ggplot(xcorr_df)+
  geom_line(aes(x = Seconds, y = O2), color = alpha("grey50", 0.5)) +
  geom_line(aes(x = Seconds, y = FlowRate), color = "blue")+
  theme_bw()

ccf_O2 <- ccf(y = as.numeric(xcorr_df$O2), 
              x = as.numeric(xcorr_df$FlowRate), 
              lag.max = 500)

ccf_O2$acf
ccf_O2$lag

plot(ccf_O2$lag, (ccf_O2$acf)^1, type = "l")
# Add shaded area under the curve
polygon(
  x = c(ccf_O2$lag, rev(ccf_O2$lag)),
  y = c((ccf_O2$acf)^1, rep(0, length(ccf_O2$acf))),
  col = "black",   # semi-transparent blue
  border = NA
)


max_lag_index <- which.max(abs(ccf_O2$acf))
max_lag <- ccf_O2$lag[max_lag_index];max_lag # find lag
abline(v = max_lag, col = "red")


ggplot(xcorr_df[c(1:6000),])+
  geom_line(aes(x = Seconds, y = O2), color = alpha("grey50", 0.5)) +
  geom_line(aes(x = Seconds+max_lag, y = O2), color =alpha("red", 0.5))+
  geom_line(aes(x = Seconds, y = FlowRate), color = "blue")+
  theme_bw()


## Lag correct CO2 ----
ggplot(xcorr_df)+
  geom_line(aes(x = Seconds, y = CO2_scaled, color = "black")) +
  geom_line(aes(x = Seconds, y = FR_scaled, color = "blue"))+
  theme_bw()


ccf_CO2 <- ccf(y = as.numeric(xcorr_df$CO2_scaled), 
               x = as.numeric(xcorr_df$FR_scaled), 
               lag.max = 500)
max_lag_index <- which.max(abs(ccf_CO2$acf))
max_lag <- ccf_CO2$lag[max_lag_index];max_lag # find lag
abline(v = max_lag, col = "red")


ggplot(xcorr_df[c(1:2000),])+
  geom_line(aes(x = Seconds, y = CO2_scaled), color = alpha("grey50", 0.5)) +
  geom_line(aes(x = Seconds+max_lag, y = CO2_scaled), color =alpha("red", 0.5))+
  geom_line(aes(x = Seconds, y = FR_scaled), color = "blue")+
  theme_bw()

## Lag correct WVP ----
ggplot(xcorr_df)+
  geom_line(aes(x = Seconds, y = WVP_scaled, color = "black")) +
  geom_line(aes(x = Seconds, y = FR_scaled, color = "blue"))+
  theme_bw()


ccf_WVP <- ccf(y = as.numeric(xcorr_df$WVP_scaled), 
               x = as.numeric(xcorr_df$FR_scaled), 
               lag.max = 500)
max_lag_index <- which.max(abs(ccf_WVP$acf))
max_lag <- ccf_WVP$lag[max_lag_index];max_lag # find lag
abline(v = max_lag, col = "red")


ggplot(xcorr_df[c(1:2000),])+
  geom_line(aes(x = Seconds, y = WVP_scaled), color = alpha("grey50", 0.5)) +
  geom_line(aes(x = Seconds+max_lag, y = WVP_scaled), color =alpha("red", 0.5))+
  geom_line(aes(x = Seconds, y = FR_scaled), color = "blue")+
  theme_bw()



# Inspect the recording visually
# Figure of O2 peaks
ggplot(df, aes(x = Seconds, y = O2))+
  geom_line()+
  # geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

# Figure of CO2 peaks
ggplot(df, aes(x = Seconds, y = CO2))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

# Figure of WVP
ggplot(df, aes(x = Seconds, y = WVP))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

# Figure of flow rate
ggplot(df, aes(x = Seconds, y = FlowRate))+
  geom_line()+
  theme_bw()

#********************************
#*
# 2) Correct drift ----
#* https://pmc.ncbi.nlm.nih.gov/articles/PMC2886696/#B5
#********************************
nfrogs <- 5
nreps <- 7


as.character(2:(nfrogs+1))

marker_times <- df$Seconds[df$Marker != -1]
names <- rep(c("B", as.character(2:(nfrogs+1))), nreps+1)

names(marker_times) <- names

ggplot(df, aes(x = Seconds, y = O2))+
  geom_line()+
  #lims(x = c(0, 2000))+
  geom_vline(xintercept = marker_times, col = "red", alpha = 0.5)+
  #geom_hline(yintercept = 20.95, linetype = "dashed")+
  theme_bw()


library(mgcv)
fit_gam <- gam(O2 ~ s(Seconds), data = baseline_df)
df$O2_drift <- predict(fit_gam, newdata = df)
df$O2_corrected <- df$O2 - (df$O2_drift - 20.95)





# Polynomial drift (e.g. quadratic)
poly_order <- 3
fit_poly <- lm(O2 ~ poly(Seconds, poly_order), data = df)
df$signal_corrected <- df$O2 - predict(fit_poly)

# GAM drift
library(mgcv)
fit_gam <- gam(O2 ~ s(Seconds, k = 10), data = df)
df$signal_corrected <- df$O2 - predict(fit_gam)

ggplot(df, aes(x = Seconds)) +
  geom_line(aes(y = O2), color = "gray50") +
  #geom_line(aes(y = predict(fit_gam)), color = "darkred", linetype = "dashed") +
  geom_line(aes(y = predict(fit_poly)), color = "red", linetype = "dashed") +
  #geom_line(aes(y = signal_corrected), color = "blue") +
  theme_minimal()

ggplot(df, aes(x = Seconds)) +
  geom_line(aes(y = O2), color = "gray50") +
  geom_line(aes(y = predict(fit_poly)), color = "red", linetype = "dashed") +
  geom_line(aes(y = signal_corrected+20.95), color = "blue") +
  theme_minimal() +
  geom_hline(yintercept = 20.95, col = "red", linetype = "dashed")

###-###-###-###-###-###-###-###-###-###-
#
# 2) Define basic parameters ----
# THIS IS IMPORTANT ----
#
###-###-###-###-###-###-###-###-###-###-
n_frogs <-  5 # number of frogs in the respirometer
n_reps <- 6 # number of repetitions EXCLUDING first round
sampling_window_s <- 600 # length of sampling window, in samples (= seconds)
first_marker_s <- ((120*(n_frogs+1)) + (sampling_window_s*(n_frogs+2))) # calculate the sample of the first marker of interest (in samples = seconds)
enclosure_mins <- n_frogs*10 # Ten minutes per channel

# Color palette
cols <- viridis::turbo(n_frogs)

###-###-###-###-###-###-###-###-###-###-
# Markers for respirometry analysis ----
###-###-###-###-###-###-###-###-###-###-

marker_times <- df$Seconds[df$Marker != -1]
marker_names <- c(rep(c("B", as.character(2:(nfrogs+1))), nreps+1), "B", "B")
names(marker_times) <- marker_names
marker_times



###-###-###-###-###-###-###-###-###-###-
#
# 3) Transform flow rate, O2, CO2 ----
#
###-###-###-###-###-###-###-###-###-###-

###-###-###-
## 3.1) Flow rate corrected by WVP ----
###-###-###-
# Correct flow rate for water vapor, and add new column called "FlowRate_d":
# the "_d" stands for "dry", Lighton uses "_c" instead.
# [FRd = (BP - WVP)/BP] - Eqn. 8.6 (p. 96) of Lighton 2008.
df$FlowRate_d <- (df$FlowRate*(df$BP - df$WVP))/df$BP

ggplot(df) +
  geom_line(aes(x = Seconds, y = FlowRate, color = "Uncorrected")) +
  geom_line(aes(x = Seconds, y = FlowRate_d, color = "Corrected")) +
  scale_color_manual(name = "", values = c("Uncorrected" = "black", "Corrected" = "red")) +
  theme_bw()

###-###-###-
## 3.2) O2 ----
###-###-###-

# 1) Divide by 100 and 2) flip (multiply by -1)
# - Dividing by 100 gets FeO2 - FiO2.
# - flipping gets FiO2 - FeO2.
# FiO2 is the fractional concentration of O2 at the beginning [0, 1].
# FeO2 is the fractional concentration of O2 at the end [0, 1].
# See Lighton 2008 pg. 38, points 15-17.
df$Trans_O2 <- (df$O2/100) * -1

ggplot(df, aes(x = Seconds, y = Trans_O2))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

# Enter respirometry transformation to compute VolO2 
# VolO2 = FR(FiO2 - FeO2)/(1 - FeO2)
# Note that: (FiO2 - FeO2) is 'Trans_O2' in the script.
# Note that: FeO2 = 0.2095 - Trans_O2, when CO2 in absorbed in a push system.
# This is what will be integrated later.
df$VolO2  <- (df$FlowRate_d * df$Trans_O2)/(1 - (0.2095 - df$Trans_O2))

ggplot(df, aes(x = Seconds, y = VolO2))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()


###-###-###-
## 3.3) CO2 ----
###-###-###-

# Divide by 100 to get FeCO2 - FiCO2
# FiCO2 is the fractional concentration of CO2 at the beginning [0, 1].
# FeCO2 is the fractional concentration of CO2 at the end [0, 1].
df$Trans_CO2 <- (df$CO2/100)

ggplot(df, aes(x = Seconds, y = Trans_CO2))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

# Calculate [FR(FeCO2 - FiCO2)]
df$VolCO2 <- df$FlowRate_d * df$Trans_CO2

ggplot(df, aes(x = Seconds, y = VolCO2))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()



###-###-###-###-###-###-###-###-###-###-
#
# 4) Extract start and end samples for peaks ----
#
###-###-###-###-###-###-###-###-###-###-
peaks_Ch2 <- list()

# Loop to generate peak markers from peak1_Ch2 to peak7_Ch2 
for (i in 1:n_reps) {
  if (i == 1) {
    # For the first peak
    start_peak <- first_marker_s
    end_peak <- first_marker_s + sampling_window_s + 1
  } else {
    # For subsequent peaks
    start_peak <- peaks_Ch2[[i - 1]][2] + sampling_window_s * n_frogs - 1
    end_peak <- peaks_Ch2[[i - 1]][2] + sampling_window_s * n_frogs + 600
  }
  
  # Store the calculated peak range
  peaks_Ch2[[i]] <- c(start_peak, end_peak)
}

# Output the peaks list
peaks_Ch2

# Compute peaks for channel 3, 4, 5, 6, 7 and 8 using lapply (just add 600 to the previous channels).
peaks_Ch3 <- lapply(peaks_Ch2, function(p) p + sampling_window_s)
peaks_Ch4 <- lapply(peaks_Ch3, function(p) p + sampling_window_s)
peaks_Ch5 <- lapply(peaks_Ch4, function(p) p + sampling_window_s)
peaks_Ch6 <- lapply(peaks_Ch5, function(p) p + sampling_window_s)
peaks_Ch7 <- lapply(peaks_Ch6, function(p) p + sampling_window_s)
peaks_Ch8 <- lapply(peaks_Ch7, function(p) p + sampling_window_s)


###-###-###-###-###-###-###-###-###-###-
#
# 5) Plot peaks (for checking) ----
#
###-###-###-###-###-###-###-###-###-###-

# Turn values in list to data frame
peaks_Ch2_df <- do.call(rbind, peaks_Ch2); colnames(peaks_Ch2_df) <- c("start", "end"); peaks_Ch2_df <- as.data.frame(peaks_Ch2_df)
peaks_Ch3_df <- do.call(rbind, peaks_Ch3); colnames(peaks_Ch3_df) <- c("start", "end"); peaks_Ch3_df <- as.data.frame(peaks_Ch3_df)
peaks_Ch4_df <- do.call(rbind, peaks_Ch4); colnames(peaks_Ch4_df) <- c("start", "end"); peaks_Ch4_df <- as.data.frame(peaks_Ch4_df)
peaks_Ch5_df <- do.call(rbind, peaks_Ch5); colnames(peaks_Ch5_df) <- c("start", "end"); peaks_Ch5_df <- as.data.frame(peaks_Ch5_df)
peaks_Ch6_df <- do.call(rbind, peaks_Ch6); colnames(peaks_Ch6_df) <- c("start", "end"); peaks_Ch6_df <- as.data.frame(peaks_Ch6_df)
peaks_Ch7_df <- do.call(rbind, peaks_Ch7); colnames(peaks_Ch7_df) <- c("start", "end"); peaks_Ch7_df <- as.data.frame(peaks_Ch7_df)
peaks_Ch8_df <- do.call(rbind, peaks_Ch8); colnames(peaks_Ch8_df) <- c("start", "end"); peaks_Ch8_df <- as.data.frame(peaks_Ch8_df)

# Figure of peaks that will be analyzed.
peaks_plot <-
ggplot(df, aes(x = Seconds, y = VolO2))+
  ggtitle(label = filename)+
  geom_rect(data = peaks_Ch2_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = cols[1], alpha = 0.2, inherit.aes = FALSE)+
  geom_rect(data = peaks_Ch3_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = cols[2], alpha = 0.2, inherit.aes = FALSE)+
  geom_rect(data = peaks_Ch4_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = cols[3], alpha = 0.2, inherit.aes = FALSE)+
  geom_rect(data = peaks_Ch5_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = cols[4], alpha = 0.2, inherit.aes = FALSE)+
  geom_rect(data = peaks_Ch6_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = cols[5], alpha = 0.2, inherit.aes = FALSE)+
  geom_rect(data = peaks_Ch7_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = cols[6], alpha = 0.2, inherit.aes = FALSE)+
  geom_rect(data = peaks_Ch8_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = cols[7], alpha = 0.2, inherit.aes = FALSE)+
  geom_line()+
  geom_hline(yintercept = 0, col = "black", linetype = "dashed")+
  theme_bw()
peaks_plot

# Save plot.
#ggsave(filename = paste0(getwd(),"/figures/",filename, "_peaks_plot.png"), 
 #      peaks_plot, width = 15, height = 5)

###-###-###-###-###-###-###-###-###-###-
#
# 6) Integrate VolO2, VolCO2, and mean CO2 ----
#
###-###-###-###-###-###-###-###-###-###-
markers_df

subset(markers_df, Marker != "B" & Repetition != 0 & Repetition != 1)



subset(markers_df, Marker == "3" & Repetition != 0 & Repetition != 1)
subset(markers_df, Marker == "4" & Repetition != 0 & Repetition != 1)
subset(markers_df, Marker == "5" & Repetition != 0 & Repetition != 1)
subset(markers_df, Marker == "6" & Repetition != 0 & Repetition != 1)

# Create an empty results data frame
results <- data.frame(
  Peak = integer(),
  VolO2_integral = numeric(),
  VolCO2_integral = numeric(),
  Mean_CO2 = numeric(),
  Channel = character(),
  Filename = character(),
  Start_sample = numeric(),
  End_sample = numeric(),
  stringsAsFactors = FALSE
)

ch=2
# Loop through channels 2 to (n_frogs+1)
for (ch in 2:(n_frogs+1)) {
  
  # Construct the variable name, e.g., "peaks_Ch2"
  peaks_var <- get(paste0("peaks_Ch", ch))
  
  i=1
  # Loop through each peak (i) in the current channel (ch)
  for (i in seq_along(peaks_var)) {
    
    start_idx <- peaks_var[[i]][1]
    end_idx <- peaks_var[[i]][2]
    
    # Subset the data for the current peak
    # trapz() function need the data to be subset before analysis
    peak_data <- df[start_idx:end_idx, ]
    
    # Compute integrals (vs time in minutes) and mean CO2.
    volO2_int <- trapz(peak_data$Minutes, peak_data$VolO2)
    volcO2_int <- trapz(peak_data$Minutes, peak_data$VolCO2)
    mean_cO2 <- mean(peak_data$CO2, na.rm = TRUE)
    
    # Add results to empty table
    results <- rbind(results, data.frame(
      Peak = i,
      VolO2_integral = volO2_int,
      VolCO2_integral = volcO2_int,
      Mean_CO2 = mean_cO2,
      Channel = paste0("Channel_", ch),
      Filename = filename,
      Start_sample = start_idx,
      End_sample = end_idx,
      stringsAsFactors = FALSE
    ))
  }
}

# View the results
results
str(results)

###-###-###-###-###-###-###-###-###-###-
#
# 7) Calculate VO2 and VCO2 ----
#
###-###-###-###-###-###-###-###-###-###-

## 7.1) Divide VolO2 by enclosure time  ----
# VO2 = VolO2/enclosure_time
results$VO2 <- (results$VolO2_integral/enclosure_mins)


## 7.2) Calculate VCO2  ----
# VCO2 = FR(FeCO2 - FiCO2) - FeCO2*VO2)/(1 - FeCO2)
results$VCO2 <- ((results$VolCO2_integral/enclosure_mins) - results$Mean_CO2*results$VO2)/(1 - results$Mean_CO2)


## 7.3) Transform to hourly rates (multiply VO2 and VCO2 by 60) ----
results$VO2_mlhr <- results$VO2*60
results$VCO2_mlhr <- results$VCO2*60

results

mean(results$VO2_mlhr)
## 7.4) Export results----
write.csv(results, file = paste(getwd(), "/R_output/",filename, "_Routput.csv", sep = ""),)

# 8) Calculate averages per channel (i.e., per individual) ----
# This could be easily done in Excel from the file created above in
# point 7.4), but it may be faster in R. 
results$Channel <- as.factor(results$Channel)
str(results)


mean_results <- results %>%
  group_by(Channel) %>%
  summarise(
    mean_VO2_mlhr = mean(VO2_mlhr, na.rm = TRUE),
    mean_VCO2_mlhr = mean(VCO2_mlhr, na.rm = TRUE),
    file = filename
  )
mean_results

## 8.1) Export MEAN results----
write.csv(mean_results, file = paste(getwd(), "/R_output/",filename, "_MEAN_Routput.csv", sep = ""),)

