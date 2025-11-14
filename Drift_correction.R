#**********************************************
#
# Try to get records drift corrected
#
#**********************************************
library(ggplot2)
library(interpolators)
library(dplyr)
library(tidyr)


folder <- "/data/"
filename <- "Data_06-24-2025_001_DAY.txt"

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

# Markers
marker_times <- df$Seconds[df$Marker != -1];length(marker_times)
marker_names <- c(rep(c("B", as.character(2:(nfrogs+1))), nreps+1), "B", "B");length(marker_names)
names(marker_times) <- marker_names
marker_times

# All B's and 2's
Bs_2s <- marker_times[names(marker_times) == "B" | names(marker_times) == "2"]
Bs_2s_df <- cbind.data.frame(Repetition = rep(1:(length(Bs_2s)/2), each = 2) , Marker = rep(c("Begin", "End"), (length(Bs_2s)/2)), Seconds =  Bs_2s)
Bs_2s_df_wide <- pivot_wider(Bs_2s_df, names_from = Marker, values_from = Seconds)
Bs_2s_df_wide$Midpoint <- round((Bs_2s_df_wide$End + Bs_2s_df_wide$Begin)/2)
Bs_2s_df_wide <- as.data.frame(Bs_2s_df_wide)


prop <- 0.1
Bs_2s_df_wide$Low25 <- round(Bs_2s_df_wide[,"Begin"] + (Bs_2s_df_wide[,"End"] - Bs_2s_df_wide[,"Begin"])*((1-prop)/2))
Bs_2s_df_wide$Up75 <- round(Bs_2s_df_wide[,"End"] - (Bs_2s_df_wide[,"End"] - Bs_2s_df_wide[,"Begin"])*((1-prop)/2))

Bs_2s_df_wide
str(df)

# Compute mean O2 for each repetition
mean_O2_df <- Bs_2s_df_wide %>%
  rowwise() %>%
  mutate(mean_O2 = mean(df$O2[df$Seconds >= Low25 & df$Seconds <= Up75], na.rm = TRUE)) %>%
  ungroup()

mean_O2_df <- as.data.frame(mean_O2_df)


#********************************************************
#* Forsythe, Malcolm and Moler spline
#********************************************************

# Fit a  Forsythe, Malcolm and Moler spline
baseline_spline <- splinefun(mean_O2_df$Midpoint, mean_O2_df$mean_O2, method = "fmm")

# Predict baseline for all times
df$O2_drift <- baseline_spline(df$Seconds)
head(df)

# Correct O 2 and span to 20.95%
df$O2_corrected <- df$O2 - (df$O2_drift - 20.95)
head(df)

ggplot(df, aes(x = Seconds)) +
  geom_line(aes(y = O2), color = "gray50") +
  geom_line(aes(y = O2_drift), color = "red", linetype = "dashed") +
  geom_line(aes(y = O2_corrected), color = "blue") +
  geom_hline(yintercept = 20.95, col = "red", linetype = "dashed") +
  geom_vline(xintercept = marker_times, color = "darkred", linetype = "solid", alpha = 0.4)+
  geom_point(data = mean_O2_df, aes(x = Midpoint, y = mean_O2), col = "red", size = 2)+
  theme_minimal()


##################### END ######################## ----
# Catmull-Rom test
fit_catmull <- iprCatmullRom(as.matrix(cbind(mean_O2_df$Midpoint, mean_O2_df$mean_O2)))
curve_catmull <- evalInterpolator(fit_catmull, seq(0, 1, length.out = (26159)))
curve_catmull <- cbind.data.frame("x"= curve_catmull[,1], "y" = curve_catmull[,2])

head(curve_catmull)
str(curve_catmull)

curve_catmull2 <- as.data.frame(curve_catmull2)
head(curve_catmull2)

# Add corrected O2 to data frame
length(df$O2[60:26219])
nrow(curve_catmull2)
df$catmull_O2 <- df$O2[60:26219] - curve_catmull2[,2]

# Simple plot
ggplot(df, aes(x = Seconds, y = O2))+
  geom_line()+
  #geom_vline( xintercept = mean_O2_df$Begin, col = "red", alpha = 0.5)+
  #geom_vline( xintercept = mean_O2_df$End, col = "red", alpha = 0.5)+
  geom_point(data = mean_O2_df, aes(x = Midpoint, y = mean_O2), col = "blue")+
  geom_line(data = curve_catmull2, aes(x = x_int, y = y), col = "blue")+
  theme_bw()

#********************************************************
# Polynomial drift correction (e.g. quadratic, cubic, etc...) ----
#********************************************************
poly_order <- 3
fit_poly <- lm(O2 ~ poly(Seconds, poly_order), data = df)
df$signal_corrected <- df$O2 - predict(fit_poly)

ggplot(df, aes(x = Seconds)) +
  geom_line(aes(y = O2), color = "gray50") +
  geom_line(aes(y = predict(fit_poly)), color = "red", linetype = "dashed") +
  geom_line(aes(y = signal_corrected+20.95), color = "blue") +
  theme_minimal() +
  geom_hline(yintercept = 20.95, col = "red", linetype = "dashed")

#********************************************************
#* Natural spline
#********************************************************
fit_spline <- lm(O2 ~ ns(Seconds, df = 10), data = df)  # natural spline
df$signal_corrected_spline <- df$O2 - predict(fit_spline)


ggplot(df, aes(x = Seconds)) +
  geom_line(aes(y = O2), color = "gray50") +
  geom_line(aes(y = predict(fit_spline)), color = "red", linetype = "dashed") +
  geom_line(aes(y = signal_corrected_spline+20.95), color = "blue") +
  theme_minimal() +
  geom_hline(yintercept = 20.95, col = "red", linetype = "dashed")

#********************************************************
# Catmull-Rom drift correction ----
#********************************************************
library(interpolators)

str(df)
fit_catmull <- iprCatmullRom(as.matrix(cbind(df$Seconds, df$O2)))
curve_catmull <- evalInterpolator(fit_catmull, seq(0, 1, length.out = 10000))

plot(curve_catmull, type = "l", lwd = 0.7)

# ---------------------- END -------------------------

install.packages("PathInterpolatR")

library(interpolators)

test_df <- data.frame(
  x = c(0, 1, 3, 4, 6, 7),
  y = c(0, 2, 1, 3, 2, 4)
)

plot(test_df)
ipr <- iprCatmullRom(as.matrix(test_df))
iprO2 <- iprCatmullRom(as.matrix(cbind(df$Seconds, df$O2)))

s <- seq(0, 1, length.out = 10000)
Curve <- evalInterpolator(iprO2, s)
head(Curve)
plot(Curve, type = "l", lwd = 2)
points(test_df, pch = 19)
