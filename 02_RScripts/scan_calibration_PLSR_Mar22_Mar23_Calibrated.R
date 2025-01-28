#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Coder: Kaci Zarek
#Date: 5/20/2024
#Goal: Calibrate and correct scan data from TAL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#1.0 --- Set up Workspace ------------------------------------------------------
#Load packages
remove(list = ls())
library(spectrolab)
library(pls)
library(data.table)
library(xts)
library(readxl)
library(devtools)
library(tidyverse)

#2.0 -- Upload scan data with spectra ------------------------------------------
file.path <- "01_tidydata/"
scandat <- read_csv("01_tidydata/TAL_TLM01_scan_Mar22_Mar23_compensated.csv",
                    na = c("", "NA"),
                    col_types = cols(grabNO3_mgL= col_double(), grabDOC_mgL= col_double(),
                                     grabTSS_mgL = col_double(),
                                     grab = col_character(),
                                     datetime = col_datetime(format = "%m/%d/%Y %H:%M"))) # make sure this matches your non-detects
scandat <- force_tz(scandat, tz = "America/Chicago")
scandat <- scandat %>% 
  filter(datetime >= "2022-03-19 00:00:00" & datetime <= "2023-03-20 23:45:00")

# Upload the raw scan-generated concentration data, spectra, and your grab samples
# This makes them a time-stamped object
scan.NO3 = xts(scandat$NO3_mgL_scan, as.POSIXct(scandat$datetime, format = "%m/%d/%Y %H:%M:%S"))
scan.spec = xts(scandat[10:220], as.POSIXct(scandat$datetime, format = "%m/%d/%Y %H:%M")) 
# select full spectra
# note here that if there are 0s in your spectra, this code will throw an error
# so only use the wavelengths where you have detectable absorbance
grab.NO3 = xts(scandat$grabNO3_mgL, as.POSIXct(scandat$datetime, format = "%m/%d/%Y %H:%M")) 

#2.0 ---- See how scan data did relative to grab sample conc -------------------
grabspec <- scandat %>% as_tibble() %>% filter(grab == "Y")# Ony gets data when there is a Y
grab.NO3 <- grabspec$grabNO3_mgL
spec.NO3 <- grabspec$NO3_mgL_scan

#---NO3 1:1 line---
plot(grab.NO3 ~ spec.NO3)
calib.mod.NO3 = lm(grab.NO3 ~ spec.NO3)
summary(calib.mod.NO3) ###need PLSR for scan since NOT great 1:1 line

#3.0 ---- Create matrices of GRAB spectral data (training dataset) -------------
# 1. Index data set with columns with absorbances
grab.spec.dat <- grabspec[10:220] # Full spectra

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
abs = (grab.spec.dat)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wl = as.numeric(colnames(abs))
#str(wl)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrow = as.numeric(nrow(abs))
Num = c(1:lastrow)

# 5. Create the final matrix 
grab.matrix = cbind(abs)
rownames(grab.matrix) = as.numeric(Num)
colnames(grab.matrix) = as.numeric(wl)

grab.matrix = as.matrix(grab.matrix)
str(grab.matrix)
attributes(grab.matrix)

# 6. Make this into spectral matrix for model
# Must be in format: grab.spectra = spectra(value = abs, bands = wl, names = Num)
grab.spectra = spectra(value
                       = abs, bands = wl, names = Num)
attributes(grab.spectra)
plot(grab.spectra) # Note = bands here = absorbance from the scans

#grab.spectra = as_spectra.list(grab.spectra, wave_unit = "wavenumber", measurement_nit = "absorbance")
grab.spectra = as.matrix(grab.spectra)

# Change attributes so this is correct for scan data
attr(grab.spectra, 'wave_unit') = 'wavelength'
attr(grab.spectra, 'measurement_unit') = 'absorbance'
attributes(grab.spectra)

#4.0 --Create matrices of ALL spectral data (raw data that needs calibration)---

# 1. Index FULL dataset with columns with absorbances
scan.spec = scandat[10:220]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
abs = (scan.spec)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wl = as.numeric(colnames(abs))

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrow = as.numeric(nrow(abs))
Num = c(1:lastrow)

# 5. Create the final matrix 
scan.matrix = cbind(abs)
rownames(scan.matrix) = as.numeric(Num)
colnames(scan.matrix) = as.numeric(wl)

scan.matrix = as.matrix(scan.matrix)
spec = spectra(value = abs, bands = wl, names = Num)
#head(spec)
plot(spec) # Note = reflectance here = absorbance from the scans

# NOTE: this is where you can identify problem spectra & remove them
# potential fix
#spec <- spec %>% 
 # as_tibble() %>%
  #pivot_longer(!sample_name) %>% 
  #mutate(value = if_else(value<0, NA, value)) %>% 
  #pivot_wider() %>% 
  #na.omit()

# = as.spectra.list(spec)
scan.spectra = as.matrix(spec)
str(scan.spectra)
attr(scan.spectra, 'wave_unit') = 'wavelength'
attr(scan.spectra, 'measurement_unit') = 'absorbance'
attributes(scan.spectra)
plot(scan.spectra)
#5.0 -- Create a new dataframe with the spectral matrices ----------------------

# This creates a dataframe with 
# 1. NO3 (regression calibrated)
# 2. NO3 (regression calibrated)
# 3. Full s::can spectra (from 200-700nm)

# NOTE: We use the I() function to protect the Spectra 
spectralcal.df = data.frame(NO3 = scan.NO3,  Spectra = I(scan.spectra))
str(spectralcal.df)

# Also do this for the GRAB sample data
grabcal.df = data.frame(NO3 = grab.NO3, Spectra = I(grab.spectra))
str(grabcal.df)

#6.0 -- Develop PLSR training dataset-------------------------------------------

# Create a training and test dataset
CTrain = grabcal.df
CTest = spectralcal.df

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Cmod <- plsr(NO3 ~ Spectra, ncomp = 13, data = CTrain, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod)

# Plot RMSE of the predictions to optimize model
##model with the number of components that produced the 
##minimum RMSEP was chosen as the model to use for calibration. 
plot(RMSEP(Cmod), legendpos = "bottomright", lwd = 2, 
     xlab = "Number of Components",
     ylab = "Root Mean Square Error of Prediction",
     main = "Number of Components for Nitrate",
     cex.lab =1.5,
     cex.axis = 1.5) #ncomp = 1
R2(Cmod)

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error 
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod, ncomp = 1, asp = 1, line = TRUE,
     lwd = 2,
     pch = 19,
     xlab = "Lab Measured Nitrate",
     ylab = "PLSR Predicted Nitrate",
     main = "Predicted vs. Lab Measured Nitrate",
     cex.lab =1.5,
     cex.axis = 1.5)
plot(Cmod, ncomp = 2, asp = 1, line = TRUE)

#7.0 -- Make predictions based on reduced-error PLSR model----------------------

# Predict model!
predictedN = predict(Cmod, ncomp = 1, newdata = spectralcal.df) # use reduced error model
str(predictedN)

# Plot final predictions
plot(predictedN)

write.csv(predictedN, file = "01_tidydata/TAL_NO3predicted_Mar22_Mar23_1ncomp_2.csv") # <- this is your newly calibrated dataset!

#8.0 ---- Import data set with predicted No3 values from PLSR ------------------
df <- read_csv("01_tidydata/TAL_NO3predicted_Mar22_Mar23_1ncomp.csv")

#Make datetime in UTC 
#Ensure central timezone
df <- df %>% mutate(datetime = mdy_hm(datetime_local))
df1 <- df %>% force_tz(datetime, tzone = "America/Chicago")
#Make it in UTC to match Nitrate data
df2 <- df1 %>% with_tz(datetime, tzone = "UTC")

#Rename datetime column to say UTC
df2 <- rename(df2, datetime_UTC = datetime)

#Save new data frame with both datetimes in UTC and CST 
write_csv(df2, file = "01_tidydata/TAL_NO3predicted_April22_Mar23_WY(1ncomp)_UTCtimezone.csv") 
# ^ this is your newly calibrated data set!
