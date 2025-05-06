## TEST ------

library(ggplot2)
library(mrbin)

# Load your custom functions
source("C://Users//juguibert//Documents//Function_test//Read_2DNMR_spectrum.R")
source("C://Users//juguibert//Documents//Function_test//Peak_picking.R")
source("C://Users//juguibert//Documents//Function_test//Integration.R")
source("C://Users//juguibert//Documents//Function_test//Vizualisation.R")


### Melange 1.25 ----

bruker_folder <- "C:/Users/juguibert/Documents/Spectres_TOCSY_melange/MTH_melange_1.25mM/5"

bruker_fdr <- "C:/Users/juguibert/Documents/240202131_project/240202131_ech/240202131-melange/180700367-melange/7"

test_UF <- "C:/Users/juguibert/Documents/240202131_Spectres_Julien_230601884_liver_apolar/4/pdata/1"
test_COSY <- "C:/Users/juguibert/Documents/MTH_melange_1.25mM/4/pdata/1"
test_TOCSY <- "C:/Users/juguibert/Documents/MTH_melange_1.25mM/5/pdata/1"
test_HSQC <- "C:/Users/juguibert/Documents/MTH_melange_1.25mM/6/pdata/1"
test_cosy_chris <- "C:/Users/juguibert/Documents/MTH_melange_1.25mM/4/pdata/1"
test_hs_chris <- "C:/Users/juguibert/Documents/panoramics-main/Data/MTH-Plasma-NIST-090416/9"
test_cec <- "C:/Users/juguibert/Documents/240202131_project/240202131_ech/240202131-melange/180700367-melange/6/pdata/1"
test_foie <- "C:/Users/juguibert/Documents/240202131_project/240202131_ech/240202131_Spectres_foie_2024/Spectres_UF_foie/222/pdata/1"
test_Cosy_mel <- "C:/Users/juguibert/Documents/240202131_project/240202131_ech/240202131-melange/180700367-melange/5/pdata/1"

test_Tocsy_cpl <- "C:/Users/juguibert/Documents/240202131_project/240202131_ech/240202131_TOCSY_2D_polar/230601884_MTH7_01_2D_TOCSY_polar/230601884_MTH7_01_2D_TOCSY_polar/1/pdata/1"


test_TOCSY_20 <- "C:/Users/juguibert/Documents/Spectres_TOCSY_melange/MTH_melange_20mM/5/pdata/1"

bruker_data <- read_bruker(test_TOCSY, dim = "2D")

peak_results <- peak_pick_2d_nt2(bruker_data$spectrumData, threshold = 1500, threshold_type = "noise")

peak_results2 <- peak_pick_2d_nt_optimized(bruker_data$spectrumData, threshold = 200, threshold_type = "noise")


find_nmr_peak_centroids(
  bruker_data$spectrumData,
  spectrum_type = "TOCSY",
  intensity_threshold = 3000,
  contour_start = 100000,
  contour_num = 70,
  contour_factor = 1.3,
  f2_exclude_range = c(4.7, 5.0)
)


df <- peak_results

as.data.frame(data$spectrumData) -> datae

# Get the 90th percentile (i.e., bottom value of the top 10%)
intensity_threshold <- quantile(datae, 0.9995, na.rm = TRUE)

# Result: this is the minimal intensity among the top 10% values
intensity_threshold

# filtered_df <- df[df$F2_ppm <= 7.8 & df$F2_ppm >= 0 & df$F1_ppm <= 7.8 & df$F1_ppm >= 0 , ]

filtered_df <- df[df$F1_ppm <= 4.7 | df$F1_ppm >= 5.1, ]

# Calculate peak volumes
peak_volumes <- calculate_peak_volumes(bruker_data$spectrumData, filtered_df, gran = 200, c.vol = TRUE)

# Display results
print(peak_volumes)

test1 <- cmp$matched
test1 <- test1[,1:2]
colnames(test1) <- c("F2_ppm", "F1_ppm")
unmatched_list <- cmp2$unmatched[cmp2$unmatched$Source == "Manual", c("F2_ppm", "F1_ppm")]

p <- plot_peaks2(bruker_data$spectrumData, peak_list = NULL, unmatched_list = NULL, intensity_threshold = 1200000)
print(p)

p <- plot_peaks2(bruker_data$spectrumData, peak_list = test1, unmatched_list = unmatched_list, intensity_threshold = 320000)
print(p)

p2 <- plot_peaks2(Test_C, intensity_threshold = 5000000)
print(p2)

pI <- plot_contour_peaks(bruker_data_test, intensity_threshold = 2.99e+29)
print(pI)

### Melange 2.5 ----

bruker_folder_test <- "C:/Users/juguibert/Documents/Spectres_TOCSY_melange/MTH_melange_2.5mM/5"


bruker_data_test2 <- read_bruker_file(bruker_folder_test)

peak_results2 <- peak_pick_2d_nt(bruker_data_test2, threshold = 95)

df2 <- peak_results2

filtered_df2 <- df2[df2$F2_ppm <= 7.8 & df2$F2_ppm >= 0 & df2$F1_ppm <= 7.8 & df2$F1_ppm >= 0, ]

# Calculate peak volumes
peak_volumes2 <- calculate_peak_volumes(bruker_data_test2, filtered_df2, gran = 200, c.vol = TRUE)

# Display results
print(peak_volumes2)

p2 <- plot_peaks(bruker_data_test2, intensity_threshold = 2.95e+29)
print(p2)


## Function ----

normalize_data <- function(data, method = "dynamic", YMAX = NULL, YMIN = NULL) {
  data<- data[!is.finite(data)] <- 0
  if (method == "metadata" && !is.null(YMAX) && !is.null(YMIN)) {
    return((data - YMIN) / (YMAX - YMIN))
  } else if (method == "dynamic") {
    return(data / max(abs(data), na.rm = TRUE))
  } else {
    stop("Invalid normalization method or missing metadata.")
  }
}
bruker_data_test$zub <- bruker_data_test$rr_data / max(abs(bruker_data_test$rr_data))




sigma_bruit <- sd(as.vector(bruker_data$rr_data), na.rm = TRUE)  # Estimate noise level

k <- 3  # Set threshold multiplier

# Apply the noise-based threshold
bruker_data$rr_data[bruker_data$rr_data < k * sigma_bruit] <- NA




I_max <- max(as.vector(bruker_data$rr_data), na.rm = TRUE)  # Get maximum intensity

p <- 0.1  # Set percentage threshold

# Apply the percentage-based threshold
bruker_data$rr_data[bruker_data$rr_data < p * I_max] <- NA
