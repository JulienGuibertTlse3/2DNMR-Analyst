# Load necessary library

library(tidyr)


#### Peak picking ----

##### Peaklist ----

library(dplyr)
compare_peak_lists <- function(df1, df2, tolerance_F1 = 0.005, tolerance_F2 = 0.005) {
  required_cols <- c("F2_ppm", "F1_ppm")
  if (!all(required_cols %in% colnames(df1))) stop("df1 must contain 'F2_ppm' and 'F1_ppm' columns.")
  if (!all(required_cols %in% colnames(df2))) stop("df2 must contain 'F2_ppm' and 'F1_ppm' columns.")
  
  df1 <- df1 %>% mutate(F2_ppm = as.numeric(F2_ppm), F1_ppm = as.numeric(F1_ppm)) %>% drop_na(F2_ppm, F1_ppm)
  df2 <- df2 %>% mutate(F2_ppm = as.numeric(F2_ppm), F1_ppm = as.numeric(F1_ppm)) %>% drop_na(F2_ppm, F1_ppm)
  
  all_peaks <- data.frame(F2_ppm_auto = numeric(0), F1_ppm_auto = numeric(0),
                          F2_ppm_manual = numeric(0), F1_ppm_manual = numeric(0), Matched = character(0))
  
  df2_copy <- df2 
  
  for (i in seq_len(nrow(df1))) {
    if (nrow(df2) == 0) break
    
    best_match <- df2 %>%
      mutate(diff_F2 = abs(F2_ppm - df1$F2_ppm[i]),
             diff_F1 = abs(F1_ppm - df1$F1_ppm[i])) %>%
      filter(diff_F2 <= tolerance_F2 & diff_F1 <= tolerance_F1)
    
    if (nrow(best_match) > 0) {
      best_match <- best_match %>% arrange(diff_F2 + diff_F1) %>% slice(1)
      
      match_entry <- data.frame(F2_ppm_auto = df1$F2_ppm[i],
                                F1_ppm_auto = df1$F1_ppm[i],
                                F2_ppm_manual = best_match$F2_ppm,
                                F1_ppm_manual = best_match$F1_ppm,
                                Matched = "Yes")
      
      all_peaks <- rbind(all_peaks, match_entry)
      
      df2 <- df2 %>% filter(!(F2_ppm == best_match$F2_ppm & F1_ppm == best_match$F1_ppm))
    } else {
      unmatched_entry <- data.frame(F2_ppm_auto = df1$F2_ppm[i],
                                    F1_ppm_auto = df1$F1_ppm[i],
                                    F2_ppm_manual = NA,
                                    F1_ppm_manual = NA,
                                    Matched = "No")
      all_peaks <- rbind(all_peaks, unmatched_entry)
    }
  }
  
  if (nrow(df2) > 0) {
    unmatched_manual <- df2 %>%
      mutate(F2_ppm_auto = NA, F1_ppm_auto = NA, Matched = "No") %>%
      select(F2_ppm_auto, F1_ppm_auto, F2_ppm_manual = F2_ppm, F1_ppm_manual = F1_ppm, Matched)
    
    all_peaks <- rbind(all_peaks, unmatched_manual)
  }
  
  return(list(all_peaks = all_peaks))
}

# write.csv(matched_peaks, file = file.path(output_dir, "matched_peaks.csv"), row.names = FALSE)
# write.csv(unmatched_peaks, file = file.path(output_dir, "unmatched_peaks.csv"), row.names = FALSE)

##### Confidence ----

compare_peak_lists_cf <- function(df1, df2, tolerance = 0.02) {
  # Ensure required columns exist
  required_cols <- c("F2_ppm", "F1_ppm", "Confidence")
  if (!all(required_cols %in% colnames(df1))) {
    stop("df1 must contain 'F2_ppm', 'F1_ppm', and 'Confidence' columns.")
  }
  if (!all(c("F2_ppm", "F1_ppm") %in% colnames(df2))) {
    stop("df2 must contain 'F2_ppm' and 'F1_ppm' columns.")
  }
  
  # Convert columns to numeric and remove non-numeric values
  df1 <- df1 %>% mutate(F2_ppm = as.numeric(F2_ppm), F1_ppm = as.numeric(F1_ppm), Confidence = as.numeric(Confidence)) %>% drop_na(F2_ppm, F1_ppm, Confidence)
  df2 <- df2 %>% mutate(F2_ppm = as.numeric(F2_ppm), F1_ppm = as.numeric(F1_ppm)) %>% drop_na(F2_ppm, F1_ppm)
  
  # Initialize empty dataframes for matches and unmatched peaks
  matched_peaks <- data.frame(F2_ppm_auto = numeric(0), F1_ppm_auto = numeric(0),
                              F2_ppm_manual = numeric(0), F1_ppm_manual = numeric(0),
                              Confidence = numeric(0), Matched = character(0))
  
  unmatched_peaks <- data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), Confidence = numeric(0), Source = character(0))
  
  complete_peaks <- data.frame(F2_ppm_auto = numeric(0), F1_ppm_auto = numeric(0),
                               F2_ppm_manual = numeric(0), F1_ppm_manual = numeric(0),
                               Confidence = numeric(0), Matched = character(0))
  
  for (i in seq_len(nrow(df1))) {
    if (nrow(df2) == 0) break
    
    best_match <- df2 %>%
      mutate(
        diff_F2 = abs(F2_ppm - df1$F2_ppm[i]),
        diff_F1 = abs(F1_ppm - df1$F1_ppm[i])
      ) %>%
      filter(diff_F2 <= tolerance & diff_F1 <= tolerance)
    
    if (nrow(best_match) > 0) {
      best_match <- best_match %>% arrange(diff_F2 + diff_F1) %>% slice(1)  # Pick the closest match
      
      match_entry <- data.frame(
        F2_ppm_auto = df1$F2_ppm[i],
        F1_ppm_auto = df1$F1_ppm[i],
        F2_ppm_manual = best_match$F2_ppm,
        F1_ppm_manual = best_match$F1_ppm,
        Confidence = df1$Confidence[i],
        Matched = "Yes"
      )
      
      matched_peaks <- rbind(matched_peaks, match_entry)
      complete_peaks <- rbind(complete_peaks, match_entry)
      
      df2 <- df2 %>% filter(!(F2_ppm == best_match$F2_ppm & F1_ppm == best_match$F1_ppm))
    } else {
      unmatched_entry <- data.frame(
        F2_ppm = df1$F2_ppm[i],
        F1_ppm = df1$F1_ppm[i],
        Confidence = df1$Confidence[i],
        Source = "Auto"
      )
      unmatched_peaks <- rbind(unmatched_peaks, unmatched_entry)
    }
  }
  
  # Add all unmatched peaks from df2 to complete_peaks
  if (nrow(df2) > 0) {
    unmatched_manual <- df2 %>%
      mutate(F2_ppm_auto = NA, F1_ppm_auto = NA, Confidence = NA, Matched = "No") %>%
      select(F2_ppm_auto, F1_ppm_auto, F2_ppm_manual = F2_ppm, F1_ppm_manual = F1_ppm, Confidence, Matched)
    
    complete_peaks <- rbind(complete_peaks, unmatched_manual)
    unmatched_peaks <- rbind(unmatched_peaks, df2 %>% mutate(Confidence = NA, Source = "Manual") %>% select(F2_ppm, F1_ppm, Confidence, Source))
  }
  
  return(list(matched = matched_peaks, unmatched = unmatched_peaks, complete = complete_peaks))
}

##### Df ----

library(dplyr)

compare_dataframes <- function(df1, df2, tolerance = 0.02) {
  # Ensure both dataframes have the same column names
  if (!identical(sort(names(df1)), sort(names(df2)))) {
    stop("Dataframes have different column names!")
  }
  
  # Sort columns to ensure consistent order
  df1 <- df1 %>% select(sort(names(.)))
  df2 <- df2 %>% select(sort(names(.)))
  
  # Convert numeric columns to numeric (avoids factor/string issues)
  numeric_cols <- names(df1)[sapply(df1, is.numeric)]
  df1[numeric_cols] <- lapply(df1[numeric_cols], as.numeric)
  df2[numeric_cols] <- lapply(df2[numeric_cols], as.numeric)
  
  # Find exact matches
  exact_matches <- inner_join(df1, df2, by = names(df1))
  
  # Find rows in df1 that are missing in df2
  missing_in_df2 <- anti_join(df1, df2, by = names(df1))
  
  # Find rows in df2 that are missing in df1
  extra_in_df2 <- anti_join(df2, df1, by = names(df1))
  
  # Handle tolerance for numerical differences
  if (length(numeric_cols) > 0) {
    numeric_comparison <- full_join(df1, df2, by = setdiff(names(df1), numeric_cols), suffix = c("_df1", "_df2")) %>%
      mutate(across(all_of(numeric_cols), ~ abs(get(paste0(cur_column(), "_df1")) - get(paste0(cur_column(), "_df2"))))) %>%
      filter(if_any(all_of(numeric_cols), ~ . > tolerance)) %>%
      select(all_of(setdiff(names(df1), numeric_cols)), ends_with("_df1"), ends_with("_df2"))
  } else {
    numeric_comparison <- data.frame()  # No numeric columns to compare
  }
  
  # Summary message
  summary_message <- paste(
    "✅ Dataframes Comparison Summary:\n",
    "- Exact matches:", nrow(exact_matches), "\n",
    "- Missing in df2:", nrow(missing_in_df2), "\n",
    "- Extra in df2:", nrow(extra_in_df2), "\n",
    "- Numeric differences exceeding tolerance:", nrow(numeric_comparison), "\n"
  )
  
  # Return results as a list
  return(list(
    summary = summary_message,
    exact_matches = exact_matches,
    missing_in_df2 = missing_in_df2,
    extra_in_df2 = extra_in_df2,
    numeric_differences = numeric_comparison
  ))
}




#### Test ----

data <- read.csv("C:/Users/juguibert/Documents/test_tocsy.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)
data1 <- read.csv("C:/Users/juguibert/Documents/test_hsqc.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)

cmp <- compare_peak_lists(peak_list, data, 0.005)
cmp2 <- compare_peak_lists(result, data1, tolerance = 0.02)


cmd <-compare_dataframes(UN_Noise, UN_Percentage)

#### Integration ----

# Function to load data and clean column names
load_data <- function(filepath) {
  # Read the CSV file
  data <- read.csv(filepath, header = TRUE, sep = ";", stringsAsFactors = FALSE)
  
  # Convert necessary columns to numeric (replace ',' with '.')
  data <- data %>% 
    mutate(across(c(cropXInd_1, cropXInd_2, cropYInd_1, cropYInd_2, Volume), 
                  ~ as.numeric(gsub(",", ".", .))))
  
  return(data)
}

# Function to find the closest match with individual tolerances
# Function to find the closest match with strict tolerances
compare_crops <- function(file1, file2, tolerance = 0.02) {
  # Load both files
  data1 <- load_data(file1)
  data2 <- load_data(file2)
  
  # Perform a full Cartesian join to compare all rows in data1 with all rows in data2
  comparison <- data1 %>%
    rename_with(~ paste0(.x, "_data1")) %>%
    crossing(data2 %>% rename_with(~ paste0(.x, "_data2"))) %>%
    # Calculate differences and filter based on tolerances
    mutate(
      diff_X1 = abs(cropXInd_1_data1 - cropXInd_1_data2),
      diff_X2 = abs(cropXInd_2_data1 - cropXInd_2_data2),
      diff_Y1 = abs(cropYInd_1_data1 - cropYInd_1_data2),
      diff_Y2 = abs(cropYInd_2_data1 - cropYInd_2_data2)
    ) %>%
    filter(
      diff_X1 <= tolerance &
        diff_X2 <= tolerance &
        diff_Y1 <= tolerance &
        diff_Y2 <= tolerance
    ) %>%
    # Calculate the Euclidean distance
    mutate(
      Distance = sqrt(
        (cropXInd_1_data1 - cropXInd_1_data2)^2 +
          (cropXInd_2_data1 - cropXInd_2_data2)^2 +
          (cropYInd_1_data1 - cropYInd_1_data2)^2 +
          (cropYInd_2_data1 - cropYInd_2_data2)^2
      )
    ) %>%
    # Select the closest match for each crop in data1
    group_by(cropTag_data1) %>%
    slice_min(Distance) %>%
    ungroup() %>%
    # Select relevant columns for the output
    select(
      cropTag_data1, cropTag_data2,
      cropXInd_1_data1, cropXInd_1_data2,
      cropXInd_2_data1, cropXInd_2_data2,
      cropYInd_1_data1, cropYInd_1_data2,
      cropYInd_2_data1, cropYInd_2_data2,
      Volume_data1, Volume_data2, Distance
    ) %>%
    mutate(
      Volume_Difference = abs(Volume_data1 / Volume_data2)
    )
  
  return(comparison)
}
# File paths
file1 <- "C:/Users/juguibert/Documents/crops1.csv"
file2 <- "C:/Users/juguibert/Documents/crops2.csv"
file3 <- "C:/Users/juguibert/Documents/crops3.csv"
file4 <- "C:/Users/juguibert/Documents/crops4.csv"

# Run the comparison with individual tolerances
tolerance_threshold <- 0.02  # Set tolerance for each crop index
result1_2 <- compare_crops(file1, file2, tolerance = tolerance_threshold)
result2_3 <- compare_crops(file2, file3, tolerance = tolerance_threshold)
result1_3 <- compare_crops(file1, file3, tolerance = tolerance_threshold)
# Save result
write.csv(result1_2, "crop_comparison_results.csv", row.names = FALSE)
write.csv(result2_3, "crop_comparison_results2.csv", row.names = FALSE)

# Display the result
print(result)
