# #### ACQUS ----
# 
# 
# read_acqus <- function(filename) {
#   # Read the file line by line
#   lines <- readLines(filename, warn = FALSE)
#   
#   # Patterns to identify different types of entries
#   patterns <- list(
#     ParVecVal = "^##\\$*(.+)= \\(\\d\\.\\.\\d+\\)(.+)",
#     ParVec = "^##\\$*(.+)= \\(\\d\\.\\.\\d+\\)$",
#     ParVal = "^##\\$*(.+)= (.+)",
#     Val = "^([^\\$#].*)",
#     Stamp = "^\\$\\$(.*)",
#     EmptyPar = "^##\\$*(.+)=",
#     Anything = "^(.+)"
#   )
#   
#   # Determine the type of each line
#   row_types <- lapply(lines, function(line) {
#     for (type in names(patterns)) {
#       if (grepl(patterns[[type]], line)) {
#         return(list(type = type, match = regmatches(line, regexec(patterns[[type]], line))[[1]]))
#       }
#     }
#     return(NULL)
#   })
#   
#   # Parse the lines into a list
#   params <- list()
#   last_param <- NULL
#   for (row in row_types) {
#     if (is.null(row)) next
#     type <- row$type
#     match <- row$match[-1]
#     if (type == "ParVal") {
#       last_param <- gsub("[- ]", "", match[1])
#       params[[last_param]] <- match[2]
#     } else if (type %in% c("ParVec", "EmptyPar")) {
#       last_param <- match[1]
#       params[[last_param]] <- NULL
#     } else if (type == "ParVecVal") {
#       last_param <- match[1]
#       params[[last_param]] <- match[2]
#     } else if (type == "Stamp") {
#       if (!is.null(params$Stamp)) {
#         params$Stamp <- paste(params$Stamp, "##", match[1])
#       } else {
#         params$Stamp <- match[1]
#       }
#     } else if (type == "Val" && !is.null(last_param)) {
#       params[[last_param]] <- paste(params[[last_param]], match[1], sep = " ")
#     }
#   }
#   
#   # Convert numeric fields
#   for (name in names(params)) {
#     val <- suppressWarnings(as.numeric(params[[name]]))
#     if (!is.na(val)) {
#       params[[name]] <- val
#     }
#   }
#   
#   return(params)
# }
# 
# 
# 
# ##### FID -----
# 
# 
# read_fid <- function(filename, bruker_byte_format, bruker_byte_size) {
#   # Determine endianness
#   endian <- if (bruker_byte_format == 0) "little" else if (bruker_byte_format == 1) "big" else stop("Unknown data format (BYTORDA)")
#   
#   # Determine data type
#   data_type <- switch(
#     bruker_byte_size,
#     "0" = "integer",
#     "1" = "double",
#     "2" = "double",
#     stop("Unknown data format (DTYPA)")
#   )
#   
#   # Read the binary data
#   con <- file(filename, "rb")
#   raw_data <- readBin(con, what = data_type, size = ifelse(data_type == "integer", 4, 8), endian = endian, n = file.info(filename)$size / (ifelse(data_type == "integer", 4, 8)))
#   close(con)
#   
#   # Combine real and imaginary parts
#   npoints <- length(raw_data) / 2
#   complex_data <- raw_data[seq(1, 2 * npoints, by = 2)] + 1i * raw_data[seq(2, 2 * npoints, by = 2)]
#   
#   return(complex_data)
# }
# 
# #### SER -----
# 
# read_ser <- function(filename, bruker_byte_format, bruker_byte_size, TD1, TD2) {
#   # Open the binary file
#   con <- file(filename, "rb")
#   on.exit(close(con))
#   
#   # Determine the number of points
#   total_points <- file.info(filename)$size / bruker_byte_size
#   if (total_points != TD1 * TD2) {
#     stop("The file size does not match the expected dimensions (TD1 * TD2).")
#   }
#   
#   # Read binary data
#   raw_data <- readBin(con, what = ifelse(bruker_byte_size == 4, "integer", "double"),
#                       n = total_points, size = bruker_byte_size, endian = ifelse(bruker_byte_format == 0, "big", "little"))
#   
#   # Convert to complex numbers
#   data_real <- raw_data[seq(1, total_points, 2)]
#   data_imag <- raw_data[seq(2, total_points, 2)]
#   complex_data <- complex(real = data_real, imaginary = data_imag)
#   
#   # Reshape into a matrix for 2D NMR
#   matrix_data <- matrix(complex_data, nrow = TD2, ncol = TD1, byrow = TRUE)
#   
#   return(matrix_data)
# }
# 
# 
# # Read metadata
# acqus <- read_acqus("path/to/acqus")
# acqu2s <- read_acqus("path/to/acqu2s")  # Use the same function as read_acqus
# 
# # Parameters for `ser`
# TD <- acqus$TD        # Number of points in the direct dimension
# TD1 <- acqu2s$TD      # Number of points in the indirect dimension
# BYTORDA <- acqus$BYTORDA
# DTYPA <- acqus$DTYPA
# 
# # Read the `ser` file
# ser_file <- "path/to/ser"
# ser_data <- read_ser(
#   filename = ser_file,
#   bruker_byte_format = BYTORDA,
#   bruker_byte_size = ifelse(DTYPA == 2, 4, 8),  # 4 bytes for int32, 8 bytes for double
#   TD1 = TD1,
#   TD2 = TD / TD1  # Compute the number of points in the second dimension
# )
# 
# # Inspect the data
# print(dim(ser_data))  # Should show (TD2, TD1)
# 
# image(Mod(ser_data), main = "2D NMR Spectrum", xlab = "F2", ylab = "F1")



#### Fonction lecture et chargement infos importantes fichier bruker  ----


bruker_folder <- "C:/Users/juguibert/Documents/240202131_project/240202131_ech/240202131_Spectres_foie_2024/Spectres_UF_foie/404"

  # Helper function to read metadata from Bruker files
  read_metadata <- function(metadata_file) {
    if (!file.exists(metadata_file)) stop(paste("Metadata file not found:", metadata_file))
    metadata <- readLines(metadata_file, warn = FALSE)
    extract_value <- function(key) {
      line <- grep(paste0("##\\$", key, "="), metadata, value = TRUE)
      if (length(line) == 0) return(NULL)
      gsub(paste0("##\\$", key, "=\\s*"), "", line)
    }
    list(
      TD = as.numeric(extract_value("TD")),
      NUS = as.numeric(extract_value("NusTD")),
      BYTORDA = as.numeric(extract_value("BYTORDA")),
      DTYPA = as.numeric(extract_value("DTYPA")),
      SW_p = as.numeric(extract_value("SW_p")),
      O1 = as.numeric(extract_value("O1")),
      SF = as.numeric(extract_value("SF")),
      OFFSET = as.numeric(extract_value("OFFSET")),
      GRPDLY = as.numeric(extract_value("GRPDLY")),
      GPX = as.numeric(extract_value("GPX")),
      GPY = as.numeric(extract_value("GPY")),
      GPZ = as.numeric(extract_value("GPZ")),
      NUC1 = extract_value("NUC1"),
      NUC2 = extract_value("NUC2"),
      SI= as.numeric(extract_value("SI")),
      YMAX = as.numeric(extract_value("YMAX_p")),
      YMIN = as.numeric(extract_value("YMIN_p")),
      PHC0 = as.numeric(extract_value("PHC0")),
      PHC1 = as.numeric(extract_value("PHC1")),
      BC_mod =as.numeric(extract_value("BC_mod"))
    )
  }
  
  # # Function to read binary FID/SER data
  # read_binary_data <- function(filename, byte_format, byte_size, TD1, TD2) {
  #   con <- file(filename, "rb")
  #   on.exit(close(con))
  #   
  #   total_points <- file.info(filename)$size / byte_size
  #   if (is.null(TD1) || TD1 == 0) TD1 <- total_points
  #   if (is.null(TD2) || TD2 == 0) TD2 <- total_points / TD1
  #   
  #   raw_data <- readBin(con, what = ifelse(byte_size == 4, "integer", "double"),
  #                       n = total_points, size = byte_size, endian = ifelse(byte_format == 0, "big", "little"))
  #   
  #   data_real <- raw_data[seq(1, total_points, 2)]
  #   data_imag <- raw_data[seq(2, total_points, 2)]
  #   complex_data <- complex(real = data_real, imaginary = data_imag)
  #   
  #   max_val <- max(abs(complex_data), na.rm = TRUE)
  #   complex_data <- complex_data #/ max_val
  #   
  #   if (!is.null(TD1) && !is.null(TD2) && TD1 > 1 && TD2 > 1) {
  #     matrix(complex_data, nrow = TD1, ncol = TD2, byrow = TRUE)
  #   } else {
  #     complex_data
  #   }
  # }
  
  preprocess_rr_data <- function(data) {
    # Replace Inf and NaN values with 0
    data[is.nan(data)] <- 0
    
    finite_max <- max(data[is.finite(data)], na.rm = TRUE)
    finite_min <- min(data[is.finite(data)], na.rm = TRUE)
    # 
    data[data == Inf] <- finite_max
    data[data == -Inf] <- finite_min
    
    # Debug: Print summary of cleaned data
    cat("After preprocessing, rr_data summary:\n")
    print(summary(data))
    
    return(data)
  }
  
  normalize_data <- function(data, method = "dynamic", YMAX = NULL, YMIN = NULL) {
    if (method == "metadata") {
      # Normalize using metadata values (YMAX and YMIN)
      if (!is.null(YMAX) && !is.null(YMIN) && YMAX != YMIN) {
        normalized_data <- (data - YMIN) / (YMAX - YMIN)
      } else {
        stop("YMAX and YMIN are required for 'metadata' normalization and must not be equal.")
      }
    } else if (method == "dynamic") {
      # Normalize using the maximum absolute intensity
      max_val <- max(abs(data), na.rm = TRUE)
      cat("Maximum value in rr_data for normalization:", max_val, "\n")
      if (is.infinite(max_val) || is.nan(max_val) || max_val <= 0) {
        stop("Maximum value of the data is zero, invalid, or infinite for dynamic normalization.")
      }
      normalized_data <- data / max_val
    } else if (method == "percentile") {
      # Normalize using percentiles to handle outliers
      lower_limit <- quantile(data, 0.01, na.rm = TRUE)  # 1st percentile
      upper_limit <- quantile(data, 0.99, na.rm = TRUE)  # 99th percentile
      data <- pmax(pmin(data, upper_limit), lower_limit)  # Clamp data
      max_val <- max(abs(data), na.rm = TRUE)
      if (is.infinite(max_val) || is.nan(max_val) || max_val <= 0) {
        stop("Maximum value of the data is zero, invalid, or infinite for percentile normalization.")
      }
      normalized_data <- data / max_val
    } else {
      stop("Invalid normalization method. Choose 'metadata', 'dynamic', or 'percentile'.")
    }
    
    return(normalized_data)
  }
  
  # Function to read 2rr file with block size consideration
  read_2rr_file <- function(data_file, SI1, SI2, XDIM1, XDIM2) {
    total_points <- SI1 * SI2
    con <- file(data_file, "rb")
    on.exit(close(con))
    
    # Read the raw data
    raw_data <- readBin(con, what = "double", size = 4, n = total_points, endian = "big")
    if (length(raw_data) != total_points) {
      stop("The file size does not match the expected dimensions (SI1 * SI2).")
    }
    
    # Handle block-based reshaping
    if (XDIM1 > 1 || XDIM2 > 1) {
      # Reshape data block-by-block
      blocks_x <- SI1 / XDIM1
      blocks_y <- SI2 / XDIM2
      
      if (blocks_x %% 1 != 0 || blocks_y %% 1 != 0) {
        stop("SI1 or SI2 is not divisible by XDIM1 or XDIM2.")
      }
      
      # Initialize the reshaped matrix
      matrix_data <- matrix(0, nrow = SI1, ncol = SI2)
      idx <- 1
      
      for (bx in 0:(blocks_x - 1)) {
        for (by in 0:(blocks_y - 1)) {
          # Determine the start and end indices for this block
          block_start <- idx
          block_end <- idx + (XDIM1 * XDIM2) - 1
          block_data <- raw_data[block_start:block_end]
          
          # Reshape the block and place it in the right position
          block_matrix <- matrix(block_data, nrow = XDIM1, ncol = XDIM2, byrow = TRUE)
          row_start <- (bx * XDIM1) + 1
          row_end <- row_start + XDIM1 - 1
          col_start <- (by * XDIM2) + 1
          col_end <- col_start + XDIM2 - 1
          
          matrix_data[row_start:row_end, col_start:col_end] <- block_matrix
          
          idx <- block_end + 1
        }
      }
    } else {
      # Default reshaping if no blocks are used
      matrix_data <- matrix(raw_data, nrow = SI1, ncol = SI2, byrow = TRUE)
    }
    
    return(matrix_data)
  }

  
  calculate_ppm <- function(metadata, SI1, SI2) {
    # Calculate the ppm values
    ppm_y <- metadata$OFFSET2 - (0:(SI2 - 1)) * (metadata$SW_p2 / metadata$SF2 / SI2)
    ppm_x <- metadata$OFFSET1 - (0:(SI1 - 1)) * (metadata$SW_p1 / metadata$SF1 / SI1)

    
    list(ppm_x = ppm_x, ppm_y = ppm_y)
  }
  
  estimate_noise <- function(data) {
    # Use the lowest 1% of intensities to estimate noise
    noise_region <- data[data < quantile(data, 0.01, na.rm = TRUE)]
    noise_est <- sd(noise_region, na.rm = TRUE)
    cat("Estimated noise level:", noise_est, "\n")
    return(noise_est)
  }
  
  # Function to apply group delay correction
  apply_group_delay <- function(data, GRPDLY) {
    if (is.null(GRPDLY)) return(data)
    chop <- floor(GRPDLY)
    phase_shift <- GRPDLY - chop
    data <- c(data[-(1:chop)], rep(0, chop))
    data <- data * exp(-1i * phase_shift)
    data
  }
  
  apply_phase_correction <- function(data, PHC0, PHC1) {
    n <- ncol(data)
    phase <- PHC0 + PHC1 * (0:(n - 1)) / n
    phase_correction <- exp(-1i * phase * pi / 180)  # Convert degrees to radians
    corrected_data <- data * matrix(phase_correction, nrow = nrow(data), ncol = n, byrow = TRUE)
    
    # Return only the real part for further processing
    return(Re(corrected_data))
  }
  
  apply_baseline_correction <- function(data) {
    corrected_data <- sweep(data, 1, rowMeans(data), FUN = "-")  # Subtract row-wise mean
    corrected_data <- sweep(corrected_data, 2, colMeans(corrected_data), FUN = "-")  # Subtract column-wise mean
    return(corrected_data)
  }
  
  suppress_diagonal <- function(data) {
    diag_indices <- 1:min(nrow(data), ncol(data))
    data[cbind(diag_indices, diag_indices)] <- 0
    return(data)
  }
  
  
  # Main function to read Bruker files
  read_bruker_file <- function(bruker_folder, normalization = "dynamic", verbose = FALSE) {
    acqus_file <- file.path(bruker_folder, "acqus")
    acqu2s_file <- file.path(bruker_folder, "acqu2s")
    procs_file <- file.path(bruker_folder, "pdata/1/procs")
    proc2s_file <- file.path(bruker_folder, "pdata/1/proc2s")
    ser_file <- file.path(bruker_folder, "ser")
    if (!file.exists(ser_file)) ser_file <- file.path(bruker_folder, "fid")
    rr_file <- file.path(bruker_folder, "pdata/1/2rr")
    
    acqus <- read_metadata(acqus_file)
    acqu2s <- if (file.exists(acqu2s_file)) read_metadata(acqu2s_file) else list()
    procs <- if (file.exists(procs_file)) read_metadata(procs_file) else list()
    proc2s <- if (file.exists(proc2s_file)) read_metadata(proc2s_file) else list()
    
    print(procs)
    print(proc2s)
    
    metadata <- list(
      SI2 = procs$SI,
      SI1 = proc2s$SI,
      BYTORDA = acqus$BYTORDA,
      DTYPA = acqus$DTYPA,
      SW_p2 = procs$SW_p,
      SW_p1 = proc2s$SW_p,
      SF2 = procs$SF,
      SF1 = proc2s$SF,
      OFFSET2 = procs$OFFSET,
      OFFSET1 = proc2s$OFFSET,
      NUC1 = acqus$NUC1,
      NUC2 = acqus$NUC2,
      TD2 = acqus$TD,
      TD1 = acqu2s$NUS,
      YMAX = proc2s$YMAX,
      YMIN = proc2s$YMIN,
      PHC0_1 = proc2s$PHC0,
      PHC1_1 = proc2s$PHC1,
      PHC0_2 = procs$PHC0,
      PHC1_2 = procs$PHC1,
      BC_mod_2 = proc2s$BC_mod,
      BC_mod_1 = procs$BC_mod
    )
    
    print(metadata)
    
    if (is.null(metadata$SI1) || is.null(metadata$SI2)) stop("SI1 or SI2 not found in metadata.")
    if (is.null(metadata$BYTORDA)) stop("BYTORDA not found in metadata.")
    if (is.null(metadata$DTYPA)) stop("DTYPA not found in metadata.")
    
    experiment_type <- paste0("Direct: ", metadata$NUC1, ", Indirect: ", metadata$NUC2)
    if (verbose) cat("Experiment type: ", experiment_type, "\n")
    
    ppm_y <-metadata$OFFSET2 - (0:(metadata$SI2 - 1)) * (metadata$SW_p2 / metadata$SF2 / metadata$SI2)
    ppm_x <-metadata$OFFSET1 - (0:(metadata$SI1 - 1)) * (metadata$SW_p1 / metadata$SF1 / metadata$SI1)
  
    if (any(!is.finite(ppm_x)) || any(!is.finite(ppm_y))) {
      stop("Calculated ppm values contain invalid entries. Check metadata.")
    }
    
    # binary_data <- if (file.exists(ser_file)) {
    #   read_binary_data(
    #     filename = ser_file,
    #     byte_format = metadata$BYTORDA,
    #     byte_size = ifelse(metadata$DTYPA == 2, 8, 4),
    #     TD1 = metadata$TD1,
    #     TD2 = metadata$TD2
    #   )
    # } else {
    #   NULL
    # }
    
    rr_data <- if (file.exists(rr_file)) {
      read_2rr_file(
        data_file = rr_file,
        SI1 = metadata$SI1,
        SI2 = metadata$SI2,
        XDIM1 = 16,
        XDIM2 = 512
      )
    } else {
      NULL
    }
    
    
    if (!is.null(rr_data)) {
      # Debug: Check raw data
      cat("Initial rr_data summary:\n")
      print(summary(rr_data))
      
      # Preprocess rr_data to replace invalid values
      rr_data <- preprocess_rr_data(rr_data)
      
      # Apply phase correction
      rr_data <- apply_phase_correction(rr_data, metadata$PHC0_1, metadata$PHC1_1)
      rr_data <- t(apply_phase_correction(t(rr_data), metadata$PHC0_2, metadata$PHC1_2))
      
      # Debug: Check data after phase correction
      cat("After phase correction:\n")
      print(summary(rr_data))
      
      # Estimate noise and apply SNR normalization
      noise_level <- estimate_noise(rr_data)
      if (noise_level > 0) {
        rr_data <- rr_data / noise_level
      } else {
        stop("Noise level is zero or invalid for SNR normalization.")
      }
      
      # Normalize data
      rr_data <- normalize_data(
        rr_data,
        method = normalization,
        YMAX = metadata$YMAX,
        YMIN = metadata$YMIN
      )
      
      # Apply diagonal suppression
      rr_data <- suppress_diagonal(rr_data)
      
      # Debug: Check data after normalization
      cat("After normalization:\n")
      print(summary(rr_data))
    }
    
    
  #   
  #   
    print(rr_data[1:10,1:10])
    
    list(
      metadata = metadata,
      ppm = list(x = ppm_x, y = ppm_y),
      binary_data = binary_data,
      rr_data = rr_data,
      experiment_type = experiment_type
    )
    
  }


# Access components
metadata <- bruker_data$metadata
Rppm_x <- bruker_data$ppm_x
Rppm_y <- bruker_data$ppm_y
binary_data <- bruker_data$binary_data
rr_data <- bruker_data$rr_data


### Calcul des contours (Not working as intended) ----

library(Rcpp)
cppFunction(
  "
  NumericMatrix calculateContours(NumericMatrix data, double threshold) {
      int nrow = data.nrow();
      int ncol = data.ncol();
      NumericMatrix contours(nrow, ncol);
      
      // Find the maximum value in the matrix
      int max_val = -1e10;
      for (int i = 0; i < nrow; ++i) {
          for (int j = 0; j < ncol; ++j) {
              if (!NumericVector::is_na(data(i, j)) && data(i, j) > max_val) {
                  max_val = data(i, j);
              }
          }
      }
      
      // Calculate threshold value
      int th_value = max_val * threshold / 100.0;
      
      // Generate the contours matrix based on the threshold
      for (int i = 0; i < nrow; ++i) {
          for (int j = 0; j < ncol; ++j) {
              if (data(i, j) >= th_value) {
                  contours(i, j) = data(i, j);
              } else {
                  contours(i, j) = NA_REAL;
              }
          }
      }
      
      // Replace NA values in the output contours with 0
      for (int i = 0; i < nrow; ++i) {
          for (int j = 0; j < ncol; ++j) {
              if (NumericVector::is_na(contours(i, j))) {
                  contours(i, j) = 0;
              }
          }
      }
      
      return contours;
  }
  "
)