#### ACQUS ----


read_acqus <- function(filename) {
  # Read the file line by line
  lines <- readLines(filename, warn = FALSE)
  
  # Patterns to identify different types of entries
  patterns <- list(
    ParVecVal = "^##\\$*(.+)= \\(\\d\\.\\.\\d+\\)(.+)",
    ParVec = "^##\\$*(.+)= \\(\\d\\.\\.\\d+\\)$",
    ParVal = "^##\\$*(.+)= (.+)",
    Val = "^([^\\$#].*)",
    Stamp = "^\\$\\$(.*)",
    EmptyPar = "^##\\$*(.+)=",
    Anything = "^(.+)"
  )
  
  # Determine the type of each line
  row_types <- lapply(lines, function(line) {
    for (type in names(patterns)) {
      if (grepl(patterns[[type]], line)) {
        return(list(type = type, match = regmatches(line, regexec(patterns[[type]], line))[[1]]))
      }
    }
    return(NULL)
  })
  
  # Parse the lines into a list
  params <- list()
  last_param <- NULL
  for (row in row_types) {
    if (is.null(row)) next
    type <- row$type
    match <- row$match[-1]
    if (type == "ParVal") {
      last_param <- gsub("[- ]", "", match[1])
      params[[last_param]] <- match[2]
    } else if (type %in% c("ParVec", "EmptyPar")) {
      last_param <- match[1]
      params[[last_param]] <- NULL
    } else if (type == "ParVecVal") {
      last_param <- match[1]
      params[[last_param]] <- match[2]
    } else if (type == "Stamp") {
      if (!is.null(params$Stamp)) {
        params$Stamp <- paste(params$Stamp, "##", match[1])
      } else {
        params$Stamp <- match[1]
      }
    } else if (type == "Val" && !is.null(last_param)) {
      params[[last_param]] <- paste(params[[last_param]], match[1], sep = " ")
    }
  }
  
  # Convert numeric fields
  for (name in names(params)) {
    val <- suppressWarnings(as.numeric(params[[name]]))
    if (!is.na(val)) {
      params[[name]] <- val
    }
  }
  
  return(params)
}



##### FID -----


read_fid <- function(filename, bruker_byte_format, bruker_byte_size) {
  # Determine endianness
  endian <- if (bruker_byte_format == 0) "little" else if (bruker_byte_format == 1) "big" else stop("Unknown data format (BYTORDA)")
  
  # Determine data type
  data_type <- switch(
    bruker_byte_size,
    "0" = "integer",
    "1" = "double",
    "2" = "double",
    stop("Unknown data format (DTYPA)")
  )
  
  # Read the binary data
  con <- file(filename, "rb")
  raw_data <- readBin(con, what = data_type, size = ifelse(data_type == "integer", 4, 8), endian = endian, n = file.info(filename)$size / (ifelse(data_type == "integer", 4, 8)))
  close(con)
  
  # Combine real and imaginary parts
  npoints <- length(raw_data) / 2
  complex_data <- raw_data[seq(1, 2 * npoints, by = 2)] + 1i * raw_data[seq(2, 2 * npoints, by = 2)]
  
  return(complex_data)
}

#### SER -----

read_ser <- function(filename, bruker_byte_format, bruker_byte_size, TD1, TD2) {
  # Open the binary file
  con <- file(filename, "rb")
  on.exit(close(con))
  
  # Determine the number of points
  total_points <- file.info(filename)$size / bruker_byte_size
  if (total_points != TD1 * TD2) {
    stop("The file size does not match the expected dimensions (TD1 * TD2).")
  }
  
  # Read binary data
  raw_data <- readBin(con, what = ifelse(bruker_byte_size == 4, "integer", "double"),
                      n = total_points, size = bruker_byte_size, endian = ifelse(bruker_byte_format == 0, "big", "little"))
  
  # Convert to complex numbers
  data_real <- raw_data[seq(1, total_points, 2)]
  data_imag <- raw_data[seq(2, total_points, 2)]
  complex_data <- complex(real = data_real, imaginary = data_imag)
  
  # Reshape into a matrix for 2D NMR
  matrix_data <- matrix(complex_data, nrow = TD2, ncol = TD1, byrow = TRUE)
  
  return(matrix_data)
}


# Read metadata
acqus <- read_acqus("path/to/acqus")
acqu2s <- read_acqus("path/to/acqu2s")  # Use the same function as read_acqus

# Parameters for `ser`
TD <- acqus$TD        # Number of points in the direct dimension
TD1 <- acqu2s$TD      # Number of points in the indirect dimension
BYTORDA <- acqus$BYTORDA
DTYPA <- acqus$DTYPA

# Read the `ser` file
ser_file <- "path/to/ser"
ser_data <- read_ser(
  filename = ser_file,
  bruker_byte_format = BYTORDA,
  bruker_byte_size = ifelse(DTYPA == 2, 4, 8),  # 4 bytes for int32, 8 bytes for double
  TD1 = TD1,
  TD2 = TD / TD1  # Compute the number of points in the second dimension
)

# Inspect the data
print(dim(ser_data))  # Should show (TD2, TD1)

image(Mod(ser_data), main = "2D NMR Spectrum", xlab = "F2", ylab = "F1")



#### Fonction lecture et chargement infos importantes fichier bruker ----


bruker_folder <- "C:/Users/juguibert/Documents/240202131_project/240202131_ech/240202131_Spectres_foie_2024/Spectres_UF_foie/404"


read_bruker_file <- function(bruker_folder) {
  # Helper function to read metadata
  read_metadata <- function(metadata_file) {
    if (!file.exists(metadata_file)) stop(paste("Metadata file not found:", metadata_file))
    metadata <- readLines(metadata_file, warn = FALSE)
    extract_value <- function(key) {
      line <- grep(paste0("##\\$", key, "="), metadata, value = TRUE)
      if (length(line) == 0) return(NULL)
      as.numeric(gsub(paste0("##\\$", key, "=\\s*"), "", line))
    }
    list(
      TD1 = extract_value("TD"),
      TD2 = extract_value("TD"),
      BYTORDA = extract_value("BYTORDA"),  # 0 = big endian, 1 = little endian
      DTYPA = extract_value("DTYPA"),     # 0 = int32, 1 = float32, 2 = float64
      SW_h = extract_value("SW_h"),
      O1 = extract_value("O1"),
      SF = extract_value("SF"),
      OFFSET = extract_value("OFFSET")
    )
  }
  
  # Helper function to read binary data (fid/ser)
  read_binary_data <- function(filename, byte_format, byte_size, TD1, TD2) {
    con <- file(filename, "rb")
    on.exit(close(con))
    
    # Determine the number of points
    total_points <- file.info(filename)$size / byte_size
    print(file.info(filename)$size)
    print(byte_size)
    print(total_points)
    print(TD1)
    print(TD2)
    if (is.null(TD1) || TD1 == 0) TD1 <- total_points
    if (is.null(TD2) || TD2 == 0) TD2 <- total_points / TD1
    if (total_points != TD1 * TD2) {
      stop("The file size does not match the expected dimensions (TD1 * TD2).")
    }
    
    # Read binary data
    raw_data <- readBin(con, what = ifelse(byte_size == 4, "integer", "double"),
                        n = total_points, size = byte_size, endian = ifelse(byte_format == 0, "big", "little"))
    
    # Convert to complex numbers
    data_real <- raw_data[seq(1, total_points, 2)]
    data_imag <- raw_data[seq(2, total_points, 2)]
    complex_data <- complex(real = data_real, imaginary = data_imag)
    
    # Reshape into matrix for 2D or vector for 1D
    if (!is.null(TD1) && !is.null(TD2) && TD1 > 1 && TD2 > 1) {
      matrix(complex_data, nrow = TD2, ncol = TD1, byrow = TRUE)
    } else {
      complex_data
    }
  }
  
  
  # Helper function to read 2rr file
  read_2rr_file <- function(data_file, TD1, TD2, byte_format) {
    total_points <- TD1 * TD2
    con <- file(data_file, "rb")
    on.exit(close(con))
    
    intensity <- readBin(con, what = "double", size = 4, n = total_points, endian = ifelse(byte_format == 0, "big", "little"))
    if (length(intensity) != total_points) {
      stop("The file size does not match the expected dimensions (TD1 * TD2).")
    }
    matrix(intensity, nrow = TD1, ncol = TD2, byrow = TRUE)
  }
  
  
  # Locate the necessary files
  acqus_file <- file.path(bruker_folder, "acqus")
  acqu2s_file <- file.path(bruker_folder, "acqu2s")
  procs_file <- file.path(bruker_folder, "pdata/1/procs")
  proc2s_file <- file.path(bruker_folder, "pdata/1/proc2s")
  ser_or_fid_file <- file.path(bruker_folder, "ser")
  if (!file.exists(ser_or_fid_file)) {
    ser_or_fid_file <- file.path(bruker_folder, "fid")
  }
  rr_file <- file.path(bruker_folder, "pdata/1/2rr")
  
  # Read metadata
  acqus <- read_metadata(acqus_file)
  acqu2s <- if (file.exists(acqu2s_file)) read_metadata(acqu2s_file) else list()
  procs <- if (file.exists(procs_file)) read_metadata(procs_file) else list()
  proc2s <- if (file.exists(proc2s_file)) read_metadata(proc2s_file) else list()
  
  # Combine metadata
  metadata <- list(
    TD1 = acqus$TD1,
    TD2 = acqu2s$TD1,
    BYTORDA = acqus$BYTORDA,
    DTYPA = acqus$DTYPA,
    SW_h = acqus$SW_h,
    O1 = acqus$O1,
    SF = procs$SF,
    OFFSET = procs$OFFSET
  )
  
  # Validate metadata
  if (is.null(metadata$TD1) || is.null(metadata$TD2)) stop("TD1 or TD2 not found in metadata.")
  if (is.null(metadata$BYTORDA)) stop("BYTORDA not found in metadata.")
  if (is.null(metadata$DTYPA)) stop("DTYPA not found in metadata.")
  
  # Calculate ppm values
  ppm_x <- metadata$OFFSET - (0:(metadata$TD2 - 1)) * (metadata$SW_h / metadata$SF / metadata$TD2)
  ppm_y <- metadata$OFFSET - (0:(metadata$TD1 - 1)) * (metadata$SW_h / metadata$SF / metadata$TD1)
  
  # Read FID/SER data if available
  binary_data <- if (file.exists(ser_or_fid_file)) {
    read_binary_data(
      filename = ser_or_fid_file,
      byte_format = metadata$BYTORDA,
      byte_size = ifelse(metadata$DTYPA == 2, 8, 4),
      TD1 = metadata$TD1,
      TD2 = metadata$TD2
    )
  } else {
    NULL
  }
  
  # Read 2rr data if available
  rr_data <- if (file.exists(rr_file)) {
    read_2rr_file(
      data_file = rr_file,
      TD1 = metadata$TD1,
      TD2 = metadata$TD2,
      byte_format = metadata$BYTORDA
    )
  } else {
    NULL
  }
  
  # Return data
  list(
    metadata = metadata,
    ppm_x = ppm_x,
    ppm_y = ppm_y,
    binary_data = binary_data,
    rr_data = rr_data
  )
}

bruker_data <- read_bruker_file(bruker_folder)

# Access components
metadata <- bruker_data$metadata
Rppm_x <- bruker_data$ppm_x
Rppm_y <- bruker_data$ppm_y
binary_data <- bruker_data$binary_data
rr_data <- bruker_data$rr_data

plot_2D_NMR <- function(ppm_x, ppm_y, intensity_matrix, threshold = NULL,contour_levels = 10 ) {
  library(ggplot2)
  
  # Dynamically calculate the threshold
  if (!is.null(threshold)) {
    intensity_matrix[intensity_matrix < threshold] <- NA
  }
  
  # Convert the matrix into a long format for ggplot
  intensity_df <- expand.grid(ppm_x = ppm_x, ppm_y = ppm_y)
  intensity_df$intensity <- as.vector(intensity_matrix)
  
  # Create contour plot
  ggplot(intensity_df, aes(x = ppm_y, y = ppm_x, z = intensity)) +
    geom_contour(aes(color = ..level..), bins = contour_levels) +
    scale_x_reverse() +
    labs(x = expression(delta ~ "/ppm (F2)"), y = expression(delta ~ "/ppm (F1)")) +
    theme_minimal()
}

rr_data[is.nan(rr_data)] <- 0

rr_data

plot_2D_NMR(ppm_x = Rppm_x, ppm_y = Rppm_y, intensity_matrix = rr_data, threshold = 10, 10)



### Calcul des contours ----

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
