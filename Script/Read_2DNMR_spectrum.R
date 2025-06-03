### Read Metadata from Bruker files ----
read_bruker <- function (dir = NULL, dim = NULL, onlyTitles = FALSE, 
                         useAsNames = "Spectrum titles", checkFiles = FALSE) 
{
  AcquPars <- list(NS = 0, BF1 = 0, P1 = 0, RG = 0, PULPROG = "", 
                   SOLVENT = "")
  dnDim <- c("1r", "2rr")
  names(dnDim) <- c("1D", "2D")
  spectrum_proc_path <- dir
  datanameTmp <- dnDim[dim]
  if (!is.null(spectrum_proc_path)) {
    BYTORDP_Dict <- c("little", "big")
    names(BYTORDP_Dict) <- c(0, 1)
    TITLE <- ""
    try(TITLE <- scan(file = paste(spectrum_proc_path, "/title", 
                                   sep = ""), what = "character", sep = "\n", quiet = TRUE)[1], 
        silent = TRUE)
    if (checkFiles) {
      spectrumData <- NULL
      titleFinal <- NULL
      spectrumDataTitle <- NULL
      spectrumDataFolderName <- NULL
      spectrumDataEXPNO <- NULL
      spectrumDataFolderName_EXPNO <- NULL
      list.filesTMP <- list.files(spectrum_proc_path)
      if (!"procs" %in% list.filesTMP) {
        stop(paste("Could not open spectrum", spectrum_proc_path))
      }
    }
    else {
      if (!onlyTitles) {
        spectrum_acqu_path <- NULL
        spectrum_acqu_pathTMP <- strsplit(spectrum_proc_path, 
                                          "/")[[1]]
        spectrum_acqu_path <- paste(spectrum_acqu_pathTMP[1:(length(spectrum_acqu_pathTMP) - 
                                                               2)], sep = "/", collapse = "/")
        acqusTMP <- NULL
        try(acqusTMP <- scan(file = paste(spectrum_acqu_path, 
                                          "/acqu", sep = ""), what = "character", sep = "\n", 
                             quiet = TRUE), silent = TRUE)
        if (!(is.null(acqusTMP) | (length(acqusTMP) == 
                                   0))) {
          acqusTMP <- gsub("#", "", acqusTMP)
          acqusTMP <- gsub("\\$", " ", acqusTMP)
          RGpos <- grep(" RG=", acqusTMP)
          if (length(RGpos) > 0) 
            AcquPars$RG <- as.numeric(strsplit(acqusTMP[RGpos], 
                                               split = "= ")[[1]][2])
          NSpos <- grep(" NS=", acqusTMP)
          if (length(NSpos) > 0) 
            AcquPars$NS <- as.numeric(strsplit(acqusTMP[NSpos], 
                                               split = "= ")[[1]][2])
          BF1pos <- grep(" BF1=", acqusTMP)
          if (length(BF1pos) > 0) 
            AcquPars$BF1 <- as.numeric(strsplit(acqusTMP[BF1pos], 
                                                split = "= ")[[1]][2])
          PULPROGpos <- grep(" PULPROG=", acqusTMP)
          if (length(PULPROGpos) > 0) 
            AcquPars$PULPROG <- strsplit(acqusTMP[PULPROGpos], 
                                         split = "= ")[[1]][2]
          SOLVENTpos <- grep(" SOLVENT=", acqusTMP)
          if (length(SOLVENTpos) > 0) 
            AcquPars$SOLVENT <- strsplit(acqusTMP[SOLVENTpos], 
                                         split = "= ")[[1]][2]
          P1pos <- grep(" P=", acqusTMP)
          if (length(P1pos) > 0) 
            AcquPars$P1 <- as.numeric(strsplit(acqusTMP[P1pos + 
                                                          1], , split = " ")[[1]][2])
        }
        proc <- scan(file = paste(spectrum_proc_path, 
                                  "/procs", sep = ""), what = "character", sep = "\n", 
                     quiet = TRUE)
        proc <- gsub("#", "", proc)
        proc <- gsub("\\$", " ", proc)
        SI2 <- as.numeric(strsplit(proc[grep(" SI=", 
                                             proc)], split = "= ")[[1]][2])
        BYTORDP <- BYTORDP_Dict[strsplit(proc[grep(" BYTORDP=", 
                                                   proc)], split = "= ")[[1]][2]]
        NC_proc <- as.numeric(strsplit(proc[grep(" NC_proc=", 
                                                 proc)], split = "= ")[[1]][2])
        XDIM2 <- as.numeric(strsplit(proc[grep(" XDIM=", 
                                               proc)], split = "= ")[[1]][2])
        OFFSET2 <- as.numeric(strsplit(proc[grep(" OFFSET=", 
                                                 proc)], split = "= ")[[1]][2])
        SF2 <- as.numeric(strsplit(proc[grep(" SF=", 
                                             proc)], split = "= ")[[1]][2])
        SW_p2 <- as.numeric(strsplit(proc[grep(" SW_p=", 
                                               proc)], split = "= ")[[1]][2])
        rightlimit2 <- OFFSET2 - SW_p2/SF2
        leftlimit2 <- OFFSET2
        frequencynames2 <- OFFSET2 - (0:(SI2 - 1)) * 
          SW_p2/SF2/SI2
        n <- SI2
        if (datanameTmp == "2rr") {
          proc2 <- scan(file = paste(spectrum_proc_path, 
                                     "/proc2s", sep = ""), what = "character", 
                        sep = "\n", quiet = TRUE)
          proc2 <- gsub("#", "", proc2)
          proc2 <- gsub("\\$", " ", proc2)
          SI1 <- as.numeric(strsplit(proc2[grep(" SI=", 
                                                proc2)], split = "= ")[[1]][2])
          XDIM1 <- as.numeric(strsplit(proc2[grep(" XDIM=", 
                                                  proc2)], split = "= ")[[1]][2])
          OFFSET1 <- as.numeric(strsplit(proc2[grep(" OFFSET=", 
                                                    proc2)], split = "= ")[[1]][2])
          SF1 <- as.numeric(strsplit(proc2[grep(" SF=", 
                                                proc2)], split = "= ")[[1]][2])
          SW_p1 <- as.numeric(strsplit(proc2[grep(" SW_p=", 
                                                  proc2)], split = "= ")[[1]][2])
          n <- SI2 * SI1
          rightlimit1 <- OFFSET1 - SW_p1/SF1
          leftlimit1 <- OFFSET1
          frequencynames1 <- OFFSET1 - (0:(SI1 - 1)) * 
            SW_p1/SF1/SI1
        }
        spectrumData <- readBin(paste(spectrum_proc_path, 
                                         "/", datanameTmp, sep = ""), what = "integer", 
                                   size = 4, n = n, endian = BYTORDP)
        spectrumData <- spectrumData * 2^NC_proc
        if (datanameTmp == "2rr") {
          spectrumDataTMP <- spectrumData
          spectrumData <- matrix(spectrumData, 
                                    ncol = SI2, byrow = TRUE)
          counter <- 0
          for (j in 1:(nrow(spectrumData)/XDIM1)) {
            for (i in 1:(ncol(spectrumData)/XDIM2)) {
              spectrumData[(j - 1) * XDIM1 + (1:XDIM1), 
                              (i - 1) * XDIM2 + (1:XDIM2)] <- matrix(spectrumDataTMP[counter * 
                                                                                          XDIM2 * XDIM1 + (1:(XDIM2 * XDIM1))], 
                                                                     ncol = XDIM2, byrow = TRUE)
              counter <- counter + 1
            }
          }
          rownames(spectrumData) <- frequencynames1
          colnames(spectrumData) <- frequencynames2
        }
        else {
          names(spectrumData) <- frequencynames2
        }
      }
      else {
        spectrumData <- NULL
      }
      spectrumDataTitle <- TITLE
      spectrumDataFolderName <- rev(strsplit(spectrum_proc_path, 
                                                "/")[[1]])[4]
      spectrumDataEXPNO <- rev(strsplit(spectrum_proc_path, 
                                           "/")[[1]])[3]
      spectrumDataFolderName_EXPNO <- paste(spectrumDataFolderName, 
                                               paste(c("_", "0", "0", "0", "0")[1:max(1, 5 - 
                                                                                        nchar(spectrumDataEXPNO))], sep = "", collapse = ""), 
                                               spectrumDataEXPNO, sep = "")
      if (useAsNames == "Spectrum titles") 
        titleFinal <- spectrumDataTitle
      if (useAsNames == "dir names") 
        titleFinal <- spectrumDataFolderName
      if (useAsNames == "dir names and EXPNO") 
        titleFinal <- spectrumDataFolderName_EXPNO
    }
    invisible(list(spectrumData = spectrumData, spectrumDataName = titleFinal, 
                   spectrumDataTitle = spectrumDataTitle, spectrumDataFolderName = spectrumDataFolderName, 
                   spectrumDataEXPNO = spectrumDataEXPNO, spectrumDataFolderName_EXPNO = spectrumDataFolderName_EXPNO, 
                   AcquPars = AcquPars))
  }
}
