# ---------
# Functions
# ---------

# Function : R.scan
# -------------------------------------------------------------------------
# Extracts a component from a character string/vector based on a splitter
# Example: R.scan("T-231-VB.X-Q", "-", 2) = "231" 
# Example: R.scan("T-231-VB.X-Q", ".", 2) = "X-Q"
# -------------------------------------------------------------------------
 
R.scan <- function(char.vec, splitter, component){
  if (is.na(char.vec)){temp <- NA}
  else{
    temp <- rep(NA, length(char.vec))
    for (i in 1:length(char.vec)){
      temp[i] <- strsplit(char.vec[i],splitter)[[1]][component]
    }
  }
  temp
}

# Function: minisplit
# -------------------------------------------------------------------------
# Takes a string with "words" in it separated by varying number of spaces
# Converts just the "words" to a vector & drops spaces/empty columns
# Example: minisplit("V1 V2  V3    V4") = c("V1","V2","V3","V4)
# -------------------------------------------------------------------------

minisplit <- function(x){
  temp <- strsplit(x," ")[[1]]
  temp[!(temp == "")]
}


# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
#                         Forsyth County Parser 
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

filelist <- list.files("X:\\Data\\calibration\\WS_reference_data")

result <- list()

k <- 1

# Loop through the files
for (i in filelist){

  # Read plain text
  temp <- readLines(paste0("X:\\Data\\calibration\\WS_reference_data\\",i))

  # Read and format date
  current.day <- as.Date(R.scan(temp[grep("Current Date", temp)][1],":",2), format = "%m/%d/%y")

  # Get locations within the file of hourly data, parameter names, and site names
  sitelist.1 <- grep("Hour ", temp)
  paramlist <-  temp[grepl("Param", temp) & !grepl("Sequence",temp)]
  sitelist.2 <- R.scan(R.scan(temp[grep("Logger",temp)],":",2)," ",2)

  # Loop through all the sites
  # --------------------------
  for (j in 1:length(sitelist)){
    # Extract Data
    # ------------

    # Site name
    sitename <- sitelist.2[j]

    # Hourly data
    temp2 <- t(sapply(temp[(sitelist.1[j]+2):(sitelist.1[j]+25)], FUN = minisplit))
    rownames(temp2) <- paste0("h",temp2[,1])

    # Parameter names
    colnames(temp2) <- c("hour",minisplit(R.scan(paramlist[j],":",2)))
    
    # Cast to data frame
    temp2 <- as.data.frame(temp2, stringsAsFactors = FALSE)

    # Repeat site name and date
    temp2$site_id <- rep(sitename, dim(temp2)[1])
    temp2$date <- rep(current.day, dim(temp2)[1])

    # Aggregate data
    result[[k]] <- temp2
    k <- k+1
} }

# Some ways to check your work:
# -----------------------------

i <- filelist[1]
j <- 1
str(result)
head(result[[1]])

# Quickest way to get documentation:
# ----------------------------------
?strsplit

# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
#                            Los Angeles Parser
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

filelist <- list.files("X:\\Data\\calibration\\LA_reference_data")


# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
#                               PSCAA Parser
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

filelist <- list.files("X:\\Data\\calibration\\Sea_reference_data")


# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
#                               NYDEC Parser
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

# read each xls file, constructing column names from the top few rows (in format SITENAME_VARIABLE)
filelist <- list.files("X:\\Data\\calibration\\NYC_reference_data", pattern = '\\.xls', full.names = T)
result <- list()
for( file in filelist ){
    header <- sapply(
            xlsx::read.xlsx(file, 1, startRow = 2, endRow = 3, header = F), #read to row 4 if you want units included in colnames
            function(x){str_replace(paste(x, collapse = '_'), ' ', '')}
            )
    result[[file]] <- xlsx::read.xlsx(file, 1, startRow = 5, header = F)
    names(result[[file]]) <- str_replace(header, '_NA_NA', '')
}
result <- do.call(rbind, result)
    