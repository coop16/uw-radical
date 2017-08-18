# ---------
# Functions
# ---------

library(xlsx)


# Function : R.scan
# -------------------------------------------------------------------------
# Extracts a component from a character string/vector based on a splitter
# Example: R.scan("T-231-VB.X-Q", "-", 2) = "231" 
# Example: R.scan("T-231-VB.X-Q", ".", 2) = "X-Q"
# -------------------------------------------------------------------------
 
R.scan <- function(char.vec, splitter, component){
  if (is.na(char.vec)){temp <- NA}
  if (splitter == "."){splitter <- "[.]"}
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

# Function: get.format
# -------------------------------------------------------------------------
get.format <- function(x){
  n.x <- length(x)
  hour <- strtoi(R.scan(x, ":",1))
  mins <- R.scan(x, ":",2)
  secs <- R.scan(x, ":",3)
  hour.bool <- sum(!is.na(hour)) > 0
  min.bool <- sum(!is.na(mins)) > 0
  sec.bool <- sum(!is.na(secs)) > 0
  hr.max <- max(hour, na.rm=TRUE)
  am.pm.bool <- sum(grepl("AM|PM|A.M.|P.M.", x)) > 0
  if (hr.max > 12) { f <- "%H"}
  else { f <- "%I" }
  if (min.bool){
    f <- paste0(f, ":%M")
    if (sec.bool){
      f <- paste0(f, ":%S")
  } }
  if (am.pm.bool){ f <- paste(f, "%p") }
  paste("%m/%d/%Y", f)
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
  for (j in 1:length(sitelist.1)){
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

#i <- filelist[1]
#j <- 1
#str(result)
#head(result[[1]])

# Quickest way to get documentation:
# ----------------------------------
#?strsplit

# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
#                            Los Angeles Parser
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

filelist <- list.files("X:\\Data\\calibration\\LA_reference_data")

result <- list()

k <- 1

# Loop through the files
for (i in filelist){

  sname <- R.scan(i, "_", 1)
  parm <- R.scan(i, "_", 2)

  # Read plain text
  temp <- read.xlsx(paste0("X:\\Data\\calibration\\LA_reference_data\\",i),1)
  temp$value2 <- as.numeric(as.character(temp$Value))
  temp$date <- temp[,1]
  temp$site <- rep(sname, dim(temp)[1])
  temp$pollutant <- rep(parm, dim(temp)[1])
  
  # Read and format date

  result[[k]] <- temp
  k <- k+1
}


# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
#                               PSCAA Parser
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

filelist <- list.files("X:\\Data\\calibration\\Sea_reference_data")

result <- list()

k <- 1

# Loop through the files
for (i in filelist){
  sname <- R.scan(i, "_", 1)

  # Read plain text
  temp <- read.csv(paste0("X:\\Data\\calibration\\Sea_reference_data\\",i))
  poll.cols <- 2:dim(temp)[2]
  poll.pos <- grep("Pm25|NO|O3|CO",unlist(strsplit(names(temp)[2],"[.]")))
  parms <- R.scan(names(temp)[poll.cols],"[.]",poll.pos)
  names(temp)[poll.cols] <- parms
  temp$sitename <- rep(sname, dim(temp)[1])
  temp$date <- strptime(temp[,1], format = get.format(as.character(temp[,1])))

  # Read and format date
  result[[k]] <- temp
  k <- k+1
}

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
    