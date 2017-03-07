#######################################################################
#    
#	** Convert HYDAT database file into CFA-readable ASCII files **
#
#	** USE AT YOUR OWN RISK, NO WARRANTY **
#
# Developed by Nick(WeiTao) Rong
# Watershed Hydrology Group, UBC Forestry
#
# Last modified: March 5th, 2017
#
# Memo of modification: 
# 1) Change the data input from MS mdb to SQLite
#######################################################################


################## **** Read Me!! **** ###################
#	This script will output either mean-daily AMS or
#	instantaneous peaks. You need to specify the location
#	of your HYDAT mdb file and the script will generate a
#   sub-directory containing all ascii files with the
#   WSC station number as file name with no extension
#
#	** IMPORTANT NOTICE **
#	Because this script does not filter out any stations,
#	users are highly recommended to screen the dataset
#	first with EC DataExplorer software available on
#	the internet (Windows OS only)
#
#	To use this script, you need to change:
#	1) hydat.input.location (line 50)
#	2) ascii.output.location (line 56)
#	3) INST (line 59)
##########################################################

rm(list=ls()) # good habit to clean the workspace first

########### **** R Packages Required **** ################
require(RSQLite) # read SQLite database




########### **** Read in HYDAT dataset **** ##############
# Download the .sqlite3 HYDAT dataset: ftp://ftp.tor.ec.gc.ca/HYDAT/
# Unzip the hydat dataset
# Where is the .mdb file located? Including the file name & extension
# The whole script might take 1~2 mins

hydat.file.location = "/Users/nickrong/Dropbox/sicamous/GIS/Hydat_Jan2017.sqlite3"


########### **** Output ascii files **** ##############
# Where you want the ascii file folder to be located?
# Do not forget the "/" at the end...
ascii.output.location = "/Users/nickrong/Desktop/hydat_inst/"

# WANT INSTANEOUS PEAKS (TRUE) OR MEAN-DAILY ANNUAL PEAKS (FALSE)?
INST = TRUE

#	After modifying the items above, run the entire script in R
#	Only Modify Things Below If You Know What You Are Doing!!!





# Read the database and store information in list
hydat.file = dbConnect(drv=RSQLite::SQLite(), dbname = hydat.file.location)

# The actual hydat database is huge, extract just the table of info.
hydat.table = dbListTables(hydat.file)


# List containing all the heading info
for (i in 1:length(hydat.table)) {

	if (i == 1) {hydat.heading = list()}
	hydat.heading[[i]] = dbListFields(hydat.file, hydat.table[i])

} #end of heading for loop



if (INST == TRUE) {
	# Annual peak flows are stored in "ANNUAL_INSTANT_PEAKS" (Instantaneous peaks)
	hydat.Qmax = dbGetQuery(hydat.file, "select * from ANNUAL_INSTANT_PEAKS 
							where DATA_TYPE ='Q' AND PEAK_CODE = 'H'")

	hydat.allQ = data.frame(
			STATION_NUMBER = hydat.Qmax$STATION_NUMBER,
			YEAR = hydat.Qmax$YEAR,
			MONTH = formatC(as.numeric(hydat.Qmax$MONTH), width=2, flag="0"),
			FLOW = hydat.Qmax$PEAK,
			# DATA_TYPE has to be the last one so I can remove it easily later
			DATA_TYPE = hydat.Qmax$DATA_TYPE)
} else{

	# Annual peak flows are stored in "ANNUAL_STATISTICS" (Mean Daily Max)
	hydat.Qmax = dbGetQuery(hydat.file, "select * from ANNUAL_STATISTICS 
							where DATA_TYPE ='Q'")
	
	hydat.allQ = data.frame(
			STATION_NUMBER = hydat.Qmax$STATION_NUMBER,
			YEAR = hydat.Qmax$YEAR,
			MONTH = formatC(as.numeric(hydat.Qmax$MAX_MONTH), width=2, flag="0"),
			FLOW = hydat.Qmax$MAX,
			# DATA_TYPE has to be the last one so I can remove it easily later
			DATA_TYPE = hydat.Qmax$DATA_TYPE)
}


# Station information in "STATIONS"
hydat.readstation = dbGetQuery(hydat.file, "select * from STATIONS")
hydat.allSTATION = data.frame(
			STATION_NUMBER = hydat.readstation$STATION_NUMBER,
			STATION_NAME = hydat.readstation$STATION_NAME,
			PROVINCE = hydat.readstation$PROV_TERR_STATE_LOC,
			AREA = hydat.readstation$DRAINAGE_AREA_GROSS
			)

# Create the output folder if not exist already
dir.create(file.path(ascii.output.location), showWarnings = FALSE)
setwd(file.path(ascii.output.location))

# loop to generate one ASC file each station...
for (loop1 in 1:length(hydat.allSTATION$STATION_NUMBER)){

	# Which station we are working on?
	station = as.character(hydat.allSTATION$STATION_NUMBER[loop1])

	# Subset out Annual Peaks of just this station and just Flow (DATA_TYPE == Q)
	station.Q = subset(hydat.allQ, STATION_NUMBER == station)
	input.Q = station.Q[,1:4] # remove DATA_TYPE
	input.Q = input.Q[order(input.Q$YEAR),] # sort by years

	# filter out stations without records
	if (length(input.Q$YEAR) != 0) {
		# Initial file with the file name as the station number; no extension
		CFAinput <- file(paste0(ascii.output.location, station), "w")

		# Writting header info
		cat(paste0(hydat.allSTATION$STATION_NUMBER[loop1],"\n"), file = CFAinput)
		cat(paste0(hydat.allSTATION$STATION_NAME[loop1],"\n"), file = CFAinput)
		cat(paste0(length(input.Q$YEAR),"    ",
			hydat.allSTATION$AREA[loop1],"\n"), file = CFAinput)

		# Appending the flow data to the file (no col/row names)
		write.table(input.Q, file = CFAinput, append = TRUE, 
			col.names = FALSE, row.names = FALSE, quote = FALSE)

		# Finish the file writing
		close(CFAinput)
	}

	completion = (loop1/length(hydat.allSTATION$STATION_NUMBER))*100

	if(completion %% 5 < 0.01) {
		print(paste0(round(completion, digits = 0), "% of stations output completed"))
	}

} # End of loop for exporting each WSC station into a separated ascii file

#################### EOF ####################




