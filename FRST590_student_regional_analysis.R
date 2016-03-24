##########################################################
# 			
#			*** Regional Analysis R Script ***
#
# Developed by Nick(WeiTao) Rong
# MSc Candidate, UBC Forestry
# Use at your own risk, ABSOLUTELY NO WARRANTY
#
# Last modified: March 22, 2016
#
##########################################################

################## **** Read Me!! **** ###################
#	The regional analysis method in this script follows 
#	the methods described in YZ Wang's PhD thesis, titled:
#
#	"Development of methods for regional flood estimates
#	 in the province of British Columbia, Canada"
#
#              *** NOTE !!!! ***
#
#   The script is designed to work with AMS of Mean-Daily only
#	The distribution is Generalized Logistic (GLOG), if other
#   	distribution is needed you need to modify this script
##########################################################

rm(list=ls()) # good habit to clean the workspace findRestart

########### **** R Packages Required **** ################
library(Hmisc) # mdb.get() read MS Access database
library(homtest) # for the HW.test() for regional homogeneitiy
library(lmomRFA) # regtst() to test regional homogeneity
library(lmom) # samlmu() to calculate sample l-moments

# Note mdb.get() of {Hmisc} requires mdbtools package on the OS level
# For Mac, install mdbtools use brew or macports
# For Linux, install mdbtools use apt-get
# For Windows with Cygwin, install mdbtools


########### **** Read in HYDAT dataset **** ##############
# Download the .mdb HYDAT dataset: ftp://ftp.tor.ec.gc.ca/HYDAT/
# Unzip the hydat dataset
# Where is the .mdb file located?
hydat.file.location = "/Users/nickrong/Dropbox/sicamous/GIS/Hydat_Jan2016.mdb"


######### **** Input Regional Station Numbers **** #########
# Give the region a name (eg: NMR83)
region.name = "NMR83"

# STATION.NUMBER of each selected stream gauge in the region
# The below list is for NMR8-3 region in YZ Wang thesis
region.station.list = c(
"08LC004", "08LC034", "08LE024", "08LE041", "08LE043", "08LE108", "08NB013",
"08NB019", "08ND001", "08ND006", "08ND009", "08ND012", "08ND013", "08ND014",
"08NA018", "08ND019", "08NE001", "08NE006", "08NE008", "08NE021", "08NE110",
"08NE117", "08NH066", "08NJ164", "08NN023")
##########################################################


#   ***** COPY AND PASTE EVERYTHING AND RUN IT IN R *****
#   ***** The process can take 2~5 mins *****





#  Only Modify Things Below If You Know What You Are Doing!!!

# Read the database and store information in list
hydat.all = mdb.get(hydat.file.location)

# The actual hydat database is huge, extract just the table of info.
hydat.table = mdb.get(hydat.file.location, tables = TRUE)

hydat.Qmax = subset(hydat.all[[19]], DATA.TYPE == 'Q')
# Annual peak flows are stored in hydat.all[[19]] -->"ANNUAL_STATISTICS" (Mean Daily Max)
hydat.allQ = data.frame(
		STATION.NUMBER = hydat.Qmax$STATION.NUMBER,
		YEAR = hydat.Qmax$YEAR,
		MONTH = formatC(as.numeric(hydat.Qmax$MAX.MONTH), width=2, flag="0"),
		FLOW = hydat.Qmax$MAX,
		# DATA.TYPE has to be the last one so I can remove it easily later
		DATA.TYPE = hydat.Qmax$DATA.TYPE
		)


# Station information in hydat.all[[26]] -->"STATIONS"
hydat.allSTATION = data.frame(
			STATION.NUMBER = hydat.all[[26]]$STATION.NUMBER,
			STATION.NAME = hydat.all[[26]]$STATION.NAME,
			PROVINCE = hydat.all[[26]]$PROV.TERR.STATE.LOC,
			AREA = hydat.all[[26]]$DRAINAGE.AREA.GROSS,
			LAT = hydat.all[[26]]$LATITUDE,
			LONG = hydat.all[[26]]$LONGITUDE
			)


# Function to calculate Station L-statistics
samplelmom = function(allSTATION = hydat.allSTATION, allQ = hydat.allQ, 
	zones = region.name, station.list = region.station.list) {

	stn.lmom = data.frame()

	for (loop2 in 1:length(station.list)) {
		station = station.list[loop2]
		stn.info = subset(allSTATION, STATION.NUMBER == station)
		stn.info$avail = length(subset(allQ, STATION.NUMBER == station & !is.na(FLOW))$YEAR)
		
		stn.flow = subset(allQ, STATION.NUMBER == station)$FLOW
		stn.flow = stn.flow[!is.na(stn.flow)]

		stn.info$AREA[is.na(stn.info$AREA)] = 0
		stn.info$l_1 = samlmu(stn.flow)[1]
		stn.info$l_2 = samlmu(stn.flow)[2]
		stn.info$lcv = samlmu(stn.flow)[2]/samlmu(stn.flow)[1]
		stn.info$t_3 = samlmu(stn.flow)[3]
		stn.info$t_4 = samlmu(stn.flow)[4]
		stn.info$t_5 = samlmu(stn.flow, nmom = 5)[5]
		stn.info$zones = zones
		stn.info$stn_k = -(stn.info$t_3)
		stn.info$alpha = (stn.info$l_2 * sin(stn.info$stn_k) * pi)/(stn.info$stn_k * pi)
		stn.info$zeta= (stn.info$l_1) + ((stn.info$l_2 - stn.info$alpha) / stn.info$stn_k)

		stn.lmom = rbind.data.frame(stn.lmom, stn.info)

	}

return(stn.lmom)
}


# Function to create a data frame with format readable by lmomRFA{}
convert.regdata = function(stn.df) {
	df <- data.frame(
	name = stn.df$STATION.NUMBER,
	n = stn.df$avail,
	mean = stn.df$l_1,
	LCV = stn.df$lcv,
	L_skewness = stn.df$t_3,
	L_kurtosis = stn.df$t_4,
	t_5 = stn.df$t_5
	)
	# Convert to class "regdata"
	output.df<-as.regdata(df)

return(output.df)
}

# Calculate station L-stats in the region
region.regdata = samplelmom()

# Homogeneity test of the region
region.homo.test = summary(regtst(convert.regdata(region.regdata)))
regional.lstats = regavlmom(convert.regdata(region.regdata))

region.homo.test