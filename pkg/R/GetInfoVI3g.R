GetInfoVI3g <- structure(function(
	##title<<
	## Get meta-information from GIMMS VI3g binary file names
	
	##description<< This function extracts the date and satellite from the GIMMS VI3g file names.

	file
	### GIMMS VI3g file name
) {
	# get date and satellite information from file name
	geo <- strtrim(file, 3)
	file.str <- gsub(geo, "", file)
	year <- strtrim(file.str, 2)
	file.str <- gsub(year, "", file.str)
	month <- strtrim(file.str, 3)
	file.str <- gsub(month, "", file.str)
	period <- strtrim(file.str, 3)
	file.str <- gsub(period, "", file.str)
	sat <- gsub(".", "", strtrim(file.str, 4), fixed=TRUE)

	# convert month abbreviation to month number
	month <- grep(month, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

	# convert period to day
	if (period == "15a") day <- 01	# means first 15 days
	if (period == "15b") day <- 16	# means second 15 days

	# create date 
	date <- as.Date(paste(year, month, day, sep="-"), format="%y-%m-%d")
	return(list(date=date, sat=sat))
	### The function returns a list with $date and $sat.
}, ex=function() {
# GetInfoVI3g("geo00oct15a.n14-VI3g")
})