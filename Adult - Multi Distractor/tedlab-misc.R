###############################################################
## Library for tedlab -- miscellaneous functions
## October 2009
###############################################################

mean.na.rm <- function(x) { mean(x,na.rm=T) }


# This reads in a turk file, and transforms it. It takes columns that end in numbers, and lines 
# up the columns by their number and all other column info to create a new row. So for each
# column ending in a number, it makes a row of those columns. 
read.turk.rating.file <- function(f, list.number) {
	tmp <- read.table(f, header=T, sep=",", stringsAsFactors=FALSE) # csv file with header
	
	## Reconfigure all of the data:	
	# a list of extracted item numbers--the last number in each header (assuming there are less than 3 or 4)
	item.numbers <- as.numeric(gsub("[^0-9]*[0-9]*[^0-9]*[0-9]*[^0-9]*[0-9]*[^0-9]+", "", names(tmp), perl=T)) # what a hack
	item.numbers

	data <- data.frame(NULL)

	# loop over each item number
	for(i in min(item.numbers, na.rm=T):max(item.numbers, na.rm=T)) {
		# extract all 
		row <- tmp[,(item.numbers==i)  # keep columns with the right number
			     | is.na(item.numbers)  #or no number
				] # a block of rows, one for each subject in the experiment
				
		row$PresentationOrder <- i
		row$Subject <- tmp$WorkerId 
		row$ListNumber <- list.number
		names(row) <- gsub("[0-9]+$", "", names(row), perl=T) # remove item numbers from row
		# sort these row elements by their column name, minus the numbers, so we add to data correctly
		row <- row[,order(names(row))] 
		data <- rbind(data, row)
		names(data) <- names(row)[order(names(row))] # save this. Slightly inefficient. 
	}

	data
}










