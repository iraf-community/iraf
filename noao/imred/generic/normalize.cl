#{ NORMALIZE -- Compute the average of a sample region and normalize.

#images,s,a,,,,Images to be normalized
#norm,r,h,INDEF,,,Normalization value
#sample_section,s,h,"[]",,,Sample section
#lower,r,h,INDEF,,,Lower limit of data values for sampling
#upper,r,h,INDEF,,,Upper limit of data values for sampling
#keeplog,b,h,@generic.keeplog,,,Keep log of processing?
#logfile,f,h,@generic.logfile,,,Log file
#imlist,f,h
#meanlist,f,h
#input,f,h
#imfd,*s,h
#meanfd,*s,h
#mean,r,h
#stat,i,h

{
	# Startup message.
	if (keeplog) {
	    time (>> logfile)
	    print ("  NORMALIZE: Normalize images.", >> logfile)
	}

	# Set temporary files.
	imlist = mktemp ("tmp$ims")
	meanlist = mktemp ("tmp$ims")

	# Generate image list.
	files (images, sort-, >imlist)
	imfd = imlist

	while (fscan (imfd, input) != EOF) {

	    if (norm == INDEF) {
	        # Determine the mean of the sample region.
	        imstatistics (input // sample_section, fields="mean",
		    lower=lower, upper=upper, format=no, >meanlist)
	        meanfd = meanlist
	        stat = fscan (meanfd, mean)
		meanfd = ""			# close list file
	        delete (meanlist, verify=no)
	    } else
		mean = norm

	    # Print output.
	    if (keeplog) {
		time (>> logfile)
	        print ("  Normalization  for ", input, " = ", mean, >> logfile)
	    }

	    if (mean != 0.) {
    
	        # Normalize the image by the mean.

	        imarith (input, "/", mean, input, pixtype="real",
		    calctype="real")
	    } else
		print ("  WARNING: Cannot normalize ", input, ".")
	}


	delete (imlist, verify=no)

	# Ending message.
	if (keeplog) {
	    time (>> logfile)
	    print ("  NORMALIZE: Done.", >> logfile)
	}
}
