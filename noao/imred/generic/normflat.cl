#{ NORMFLAT -- Make a flat field by normalizing and replacing low values.

# image,f,a,,,,Calibration image
# flatfield,f,a,,,,Flat field image
# norm,r,h,INDEF,,,Normalization if not INDEF
# minflat,r,h,INDEF,,,Minimum data value to use in the flat field
# sample_section,s,h,"[]",,,Sample section for determining normalization
# keeplog,b,h,@generic.keeplog,,,Keep log of processing?
# logfile,f,h,@generic.logfile,,,Log file
# img,f,h
# flt,f,h
# tmp,f,h
# rlist,*s,h
# mean,r,h
# junk,s,h
# stat,i,h

{
	# Get query parameters and set temporary parameters.
	img = image
	flt = flatfield
	tmp = mktemp ("tmp$gec")

	# Startup message.
	if (keeplog) {
	    time (>> logfile)
	    print ("  NORMFLAT: Create a flat field.\n", >> logfile)
	    print ("  Calibration image: ", img, >> logfile)
	    print ("  Flat field: ", flt, >> logfile)
	    if (minflat != INDEF)
		print ("  Minimum data value used in flat field = ", minflat,
		    >> logfile)
	}

	# Determine normalization.
	if (norm == INDEF) {
	    # Determine the mean of the sample region.

	    imstatistics (img // sample_section, lower=minflat, upper=INDEF,
		verbose=no, > tmp)
	    rlist = tmp
	    stat = fscan (rlist, junk, junk, mean)
	    rlist = ""
	    delete (tmp, verify=no)
	} else
	    mean = norm

	if (keeplog)
	    print ("  Normalization = ", mean, >> logfile)

	# Replace low values by the mean and normalize.
	if (mean != 0.) {
	    if (minflat != INDEF) {
		imcopy (img, tmp, verbose=no)
	        imreplace (tmp, mean, upper=minflat)
	        imarith (tmp, "/", mean, flt, pixtype="real")
		imdelete (tmp, verify=no)
	    } else
	        imarith (img, "/", mean, flt, pixtype="real")
	} else
	    print ("  ERROR: Cannot normalize calibration image.")

	# Ending message.
	if (keeplog) {
	    time (>> logfile)
	    print ("  NORMFLAT: Done.", >> logfile)
	}
}
