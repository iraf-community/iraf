#{ FLATTEN -- Divide images by a flat field

#images,s,a,,,,Images to be flattened
#flatfield,f,a,,,,Flat field
#minflat,r,h,INDEF,,,Minimum flat field value
#pixtype,s,h,"real",,,Flattened image pixel datatype
#keeplog,b,h,@generic.keeplog,,,Keep log of processing?
#logfile,f,h,@generic.logfile,,,Log file
#imlist,f,h
#imfd,*s,h
#input,f,h
#flat,f,h
#flt,f,h

{
	# Startup message.
	if (keeplog) {
	    time (>> logfile)
	    print ("  FLATTEN: Flatten images.", >> logfile)
	}

	# Set temporary files.
	imlist = mktemp ("tmp$ims")

	# Replace low flat field values if needed.
	flat = flatfield
	if (minflat == INDEF)
	    flt = flat
	else {
	    if (keeplog)
		print ("  Minimum flat field value = ", minflat, >> logfile)
	    flt = mktemp ("tmp$ims")
	    imcopy (flat, flt, verbose=no)
	    imreplace (flt, 1., upper=minflat)
	}

	# Generate image list.
	sections (images, option="fullname", >imlist)
	imfd = imlist

	while (fscan (imfd, input) != EOF) {

	    # Print output.
	    if (keeplog) {
	        time (>> logfile)
	        print ("  Flatten ", input, " with ", flat, ".", >> logfile)
	    }

	    # Flatten the image with the flat field.  Replace the input
	    # image by the flattened image.

	    imarith (input, "/", flt, input, pixtype=pixtype, calctype="real")
	}

	if (minflat != INDEF)
	    imdelete (flt, verify=no)
	delete (imlist, verify=no)

	# Ending message.
	if (keeplog) {
	    time (>> logfile)
	    print ("  FLATTEN: Done.", >> logfile)
	}
}
