#{ _MSFINDSPEC2 -- Create a new database, initialize with a template
# image, refine the positions, and fit a position function.

#image,f,a,,,,Image
#template,f,a,,,,Template image
#width,r,a,10,,,Width of spectra
#naverage,i,a,20,1,,Number of lines to average
#verbose,b,a,no,,,Verbose output?

{
	# Verbose message.
	if (verbose) {
	    time
	    print ("  Find the spectra in ", image, " using template image ",
		template, ".")
	}

	# Create a new database and initialize with a template image.
	newextraction (image, template)

	# Refit the model.
	fitgauss5 (image, 1, lower=-width/2, upper=width/2,
	    lines="*", spectra="*", naverage=naverage, track=no,
	    algorithm=2)

	# Fit the default interpolation function to the positions.
	fitfunction (image, parameter="x0", lines="*", spectra="*")
}
