#{ _MSFINDSPEC3 -- Refine the model and fit a position function.

#image,f,a,,,,Image
#width,r,a,10,,,Width of spectra
#naverage,i,a,20,1,,Number of lines to average
#verbose,b,a,no,,,Verbose output?

{
	# Verbose message.
	if (verbose) {
	    time
	    print ("  Refit the spectra in ", image, ".")
	}

	# Refit the model.
	fitgauss5 (image, 1, lower=-width/2, upper=width/2,
	    lines="*", spectra="*", naverage=naverage, track=no,
	    algorithm=2)

	# Fit the default interpolation function to the positions.
	fitfunction (image, parameter="x0", lines="*", spectra="*")
}
