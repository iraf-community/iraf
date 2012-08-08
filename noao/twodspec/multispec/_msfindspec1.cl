#{ _MSFINDSPEC1 -- Create a new database, find the peaks, trace, and fit a
# function.

#image,f,a,,,,Image
#sample_lines,s,a,"10x50",,,Sample image lines
#start,i,a,1,,,Starting image line
#min_nspectra,i,a,1,,,Minimum number of spectra to be found
#max_nspectra,i,a,100,,,Maximum number of spectra to be found
#separation,i,a,20,,,Minimum separation between spectra
#threshold,r,a,0.,,,Minimum peak threshold for selecting spectra
#contrast,r,a,0.1,,,Maximum contrast between peaks
#width,r,a,10,,,Width of spectra
#naverage,i,a,20,1,,Number of lines to average
#verbose,b,a,no,,,Verbose output?

{
	# Verbose message.
	if (verbose) {
	    time
	    print ("  Find the spectra in ", image, ".")
	}

	# Create a new database.
	newextraction (image, "", sample_lines=sample_lines)

	# Find the peaks.
	findpeaks (image, start, contrast, separation=separation,
	    threshold=threshold, min_npeaks=min_nspectra, edge=width/3,
	    max_npeaks=max_nspectra, naverage=naverage)

	# Initialize the model parameters and fit the model with tracking.
	msset (image, "s0", 1., lines=start)
	msset (image, "s1", 0., lines=start)
	msset (image, "s2", 0., lines=start)
	fitgauss5 (image, start, lower=-width/2, upper=width/2,
	    lines="*", spectra="*", naverage=naverage, track=yes,
	    algorithm=2)

	# Fit the default interpolation function to the positions.
	fitfunction (image, parameter="x0", lines="*", spectra="*")
}
