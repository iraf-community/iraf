#{ AVERAGE -- Compute the average and standard deviation of a list of numbers.
# Numeric input is via the standard input (one number per line).  The mean,
# sigma, and number of samples are written to the standard output.
#
# By default (Average called with no arguments), the sample is taken
# to be the set of numbers in the standard input when Average is run.  
# Additional points can be added to or deleted from the sample by
# rerunning Average with one of the following arguments:
# 
#	add - add points to the sample, recalculate mean and sigma
#	sub - subtract points from the sample


{
	if ($nargs == 0)		# if no arguments, don't query
	    opstring = "new_sample"
	else
	    opstring = option		# get parameter "option"


	if (opstring == "add")		# add, subtract, or start over?
	    addsub_flag = 1
	else if (opstring == "sub" || opstring == "subtract")
	    addsub_flag = -1
	else {				# new sample (default)
	    addsub_flag = 1
	    n = 0			# number of points in sample
	    sum = 0
	    sumsqrs = 0
	}


	# Process data;  print mean, sigma, and the sample size on
	# the standard output.

	while (scan (data_value) != EOF)
	    if (nscan() == 1 && data_value != INDEF) {
		n += addsub_flag
		sum += addsub_flag * data_value 
		sumsqrs += addsub_flag * data_value * data_value
	    }

	if (n <= 0)
	    print ("INDEF ", "INDEF ", n)
	else if (n == 1)
	    print (sum, "INDEF ", 1)
	else {
	    mean = sum / n
	    variance = (n * sumsqrs - sum * sum) / (n * (n-1))
	    if (variance < 0)		# possible with roundoff error
		sigma = 0.0
	    else
		sigma = sqrt (variance)
	    print (mean, sigma, n)
	}
}
