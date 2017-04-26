# DARKSUB -- Scale and subtract a dark count image.

procedure darksub (input, output, darkimage)

string	input		{prompt="Input images to be dark count subtracted"}
string	output		{prompt="Output dark count subtracted images"}
file	darkimage	{prompt="Dark count image"}

string	exposure	{prompt="Header parameter for exposure times"}
string	pixtype="1"	{prompt="Pixel type of final images"}
bool	verbose=yes	{prompt="Verbose output?"}
struct	*list1
struct	*list2

begin
	file	darkim
	file	dark
	file	file1
	file	file2
	string	in
	string	out
	real	exp
	real	expd
	real	expi
	int	stat

	# Make temporary filenames.
	dark = mktemp ("tmp")
	file1 = mktemp ("tmp")
	file2 = mktemp ("tmp")

	# Determine exposure time of dark image.  Quit if no exposure time.
	darkim = darkimage
	hselect (darkim, exposure, yes, > file1)
	list1 = file1
	stat = fscan (list1, expd)
	list1 = ""
	delete (file1, verify=no)

	if (stat == EOF || nscan() < 1)
	    error (1, "Exposure time for " // darkim // " not found.")
	if (expd == 0.)
	    error (2, "Exposure time for " // darkim // " is zero.")
	exp = expd

	# Make a temporary image for the scaled dark.
	imcopy (darkim, dark, verbose=no)

	# Expand the list of input and output images in temporary files.
	hselect (input, "$I,"//exposure, yes, > file1)
	sections (output, option="root", > file2)

	# Loop through the input and output images.
	list1 = file1
	list2 = file2
	while (fscan (list1, in, expi) != EOF) {

	    stat = nscan()

	    # Check that the output list has not been exhausted.
	    if (fscan (list2, out) == EOF) {
		print ("  Output list exhausted before input list.")
		break
	    }

	    # Check that there is an exposure time for the input image.
	    if (stat < 2) {
		print ("  Exposure time for ", in, " not found.")
		next
	    }
	    if (expi == 0.) {
		print ("  Exposure time for ", in, " is zero.")
		next
	    }

	    # Print log output.
	    if (verbose) {
	        time ()
		print ("  ", out, " = ", in, " - ", expi/expd, "* ", darkim)
	    }

	    # Scale the dark image if necessary.
	    if (expi != exp) {
		imarith (dark, "*", expi / exp, dark, title="", divzero=0.,
    		    hparams="", pixtype="", calctype="", verbose=no, noact=no)
		exp = expi
	    }

	    # Subtract the dark image from the input image.
	    imarith (in, "-", dark, out, title="", divzero=0.,
		hparams="", pixtype=pixtype, calctype=pixtype,
		verbose=no, noact=no)
	}

	# Finish up.
	imdelete (dark, verify=no)
	delete (file1, verify=no)
	delete (file2, verify=no)
end
