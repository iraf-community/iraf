# HIRESWCAL -- Apply HIRES wavelengths to flux data to produce an IRAF file.
#
# This script requires the onedspec, proto, and artdata packages be loaded.

procedure hireswcal (input, waves, output)

file	input		{prompt="Input hires data file"}
file	waves		{prompt="Input matching hires wavelength file"}
file	output		{prompt="Output IRAF file"}

struct	*fd		# Required to loop through a list.

begin
	file	in, win, out
	file	im, wim, out1, tmp
	int	ap

	# Set query parameters.
	in = input
	win = waves
	out = output

	# Check if output is already present.
	if (imaccess(out))
	    error (1, "Output already exists ("//out//")")

	# Define a temporary file rootname.
	tmp = mktemp ("tmp")

	# Expand input into a list of spectra.
	slist (in, apertures="", long-, > tmp)

	# For each spectrum in the list create an IRAF 1D spectrum.
	fd = tmp
	while (fscan (fd, im, ap) != EOF) {

	    # Form names for each spectrum.
	    printf ("%s[*,%d]\n", in, ap) | scan (im)
	    printf ("%s[*,%d]\n", win, ap) | scan (wim)
	    printf ("%s_%d\n", tmp, ap) | scan (out1)

	    # Dump the wavelengths and flux and put together into
	    # a file for rspectext.

	    listpix (wim, v-, > tmp//"waves")
	    listpix (im, v-, > tmp//"flux")
	    joinlines (tmp//"waves", tmp//"flux") |
	        fields ("STDIN", "2,4", > tmp//"join")

	    # Create the IRAF spectrum.
	    rspectext (tmp//"join", out1, title="", flux-, dtype="interp")

	    # Delete working files.
	    delete (tmp//"[wfj]*", v-)
	    print (out1, >> tmp//".list")
	}
	fd = ""; delete (tmp, v-)

	# Put the 1D spectrum into a multispec file.
	scopy ("@"//tmp//".list", out, format="multispec", renumber+)

	# Add the input header for what its worth.
	mkhead (out, in, append+, verbose-)

	# Finish up.
	imdelete ("@"//tmp//".list", v-)
	delete (tmp//"*", v-)
end
