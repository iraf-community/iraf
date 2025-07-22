# GHOST2MS - Convert Gemini/DRAGONS GHOST format to IRAF multispec format.

procedure ghost2ms (input)

file	input			{prompt="Input GHOST file from DRAGONS"}
string	output = ""		{prompt="Output base name"}
int	order1 = 1		{prompt="Starting order to extract"}
int	order2 = 33		{prompt="Ending order to extract"}
int	version = 1		{prompt="Version to extract"}
bool	clobber = no		{prompt="Clobber existing output?"}
bool	verbose = yes		{prompt="Verbose output?"}

begin
	file	in, out, sci, awav
	int	nvers, ndisp, nspec, order
	int	nver, ns, o1, o2, c1, c2
	real	w1, w2, dw,  unitscale
	string	unit


	# Set input and output.
	in = input
	if (strstr(".fits",in) > 0)
	    in = substr (in, 1, strstr(".fits",in)-1)
	out = output
	if (out == "")
	    out = in // "_iraf"
	else if (strstr(".fits",out) > 0)
	    out = substr (out, 1, strstr(".fits",out)-1)

	if (imaccess(out) == YES) {
	    if (clobber) {
	        imdel (out)
		del (out//".hdr")
		del (out//".aptable")
	    } else {
		printf ("%s already exists\n", out)
		return
	    }
	}

	# Extract a header for the final file.
	imhead (in//"[1]", l+, > out//".hdr")

	# Check the format.
	imextensions (in, output="none", index="1-", extname="SCI",
	    extver="", lindex=no, lname=yes, lver=yes, ikparams="")
	nvers = imextensions.nimages
	nver = max (1, min (nvers, version))

	# Get the size information and set the orders to extract.
	printf ("%s[SCI,%d]\n", in, nver) | scan (sci)
	hselect (sci, "NAXIS1,NAXIS2", yes) | scan (ndisp, nspec)
	o1 = max (1, min (nspec, order1))
	o2 = max (1, min (nspec, order2))
	printf ("%s[SCI,%d][*,%d:%d]\n", in, nver, o1, o2) | scan (sci)

	if (verbose)
	    printf ("%s -> %s\n", sci, out)

	# Create an aperture table.  The dispersion is set to linear
	# with the average dispersion per pixel.
	# Because there is no simple way to define the units we
	# set the dispersion to the assumed Angstroms units.  The user
	# can then use the options to display in whatever desired
	# units.

	if (access(out//".aptable"))
	    delete (out//".aptable")
	match ("CUNIT1", out//".hdr") | scan (unit, unit, unit)
	if (unit == "'nm")
	    unitscale = 10
	else
	    unitscale = 1
	for (ns=o1; ns<=o2; ns += 1) {
	    printf ("%s[AWAV,%d][1,%d]\n", in, nver, ns) | scan (awav)
	    listpix (awav) |& match ("Warn", stop+) | scan (w1, w1)
	    w1 *= unitscale
	    printf ("%s[AWAV,%d][%d,%d]\n", in, nver, ndisp, ns) | scan (awav)
	    listpix (awav) |& match ("Warn", stop+) | scan (w2, w2)
	    w2 *= unitscale
	    dw = (w2 - w1) / (ndisp - 1)
	    printf ("%d %d 0 %.g %.3g INDEF INDEF INDEF INDEF\n",
	        ns, 0, w1, dw, >> out//".aptable")
	}

	# Use MKECHELLE to take care of the basic multispec structure.
	ns = o2 - o1 + 1
	order = o1 + ns / 2
	mkechelle (out, yes, ncols=1, nlines=ndisp, norders=ns,
	    title="", header=out//".hdr", list=no, make=yes, comments=no,
	    xc=235.5, yc=INDEF, pixsize=0.027, profile="extracted",
	    width=20., scattered=0., f=590., gmm=INDEF, blaze=INDEF,
	    theta=INDEF, order=order, wavelength=5007.49, dispersion=2.61,
	    cf=590., cgmm=226., cblaze=4.53, ctheta=-11.97, corder=1,
	    cwavelength=6700., cdispersion=70., rv=0., z=no,
	    continuum=1000., temperature=5700., lines="", nrandom=k,
	    peak=-0.5, sigma=0.5, seed=i, >& "dev$null")

	# Update the dispersion to the input.
	sapertures (out, apertures="", apidtable=out//".aptable",
	    wcsreset="no", verbose=no)

	# Replace the MKECHELLE fluxes with the input fluxes.
	imcopy (sci, out//"[*,*]", v-)

end
