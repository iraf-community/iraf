# RSPECTEXT -- Read a 1D ascii text spectrum into an image spectrum
# The image is created from an optional header and the flux values
# using RTEXTIMAGE.  If there is no header the title, dispersion,
# and flux calibration may be set.  The dispersion can be defined
# as linear, log linear, or from the wavelengths.  The latter may be
# used as a lookup table in the image header or used to interpolate
# the spectrum to a linear wavelength dispersion.

procedure rspectext (input, output)

string	input			{prompt="Input list of text spectra"}
string	output			{prompt="Output list of image spectra"}

string	title = ""		{prompt="Spectrum title"}
bool	flux = no		{prompt="Flux calibrated?"}
string	dtype = "linear"	{prompt="Dispersion type",
				 enum="none|linear|log|nonlinear|interp"}
real	crval1 = 1.		{prompt="Coordinate of first pixel"}
real	cdelt1 = 1.		{prompt="Coordinate interval per pixel"}

struct	*fd1, *fd2

begin
    int		dim
    string	specin, specout, spec, temp1, temp2, temp3, temp4
    bool	header=no
    bool	log=no

    specin = mktemp ("tmp$iraf")
    specout = mktemp ("tmp$iraf")
    spec = mktemp ("tmp$iraf")
    temp1 = mktemp ("tmp$iraf")
    temp3 = mktemp ("iraf")
    temp2 = "tmp$id"//temp3

    # Expand the input and output lists.
    files (input, sort=no, > specin)
    files (output, sort=no, > specout)
    join (specin, specout, output=spec, delim=" ", shortest=yes, verbose=yes)
    delete (specin, verify-)
    delete (specout, verify-)

    # Go through each input and check for an existing output.
    fd2 = spec
    while (fscan (fd2, specin, specout) != EOF) {
	if (access(specout)||access(specout//".imh")||access(specout//".hhh")) {
	    print ("Image "//specout//" already exists")
	    next
	}

	# Separate the header and flux values for RTEXTIMAGE and the
	# wavelengths for later use.

	rstext (specin, temp1, temp2, header=header) | scan (header, dim)

	# Create the image from the header and flux values.
	rtextimage (temp1, specout, otype="real", header=header, pixels=yes,
	    nskip=0, dim=dim)
	fd1 = ""; delete (temp1, verify-)

	# If there is no header setup the title, dispersion and flux.
	# The dispersion may require using DISPCOR for nonlinear or
	# resampled dispersion functions.

	if (!header) {
	    hedit (specout, "title", title,
		add+, addonly-, del-, update+, verify-, show-)
	    if (dtype == "linear") {
		hedit (specout, "dc-flag", 0,
		    add+, addonly-, del-, update+, verify-, show-)
		hedit (specout, "crpix1", 1.,
		    add+, addonly-, del-, update+, verify-, show-)
		hedit (specout, "crval1", crval1,
		    add+, addonly-, del-, update+, verify-, show-)
		hedit (specout, "cdelt1", cdelt1,
		    add+, addonly-, del-, update+, verify-, show-)
	    } else if (dtype == "log") {
		hedit (specout, "dc-flag", 1,
		    add+, addonly-, del-, update+, verify-, show-)
		hedit (specout, "crpix1", 1.,
		    add+, addonly-, del-, update+, verify-, show-)
		hedit (specout, "crval1", crval1,
		    add+, addonly-, del-, update+, verify-, show-)
		hedit (specout, "cdelt1", cdelt1,
		    add+, addonly-, del-, update+, verify-, show-)
	    } else if (dtype == "nonlinear") {
		hedit (specout, "refspec1", temp3,
		    add+, addonly-, del-, update+, verify-, show-)
		dispcor (specout, "", linearize=no, database="tmp$",
		    table="", w1=INDEF, w2=INDEF, dw=INDEF, nw=INDEF, log=log,
		    flux=no, samedisp=no, global=no, ignoreaps=no, confirm=no,
		    listonly=no, verbose=no, logfile="")
		hedit (specout, "dclog1",
		    add-, addonly-, del+, update+, verify-, show-)
	    } else if (dtype == "interp") {
		hedit (specout, "refspec1", temp3,
		    add+, addonly-, del-, update+, verify-, show-)
		dispcor (specout, "", linearize=yes, database="tmp$",
		    table="", w1=INDEF, w2=INDEF, dw=INDEF, nw=INDEF, log=log,
		    flux=no, samedisp=no, global=no, ignoreaps=no, confirm=no,
		    listonly=no, verbose=no, logfile="")
		hedit (specout, "dclog1",
		    add-, addonly-, del+, update+, verify-, show-)
	    }
	    if (flux) {
		hedit (specout, "ca-flag", 0,
		    add+, addonly-, del-, update+, verify-, show-)
		hedit (specout, "ex-flag", 0,
		    add+, addonly-, del-, update+, verify-, show-)
	    }
	}
	delete (temp2, verify-)
    }
    fd2=""; delete (spec, verify-)
end
