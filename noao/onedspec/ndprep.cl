# NDPREP -- Generate an ND filter correction image for use over a specified 
# wavelength range from a filter file. The output correction image may
# be 1D or 2D.

procedure ndprep (filter_curve, output)

file	filter_curve		{prompt="Input ND filter curve"}
file	output			{prompt="Output calibration image"}
real	w0			{prompt="Starting wavelength (Angstroms)"}
real	dw			{prompt="Wavelength increment (Angstroms)"}
int	nw			{prompt="Number of wavelength points"}
int	nspace=0		{prompt="Number of spatial points (0 for 1D)"}
bool	logarithm=no		{prompt="Use logarithmic wavelengths?"}
bool	flux=yes		{prompt="Conserve flux when log rebinning?"}
int	dispaxis=2		{prompt="Dispersion axis"}
file	directory="onedstds$ctio/"	{prompt="ND filter directory"}

begin
	file	in, out, temp
	bool	log

	# Page list of filters if '?'.
	in = filter_curve
	if (in == "?") {
	    page (directory // "ndfilters.men")
	    in = filter_curve
	    if (in == "?")
		return
	}

	# Check if filter curve exists.
	in = directory // in
	if (!access (in))
	    error (0, "Filter curve "// in // " not found")

	# Convert the filter curve to a 1D image.
	out = output
	sinterp (in, "", out, w0, dx=dw, npts=nw, make_image=yes,
	    interp_mode="curve")
	hedit (out, "dc-flag", 0, add=yes, show=no, verify=no)

	# Convert to log if desired.
	if (logarithm == yes) {
	    temp = mktemp ("tmp")
	    dispcor (out, temp, linearize=yes, table="", w1=INDEF,
		w2=INDEF, dw=INDEF, nw=INDEF, log=yes, flux=flux,
		confirm=no, listonly=no, verbose=no, logfile="")
	    imdelete (out, verify=no)
	    imrename (temp, out, verbose=no)
	}

	# Convert to a 2D image if the number of spacial points is > 0.
	if (nspace > 0) {
	    temp = mktemp ("tmp")
	    imstack (out, temp)
	    imdelete (out, verify=no)
	    imrename (temp, out, verbose=no)
	    if (dispaxis == 1) {
		blkrep (out, out, 1, nspace)
	    } else {
		imtranspose (out, out)
		blkrep (out, out, nspace, 1)
	    }
	}
end
