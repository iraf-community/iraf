# SEXTRACT -- Extract the specified pixels of a 1D, dispersion corrected
# spectrum.  This requires W0 and WPC in the header.  The limits may
# be specified either in wavelength or pixels.  When specified in wavelength
# the nearest pixel to that wavelength is used.

procedure sextract (input, records, output, wstart, wend)

string	input		{prompt="Input spectrum root name"}
string	records		{prompt="Input records"}	
string	output		{prompt="Output spectrum root name"}
real	wstart		{prompt="Starting wavelength or pixel"}
real	wend		{prompt="Ending wavelength or pixel"}
int	startrec	{prompt="Starting output record number", min=0}
bool	wlimits=yes	{prompt="Are limits in wavelength?"}

struct	*list

begin
	file	in, out, temp1, temp2
	int	p1, p2, naxis1, crpix1, rec
	real	w1, w2, w0, wpc, crval1, cdelt1
	string	section

	# Get parameters from the image header.
	temp1 = mktemp ("list")
	temp2 = mktemp ("list")
	names (input, records, check=no, > temp1)
	hselect ("@"//temp1, "$I,naxis1,w0,wpc", yes, > temp2)
	delete (temp1, verify=no)

	# Get other parameters.
	temp1 = output
	w1 = wstart
	w2 = wend
	rec = startrec

	# Do each input spectrum.
	list = temp2
	while (fscan (list, in, naxis1, w0, wpc) != EOF) {
	    if (nscan() < 4) {
		print ("Missing header parameters: ", in)
		next
	    }

	    # Determine the pixel limits.
	    if (wlimits) {
	        p1 = (w1 - w0) / wpc + 1.5
	        p2 = (w2 - w0) / wpc + 1.5
	    } else {
	        p1 = wstart + 0.5
	        p2 = wend + 0.5
	    }
	    if ((p1<1) || (p1>naxis1) || (p2<1) || (p2>naxis1)) {
	        print ("Wavelengths are out of bounds: ", in)
		next
	    }

	    # Extract the spectrum
	    if (rec < 10)
	        out = temp1 // ".000" // rec
	    else if (rec < 100)
	        out = temp1 // ".00" // rec
	    else if (rec < 1000)
	        out = temp1 // ".0" // rec
	    else
	        out = temp1 // "." // rec
	    rec = rec + 1
	    section = "["//p1//":"//p2//"]"
	    imcopy (in//section, out, verbose=yes)

	    # Recalculate the coordinates.
	    w0 = w0 + (p1 - 1) * wpc
	    if (p1>p2)
	        wpc = -wpc
	    crval1 = w0
	    cdelt1 = wpc
	    crpix1 = 1

	    # Update the header.
	    hedit (out, "w0", w0, add=yes, update=yes, show=no, verify=no)
	    hedit (out, "wpc", wpc, add=yes, update=yes, show=no, verify=no)
	    hedit (out, "crval1", crval1, add=yes, update=yes, show=no,
		verify=no)
	    hedit (out, "cdelt1", cdelt1, add=yes, update=yes, show=no,
		verify=no)
	    hedit (out, "crpix1", crpix1, add=yes, update=yes, show=no,
		verify=no)
        }

	delete (temp2, verify=no)
end
