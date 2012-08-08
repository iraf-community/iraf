# WSPECTEXT -- Write a 1D image spectrum as an ascii text file.
# This simply uses WTEXTIMAGE to write the header is selected and
# formats the wavelength/flux data using LISTPIX.

procedure wspectext (input, output)

string	input			{prompt="Input list of image spectra"}
string	output			{prompt="Output list of text spectra"}
bool	header = yes		{prompt="Include header?"}
string	wformat = ""		{prompt="Wavelength format"}

begin
    int		ndim
    string	specin, specout, spec

    specin = mktemp ("tmp$iraf")
    specout = mktemp ("tmp$iraf")
    spec = mktemp ("tmp$iraf")

    # Expand the input and output image templates and include naxis.
    hselect (input, "$I,naxis", yes, > specin)
    sections (output, option="fullname", > specout)
    join (specin, specout, output=spec, delim=" ", shortest=yes, verbose=yes)
    delete (specin, verify=no)
    delete (specout, verify=no)

    # For each input spectrum check the dimensionality.  Extract the header
    # with WTEXTIMAGE if desired and then use LISTPIX to extract the
    # wavelengths and fluxes.

    list = spec
    while (fscan (list, specin, ndim, specout) != EOF) {
	if (ndim != 1) {
	    print ("WARNING: "//specin//" is not one dimensional")
	    next
	}
	if (header) {
	    wtextimage (specin, specout, header=yes, pixels=no, format="",
		maxlinelen=80)
	    listpixels (specin, wcs="world", formats=wformat, verbose=no,
		>> specout)
	} else
	    listpixels (specin, wcs="world", formats=wformat, verbose=no,
		> specout)
    }
    list=""; delete (spec, verify=no)
end
