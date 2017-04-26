# SCOPY -- Copy spectra

procedure scopy (input, output)

string	input			{prompt="List of input spectra"}
string	output			{prompt="List of output spectra"}

real	w1 = INDEF		{prompt="Starting wavelength"}
real	w2 = INDEF		{prompt="Ending wavelength"}
string	apertures = ""		{prompt="List of apertures or columns/lines"}
string	bands = ""		{prompt="List of bands or lines/bands"}
string	beams = ""		{prompt="List of beams or echelle orders"}
int	apmodulus = 0		{prompt="Input aperture modulus (0=none)\n"}

string	format = "multispec"	{prompt="Output spectra format",
				 enum="multispec|onedspec"}
bool	renumber = no		{prompt="Renumber output apertures?"}
int	offset = 0		{prompt="Output aperture number offset"}
bool	clobber = no		{prompt="Modify existing output images?"}
bool	merge = no		{prompt="Merge with existing output images?"}
bool	rebin = yes		{prompt="Rebin to exact wavelength region?"}
bool	verbose = no		{prompt="Print operations?"}

begin
	sarith (input, "copy", "", output, w1=w1, w2=w2, apertures=apertures,
	    bands=bands, beams=beams, apmodulus=apmodulus, reverse=no,
	    ignoreaps=no, format=format, renumber=renumber, offset=offset,
	    clobber=clobber, merge=merge, rebin=rebin, errval=0.,
	    verbose=verbose)
end
