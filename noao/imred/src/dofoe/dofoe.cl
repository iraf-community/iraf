# DOFOE -- Process FOE spectra from 2D to wavelength calibrated 1D.
#
# The task PROC does all of the interactive work and BATCH does the
# background work.  This procedure is organized this way to minimize the
# dictionary space when the background task is submitted.

procedure dofoe (objects)

string	objects = ""		{prompt="List of object spectra"}

file	apref = ""		{prompt="Aperture reference spectrum"}
file	flat = ""		{prompt="Flat field spectrum"}
string	arcs = ""		{prompt="List of arc spectra"}
file	arctable = ""		{prompt="Arc assignment table (optional)\n"}

string	readnoise = "0."	{prompt="Read out noise sigma (photons)"}
string	gain = "1."		{prompt="Photon gain (photons/data number)"}
real	datamax = INDEF		{prompt="Max data value / cosmic ray threshold"}
int	norders = 12		{prompt="Number of orders"}
real	width = 4.		{prompt="Width of profiles (pixels)"}
string	arcaps = "2x2"		{prompt="Arc apertures\n"}

bool	fitflat = yes		{prompt="Fit and ratio flat field spectrum?"}
string	background = "none"	{prompt="Background to subtract",
			 enum="none|scattered|average|median|minimum|fit"}
bool	clean = no		{prompt="Detect and replace bad pixels?"}
bool	dispcor = yes		{prompt="Dispersion correct spectra?"}
bool	redo = no		{prompt="Redo operations if previously done?"}
bool	update = no		{prompt="Update spectra if cal data changes?"}
bool	batch = no		{prompt="Extract objects in batch?"}
bool	listonly = no		{prompt="List steps but don't process?\n"}

pset	params = ""		{prompt="Algorithm parameters"}

begin
	int	i, j
	bool	scattered

	# Remove any leading whitespace from parameters that might be null.
	if (logfile != "") {
	    j = strlen (logfile)
	    for (i=1; i<=j && substr(logfile,i,i)==" "; i+=1);
	    logfile = substr (logfile, i, j)
	}
	if (flat != "") {
	    j = strlen (flat)
	    for (i=1; i<=j && substr(flat,i,i)==" "; i+=1);
	    flat = substr (flat, i, j)
	}
	if (arctable != "") {
	    j = strlen (arctable)
	    for (i=1; i<=j && substr(arctable,i,i)==" "; i+=1);
	    arctable = substr (arctable, i, j)
	}
	if (arcaps != "") {
	    j = strlen (arcaps)
	    for (i=1; i<=j && substr(arcaps,i,i)==" "; i+=1);
	    arcaps = substr (arcaps, i, j)
	}

	apscript.readnoise = readnoise
	apscript.gain = gain
	if (arcaps != "")
	    i = 2 * norders
	else
	    i = norders
	apscript.nfind = i
	apscript.width = width
	apscript.t_width = width
	apscript.radius = width
	apscript.clean = clean
	if (background == "scattered") {
	    scattered = yes
	    apscript.background = "none"
	} else {
	    scattered = no
	    apscript.background = background
	}
	proc.datamax = datamax

	proc (objects, apref, flat, arcs, arctable, i, "", arcaps,
	    "", "", fitflat, yes, scattered, no, no, no, clean, dispcor,
	    no, redo, update, batch, listonly)

	if (proc.dobatch) {
	    print ("-- Do remaining spectra as a batch job --")
	    print ("batch&batch") | cl
	}
end
