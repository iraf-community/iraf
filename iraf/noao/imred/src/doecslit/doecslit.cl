# DOECSLIT -- Process Echelle slit spectra from 2D to wavelength calibrated
# and flux calibrated 1D spectra.
#
# The task PROC does all of the interactive work and BATCH does the
# background work.  This procedure is organized this way to minimize the
# dictionary space when the background task is submitted.

procedure doecslit (objects)

string	objects = ""		{prompt="List of object spectra"}

file	apref = ""		{prompt="Aperture reference spectrum"}
string	arcs = ""		{prompt="List of arc spectra"}
file	arctable = ""		{prompt="Arc assignment table (optional)"}
string	standards = ""		{prompt="List of standard star spectra\n"}

string	readnoise = "0."	{prompt="Read out noise sigma (photons)"}
string	gain = "1."		{prompt="Photon gain (photons/data number)"}
real	datamax = INDEF		{prompt="Max data value / cosmic ray threshold"}
int	norders = 10		{prompt="Number of orders"}
real	width = 5.		{prompt="Width of profiles (pixels)\n"}

bool	dispcor = yes		{prompt="Dispersion correct spectra?"}
bool	extcor = no		{prompt="Extinction correct spectra?"}
bool	fluxcal = no		{prompt="Flux calibrate spectra?"}
bool	resize = no		{prompt="Resize object apertures?"}
bool	clean = no		{prompt="Detect and replace bad pixels?"}
bool	trace = yes		{prompt="Trace object spectra?"}
string	background = "none"	{prompt="Background to subtract",
			 enum="none|scattered|average|median|minimum|fit"}
bool	splot = no		{prompt="Plot the final spectra?"}
bool	redo = no		{prompt="Redo operations if previously done?"}
bool	update = no		{prompt="Update spectra if cal data changes?"}
bool	quicklook = no		{prompt="Approximate quicklook reductions?"}
bool	batch = no		{prompt="Extract objects in batch?"}
bool	listonly = no		{prompt="List steps but don't process?\n"}

pset	sparams = ""		{prompt="Algorithm parameters"}

begin
	bool	recenter, arcap, tr, scat

	int	i, j
	file	obj, arc, std

	# Expand image lists
	obj = mktemp ("tmp$iraf") 
	arc = mktemp ("tmp$iraf")
	std = mktemp ("tmp$iraf")
	sgetspec (objects, arcs, arctable, standards, obj, arc, std)

	# Remove any leading whitespace from parameters that might be null.
	if (logfile != "") {
	    j = strlen (logfile)
	    for (i=1; i<=j && substr(logfile,i,i)==" "; i+=1);
	    logfile = substr (logfile, i, j)
	}
	if (arctable != "") {
	    j = strlen (arctable)
	    for (i=1; i<=j && substr(arctable,i,i)==" "; i+=1);
	    arctable = substr (arctable, i, j)
	}

	apslitproc.readnoise = readnoise
	apslitproc.gain = gain
	apslitproc.nfind = norders
	apslitproc.width = width
	apslitproc.lower = -width / 2.
	apslitproc.upper = width / 2.
	apslitproc.b_sample = \
	    str(-2*width)//":"//str(-width)//","//str(width)//":"//str(2*width)
	apslitproc.t_width = width
	apslitproc.radius = width
	apslitproc.minsep = width
	apslitproc.clean = clean
	if (background == "scattered") {
	    scat = yes
	    apslitproc.background = "none"
	} else {
	    scat = no
	    apslitproc.background = background
	}
	sproc.datamax = datamax

	recenter = yes
	tr = trace
	arcap = yes
	if (quicklook) {
	    tr = no
	    scat = no
	    arcap = no
	}

	sproc (obj, apref, arc, arctable, std, recenter,
	    resize, quicklook, tr, scat, arcap, dispcor,
	    extcor, fluxcal, splot, redo, update, batch, listonly)
	delete (std, verify=no)

	if (sproc.dobatch) {
	    print ("-- Do remaining spectra as a batch job --")
	    print ("sbatch&batch") | cl
	} else {
	    delete (obj, verify=no)
	    delete (arc, verify=no)
	}
end
