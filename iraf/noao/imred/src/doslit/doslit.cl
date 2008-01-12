# DOSLIT -- Process slit spectra from 2D to wavelength calibrated 1D.
#
# The task SPROC does all of the interactive work and SBATCH does the
# background work.  This procedure is organized this way to minimize the
# dictionary space when the background task is submitted.

procedure doslit (objects)

string	objects = ""		{prompt="List of object spectra"}

string	arcs = ""		{prompt="List of arc spectra"}
file	arctable		{prompt="Arc assignment table (optional)"}
string	standards = ""		{prompt="List of standard star spectra\n"}

string	readnoise = "rdnoise"	{prompt="Read out noise sigma (photons)"}
string	gain = "gain"		{prompt="Photon gain (photons/data number)"}
real	datamax = INDEF		{prompt="Max data value / cosmic ray threshold"}
real	width = 5.		{prompt="Width of profiles (pixels)"}
string	crval = "INDEF"		{prompt="Approximate wavelength"}
string	cdelt = "INDEF"		{prompt="Approximate dispersion\n"}

bool	dispcor = yes		{prompt="Dispersion correct spectra?"}
bool	extcor = no		{prompt="Extinction correct spectra?"}
bool	fluxcal = no		{prompt="Flux calibrate spectra?"}
bool	resize = no		{prompt="Automatically resize apertures?"}
bool	clean = no		{prompt="Detect and replace bad pixels?"}
bool	splot = no		{prompt="Plot the final spectrum?"}
bool	redo = no		{prompt="Redo operations if previously done?"}
bool	update = no		{prompt="Update spectra if cal data changes?"}
bool	quicklook = no		{prompt="Minimally interactive quick-look?"}
bool	batch = no		{prompt="Extract objects in batch?"}
bool	listonly = no		{prompt="List steps but don't process?\n"}

pset	sparams = ""		{prompt="Algorithm parameters"}

begin
	file	obj, arc, std

	# Expand image lists
	obj = mktemp ("tmp$iraf") 
	arc = mktemp ("tmp$iraf")
	std = mktemp ("tmp$iraf")
	sgetspec (objects, arcs, arctable, standards, obj, arc, std)

	apslitproc.readnoise = readnoise
	apslitproc.gain = gain
	apslitproc.width = width
	apslitproc.t_width = width
	apslitproc.radius = width
	apslitproc.clean = clean
	sproc.datamax = datamax

	sproc (obj, arc, arctable, std, crval, cdelt, dispcor, extcor, fluxcal,
	    resize, clean, splot, redo, update, quicklook, batch, listonly)
	delete (std, verify=no)

	if (sproc.dobatch) {
	    print ("-- Do remaining spectra as a batch job --")
	    print ("sbatch&batch") | cl
	} else {
	    delete (obj, verify=no)
	    delete (arc, verify=no)
	}
end
