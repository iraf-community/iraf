# DOFIBERS -- Process Coude fiber spectra from 2D to wavelength calibrated 1D.
#
# The task DOFIBERS1 does all of the interactive work and DOFIBERS2 does the
# background work.  This procedure is organized this way to minimize the
# dictionary space when the background task is submitted.

procedure dofibers (objects)

string	objects = ""		{prompt="List of object spectra"}

file	apref = ""		{prompt="Aperture reference spectrum"}
file	flat = ""		{prompt="Flat field spectrum"}
string	arcs = ""		{prompt="List of arc spectra"}
file	arctable = ""		{prompt="Arc assignment table (optional)\n"}

string	readnoise = "RDNOISE"	{prompt="Read out noise sigma (photons)"}
string	gain = "GAIN"		{prompt="Photon gain (photons/data number)"}
int	dispaxis = ")_.dispaxis" {prompt="Dispersion axis (1=along lines, 2=along columns)"}
int	fibers = 3		{prompt="Number of fibers"}
real	width = 6.		{prompt="Width of profiles (pixels)"}
string	objaps = "2"		{prompt="Object apertures"}
string	arcaps = "1,3"		{prompt="Arc apertures\n"}

bool	fitflat = yes		{prompt="Fit and ratio flat field spectrum?"}
bool	recenter = yes		{prompt="Recenter object apertures?"}
bool	edit = no		{prompt="Edit/review object apertures?"}
bool	clean = no		{prompt="Detect and replace bad pixels?"}
bool	dispcor = yes		{prompt="Dispersion correct spectra?"}
bool	splot = yes		{prompt="Plot the final spectrum?"}
bool	redo = no		{prompt="Redo operations if previously done?"}
bool	update = yes		{prompt="Update spectra if cal data changes?"}
bool	batch = no		{prompt="Extract objects in batch?"}
bool	listonly = no		{prompt="List steps but don't process?\n"}

pset	params = ""		{prompt="Algorithm parameters"}

begin
	apscript.readnoise = readnoise
	apscript.gain = gain
	apscript.dispaxis = dispaxis
	apscript.nfind = fibers
	apscript.width = width
	apscript.t_width = width
	apscript.radius = width
	apscript.clean = clean
	apscript.order = "increasing"

	proc (objects, apref, flat, "", arcs, "", "",
	    arctable, fibers, "", objaps, "", arcaps, "",
	    "", "", fitflat, recenter, edit, no, no, clean, dispcor, no,
	    no, no, no, splot, redo, update, batch, listonly)

	if (proc.dobatch) {
	    print ("-- Do remaining spectra as a batch job --")
	    print ("batch&batch") | cl
	}
end
