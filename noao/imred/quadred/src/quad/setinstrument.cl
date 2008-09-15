# SETINSTRUMENT -- Set up instrument parameters for the CCD reduction tasks.
#
# This task sets default parameters based on an instrument ID.

procedure setinstrument (instrument)

char	instrument		{prompt="Instrument ID (type ? for a list)"}
char	site="ctio"		{prompt="Site ID"}
char	directory="ccddb$"	{prompt="Instrument directory"}
bool	review=yes		{prompt="Review instrument parameters?"}
char	query			{prompt="Instrument ID (type q to quit)",
				 mode="q"}

begin
	string	inst, instdir, instmen, instfile

	# Define instrument directory, menu, and file
	instdir = directory
	if (site != "")
	    instdir = instdir // site // "/"
	instmen = instdir // "instruments.men"
	inst = instrument
	instfile = instdir // inst // ".dat"

	# Loop until a valid instrument file is given.
	while (inst != "" && !access (instfile)) {
	    if (access (instmen))
		page (instmen)
	    else if (inst == "?")
		print ("Instrument list ", instmen, " not found")
	    else
	        print ("Instrument file ", instfile, " not found")
	    print ("")
	    inst = query
	    if (inst == "q")
		return
	    instrument = inst
	    instfile = instdir // inst // ".dat"
	}

	# Set instrument parameter.
	if (access (instfile))
	    quadred.instrument = instfile
	else
	    quadred.instrument = ""

	# Run instrument setup script.
	instfile = instdir // inst // ".cl"
	if (access (instfile))
	    cl (< instfile)

	# Review parameters if desired.
	if (review) {
	    eparam ("quadred")
	    eparam ("qccdproc")
	    eparam ("quadproc")
	}
end
