# MKEXAMPLES -- Make ARTDATA examples.
# The example script files use parameter s1 to pass the image name.

procedure mkexamples (name, image)

string	name		{prompt="Example name"}
string	image		{prompt="Image name"}

bool	verbose=yes	{prompt="Print operation?"}
bool	errors=yes	{prompt="Report errors?"}
bool	list=no		{prompt="List example script file only?"}

begin
	string	name1, example


	# Get and check parameters.
	if ($nargs < 1 && mode != "h")
	    type ("mkexamples$mkexamples.men")
	name1 = name

	example = "mkexamples$" // name1 // ".cl"
	if (!access (example)) {
	    if (errors)
	        error (2, "Unknown example " // name1) 
	    return
	}

	# Make or list the example.
	if (list)
	    page (example)
	else {
	    s1 = image 
	    if (s1 == "" || name1 == "")
		return
	    if ((access (s1) || access (s1//".imh") || access (s1 //".hhh"))) {
		if (errors)
		    error (1, "Image " // s1 // " already exists")
		return
	    }

	    if (verbose)
                print ("Creating example ", name1, " in image ", s1, " ...")
	    cl (< example)
	}
end
