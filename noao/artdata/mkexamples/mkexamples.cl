# MKEXAMPLES -- Make ARTDATA examples.
# The example script files use variable s1 to pass the image name,
# variable s2 to pass the image header file, variable i to pass an object
# seed, variable j to pass a noise seed, and variable b1 to pass the comment
# flag.

procedure mkexamples (name, image)

string	name		{prompt="Example name"}
string	image		{prompt="Image name"}

int	oseed=1		{prompt="Object seed"}
int	nseed=1		{prompt="Noise seed"}
bool	comments=no	{prompt="Add comments to image?"}
bool	verbose=yes	{prompt="Print operation?"}
bool	errors=yes	{prompt="Report errors?"}
bool	list=no		{prompt="List example script file only?"}

begin
	string	name1, name2, example


	# Get and check parameters.
	if ($nargs < 1 && mode != "h") {
	    name1 = "mkexamples"
	    name2 = ""
	    while (name1 != name2) {
		example = "mkexamples$" // name1 // ".men"
		if (!access (example))
		    break
		type (example)
		name2 = name1
		name1 = name
	    }
	    if (name1 == name2)
		return
	} else
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
	    s2 = "artdata$stdheader.dat"
	    i = oseed
	    j = nseed
	    b1 = comments
	    if (s1 == "" || name1 == "")
		return
	    if ((access (s1) || access (s1//"."//envget("imtype")))) {
		if (errors)
		    error (1, "Image " // s1 // " already exists")
		return
	    }

	    if (verbose)
                print ("Creating example ", name1, " in image ", s1, " ...")
	    cl (< example)
	}
end
