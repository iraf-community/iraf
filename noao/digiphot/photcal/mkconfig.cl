# MKCONFIG -- Create / edit a PHOTCAL configuration file.

procedure mkconfig (config, catalog, observations, transform)

file	config       {prompt="The new configuration file"}
file	catalog	     {"STDIN",
	prompt="The source of the catalog format specification"}
file	observations {"STDIN",
	prompt="The source of the observations file format specification"}
file	transform    {"STDIN",
	prompt="The source of the transformation equations"}
file	template     {"", prompt="An existing template configuration file"}
string	catdir	     {")_.catdir",prompt="The standard star catalog directory"}
bool	verify	     {no, prompt="Verify each new entry"}
bool	edit	     {yes,prompt="Edit the new configuration file"}
bool	check	     {yes,prompt="Check the configuration file"}
bool	verbose	     {no, prompt="Verbose output"}


begin
	# Declare local variables

	file	con, temp, tcat, cat, tobs, obs, ttrans, trans
	string	cdir

	string junk

	# Get the output and template files.

	con = config
	if (access (con))
	    error (0, "The configuration file " // con // " already exists")
	temp = template

	# Define the source of the catalog, observation and transformation
	# sections.

	if (access (temp)) {

	    cat = ""
	    obs = ""
	    trans = ""

	} else {

	    cdir = catdir

	    tcat = catalog
	    cat = "f" // tcat // ".dat"
	    if (! access (cat)) {
		cat = cdir // cat
	        if (! access (cat)) {
		    cat = tcat
		    if (! access (cat))
		        error (0,
			    "Cannot find " // tcat //
			    " in current or standards directory")
		}
	    }

	    tobs = observations
	    obs = "f" // tobs // ".dat"
	    if (! access (obs)) {
		obs = tobs
		if (! access (obs))
		    error (0,
		        "Cannot find " // tobs // " in current directory")
	    }

	    ttrans=transform
	    trans = "t" // ttrans // ".dat"
	    if (! access (trans)) {
		trans = cdir // trans
		if (! access (trans)) {
		    trans = ttrans
	            if (! access (trans))
		        error (0,
	       	        "Cannot find " // ttrans //
			" in current or standards directory")
	       }
	    }
	}

	# Make a new configuration file.

	config (con, catalog=cat, observations=obs, transform=trans,
	    template=temp, verify=verify)

	# Edit the catalog with the editor.

	print ("")
	if (edit)
	    edit (con)

	# Check the configuration file for semantic and syntax errors.
	print ("")
	if (check)
	    chkconfig (con, verbose=verbose)
end
