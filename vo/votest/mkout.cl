#{ MKOUT.CL -- Execute the test scripts and generate the output files.

procedure mkout (module)

string	module				{ prompt = "Test module to run"      }
bool	list 		= no		{ prompt = "List available tests"    }
bool	verbose 	= yes		{ prompt = "Verbose output?"         }

struct	*in

begin
	string	mod, vot, tout, base, extn, flist, tname, oname
	bool	listonly, verb
	int	len, ntests, nfail, npass


	# Get params to local variables.
	listonly = list
	verb     = verbose

	ntests	 = 0
	npass	 = 0
	nfail	 = 0

	reset clobber = yes


	# Check the argument list to see if we're running a full test or 
	# only one module.
	if ($nargs == 0)
	    mod      = "all"
	else
	    mod      = module

	if (verb && ! listonly)
	    printf ("Executing '%s' tests.....\n\n", mod)

	# Check to see if we're only listing the tests available for the
	# module.
	if (listonly) {
	    # Get the list of test filenames.
	    vot = mktemp ("tmp$foo")
	    if (mod == "all")
	        type ("tests$/*.men")
	    else
	        type ("tests$/"//mod//"*.men")

	} else {

	    # Create a list of the tests to run.
	    flist = mktemp ("tmp$vti")
	    tout  = mktemp ("tmp$vto")

	    if (mod == "all")
	        files ("tests$*", 			> flist)
	    else
	        files ("tests$" // mod //"_*.cl", 	> flist)

	    # Run each of the tests.
	    in = flist
	    i = 0
	    while (fscan (in, tname) != EOF) {
	        len  = strlen (tname)
	        base = substr (tname, 7, strlen(tname)-3)
	        extn = substr (tname, strlen(tname)-2, len)

	        if (extn == ".cl") {
		    i = i + 1
		    if (verb)
		        printf ("    %-12.12s ....  ", base)
		    ntests = ntests + 1

		    iferr {
			fcache ("init")
			flpr (0)
		        oname =  base // ".out"
		        cl (, < tname, >& oname)

		    } then {
		        if (verb) printf ("Error")
		    }
		    if (verb) printf ("\n")
	        }
	        delete (tout, verify-, >& "dev$null")
	    }
	    delete (flist, verify-) 			# clean up
	}
end
