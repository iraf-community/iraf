#{ TESTS.CL -- Execute or list the regression tests available for the package.

procedure tests (module)

string	module				{ prompt = "Test module to run"      }
bool	list 		= no		{ prompt = "List available tests"    }
bool	verbose 	= no		{ prompt = "Verbose output?"         }

struct	*in

begin
	string	mod, vot, tout, base, extn, flist
	bool	listonly, verb
	int	len, ntests, nfail, npass


	# Get params to local variables.
	listonly = list
	verb     = verbose

	ntests	 = 0
	npass	 = 0
	nfail	 = 0


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
	    while (fscan (in, s1) != EOF) {
	        len  = strlen (s1)
	        base = substr (s1, 7, strlen(s1)-3)
	        extn = substr (s1, strlen(s1)-2, len)

	        if (extn == ".cl") {
		    i = i + 1
		    printf ("%-12.12s ", base)
		    ntests = ntests + 1
		    run_test.pass = no
		    unlearn ("run_test")

	            run_test (mod, base, tout)
	            if (run_test.pass) {
	                print ("[PASS]")
			npass = npass + 1
	            } else {
	                if (run_test.cl_err)
	                    print ("[ERROR]")
			else
	                    print ("[FAIL]")
			if (access (tout) == yes) {
			    if (verb)
	                    	type (tout)
			}
			nfail = nfail + 1
		    }
#	        } else if (extn != ".out") {
#		    print ("=")
#		    match ("#", s1, meta-)
#		    print ("=")
	        }
	        delete (tout, verify-, >& "dev$null")
	    }
	    delete (flist, verify-) 			# clean up

	    printf ("\n\nSummary:  ")
	    printf ("Tests:  %-3d\t", ntests)
	    printf ("Passed: %-3d\t", npass)
	    printf ("Failed: %-3d\n", nfail)
	}
end
