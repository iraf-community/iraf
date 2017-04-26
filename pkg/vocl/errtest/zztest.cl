#{  ZZTEST -- Test various iferr constructs.

procedure zztest ()

begin
	int	nerrs

	onerror ("flpr")

	printf ("Testing iferr....\n")
	nerrs = 0

	for (i=1; i <= 5; i=i+1) {
	    iferr { fpe () } then {
	        print ("    error from test #"//i)
		nerrs = nerrs + 1
	    } else {
	        print ("    NO error from test #"//i)
	    }
	}

	if (nerrs > 0) 
	    error (999, "errors found in script")
end
