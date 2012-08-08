#{  ERRIF -- Test error types.

procedure errif (type)

int	type			{ prompt = "Error test code: " 	}

begin
	int 	code

	# get local script variable of param
	code = type

	if (code == 1) {			# FPE test
	    fpe ()
	} else if (code == 2) {			# SEGVIO test
	    segvio ()
	} else if (code == 3) {			# SPP error() call test
	    spperr ()
	} else if (code == 4) {			# non-existant task test
	    foo ()
	} else if (code == 5) {			# CL error() command
	    error (code, "cl error() command")
	}
end
