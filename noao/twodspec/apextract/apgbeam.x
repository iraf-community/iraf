# AP_GBEAM -- Get an aperture beam number from STDIN.

procedure ap_gbeam (apbeam)

int	apbeam			# Aperture beam number
int	scan(), nscan()

begin
	repeat {
	    call printf ("Beam = ")
	    call flush (STDOUT)
	    if (scan (STDIN) != EOF) {
	        call gargi (apbeam)
	        if (nscan() == 1)
		    break
	    }
	}
end
