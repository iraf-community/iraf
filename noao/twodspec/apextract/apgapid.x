# AP_GAPID -- Get an aperture ID from STDIN.

procedure ap_gapid (apid)

int	apid			# Aperture ID
int	scan(), nscan()

begin
	repeat {
	    call printf ("Aperture = ")
	    call flush (STDOUT)
	    if (scan (STDIN) != EOF) {
	        call gargi (apid)
	        if (nscan() == 1)
		    break
	    }
	}
end
