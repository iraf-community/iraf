# SPY -- [MACHDEP] Give info on who is logged in, what they are up to,
# how much resources they have consumed, and so on.  This routine is
# machine dependent.

procedure spy()

begin
	string mach

	if ($nargs > 0) {
	    # "Verbose" mode: show UNIX processor status, filtering
	    # out all the uninteresting system processes.

	    mach = envget ("MACH")
	    if (mach == "ssol" || mach == "sx86") {
		!! ps -ef | grep -v root
	    } else if (mach == "sparc" || mach == "f68881" || mach == "ffpa") {
		!! ps -axu | grep -v root
	    } else if (mach == "linux") {
		!! ps -axuf | grep -v root
	    } else {
		!! ps -ef | grep -v root
	    }

	} else {
	    # Merely give info on who is logged in and what they are doing.
	    # The following is for Berkeley UNIX only.

	    !! w
	}
end
