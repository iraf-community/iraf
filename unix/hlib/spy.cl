# SPY -- [MACHDEP] Give info on who is logged in, what they are up to,
# how much resources they have consumed, and so on.  This routine is
# machine dependent.

procedure spy()

begin
	string mach

	if ($nargs > 0) {
	    # "Verbose" mode: show UNIX processor status, filtering
	    # out all the uninteresting system processes.

	    print ("!!uname" | cl() | scan (mach)
	    if (strlwr (mach) == "ssol") {
		!! ps -ef | grep -v root
	    } else if (strlwr (mach) == "SunOS") {
		!! ps -axu | grep -v root
	    } else if (strlwr (mach) == "linux" || strlwr (mach) == "Darwin") {
		!! ps axuf | grep -v root
	    } else {
		!! ps -ef | grep -v root
	    }

	} else {
	    # Merely give info on who is logged in and what they are doing.
	    # The following is for Berkeley UNIX only.

	    !! w
	}
end
