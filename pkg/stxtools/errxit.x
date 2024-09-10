# ERRXIT -- Take an error exit and set the error code

# This error exit routine is used on VMS system. The VMS symbol $status
# will be set to the exit_code, so that the process running this program
# will know the error condition that terminated the program. In order
# to avoid conflict with other VMS exit codes, it would be best if the
# exit_code is set to an odd value greater than one.
#
# Nelson Zarate		30-Nov-95	original
# Perry Greenfield      18-Apr-95	change exit code from 122 to 2
#					so that misleading DCL error message
#					is not given (severity level remains
#					the same: 2 --> ERROR). 122 results
#					in a "DEVICE NOT MOUNTED" message.

procedure errxit (exit_code)

int	exit_code
#--

begin
        # Reset the exit code to a constant value for this routine that 
	# will be called on a VMS system. Other system will run the
	# errxit.c that lives in tables$lib/stxtools/errxit.c to be inserted
	# at compilation time by mkpkg.sf..
	# NZ Nov 30 1995
	exit_code = 2

	call exit (exit_code)
end
