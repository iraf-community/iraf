procedure rf_exit (exit_code)
int	exit_code
begin
	# Reset the exit code to a constant value for this routine that 
	# will be called on a VMS system. Other system will run the
	# rf_exit.c that lives in tables$base/rf_exit.c to be inserted
	# at compilation time by mkpkg.sf..
	# NZ Nov 30 1995
	exit_code =123
	call exit (exit_code)
end
