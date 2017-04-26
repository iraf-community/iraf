include <clset.h>

# POST_EXIT_HANDLER -- Post an error handler that exits with an error code

# The standard behavior of an iraf program is to exit with an error code
# of zero regardless of whether program execution is halted with an error.
# This behavior complicates control of tasks by user scripts. To change this
# behavor, this procedure posts an error handler that exits the program with
# its error code set to the value passed to the error procedure. The exit
# procedure is only called of the program terminates with a non-zero error
# status and the program is being run at the host level. The latter 
# restriction is in place because exiting a program running under the iraf
# command language (cl) hangs the command language. Since error handlers
# a run in the order that they are posted, this procedure should be called
# after any other error handlers you may have in your program.
#
# Nelson Zarate		30-Nov-95	original

procedure post_exit_handler ()

#--
extern 	exit_handler()
int 	clstati()

begin
	# Only post the exit handler if the task is being run in host mode

	if (clstati(CL_PRTYPE) == PR_HOST)
	    call onerror(exit_handler)
end

# EXIT_HANDLER -- Error handler that exits the program, setting the error code

procedure exit_handler (status)

int	status		# i: program exit status
#--

begin
	# Only take exit if error status is non-zero (not OK)

	if (status != OK) {
	    # Must clean up file i/o first
	    # The OK flag flushes the buffers

	    call fio_cleanup (OK)

	    # Take the error exit with the specified status

	    call errxit (status)
	}
end
