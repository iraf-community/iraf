#{ MKBATCH -- Make a batch script.

{
	# Make a copy of the script file query parameter and delete the
	# file if it exists and not appending.

	scrpt = script
	if (access (scrpt) && !append)
	     delete (scrpt, verify=verify)

	# Outer loop provides return when the user wants to start over.

	while (YES) {

	    # Add commands until the user is done.

	    while (YES) {

		# Determine the task name and check that it is loaded.

		ltask = task
		if (!deftask (ltask)) {
		    print ("Task ", ltask, " not loaded.")
		    next
		}

		# Edit the task parameters.

	        eparam (ltask)
		print ("")

		# If the user is verifying then display the command.

	        if (verify) {
	            lparam (ltask) | cmdstr (ltask, hidden=hidden)
	            cok = YES
		    if (cok) {
		        lparam (ltask) | cmdstr (ltask, hidden=hidden, >> scrpt)
			print ("", >> scrpt)
		    }
	        } else {
		    lparam (ltask) | cmdstr (ltask, hidden=hidden, >> scrpt)
		    print ("", >> scrpt)
		}

		# Ask if more commands are to be added to the script.

		more = YES
	        if (!more)
		    break
	    }

	    # If verifying page the script and let the user decide to start
	    # over.

	    if (verify) {
		page (scrpt)
		sok = YES
		if (sok)
		    break
		else
		    delete (scrpt, verify=verify)
	    } else
		break
	}

	# If the user does not want to submit the script then quit.

	if (!submit)
    	    bye
}

# Submit the script as a background process.

print ("Script ", scrpt, " submitted at:")
time
print ("\nScript ", scrpt, " submitted at:", >> logfile)
time (>> logfile)
cl (< scrpt, >>& logfile) &
