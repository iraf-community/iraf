# Job table for detached processes.

bool	first_time
int	pr_jobcode[MAX_BKGJOBS]		# job code assigned by host system
int	pr_active[MAX_BKGJOBS]		# set to NO if job is killed
int	pr_exit_status[MAX_BKGJOBS]	# exit status of process
pointer	pr_bkgfile[MAX_BKGJOBS]		# bkgfile filename (signals job term)

common	/prdcom/ first_time, pr_jobcode, pr_active, pr_exit_status, pr_bkgfile
