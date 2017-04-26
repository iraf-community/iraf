define	MAX_PS		10		# maximum pseudofiles

# Process table common.

int	pr_pid[MAX_CHILDPROCS]		# process id
int	pr_status[MAX_CHILDPROCS]	# process status
int	pr_inchan[MAX_CHILDPROCS]	# input IPC channel from child
int	pr_infd[MAX_CHILDPROCS]		# fd of input IPC
int	pr_outchan[MAX_CHILDPROCS]	# output IPC channel to child
int	pr_outfd[MAX_CHILDPROCS]	# fd of output IPC
int	pr_nopen[MAX_CHILDPROCS]	# number of open channels
int	pr_pstofd[MAX_CHILDPROCS,MAX_PS]	# pseudofile -> FD
int	pr_last_exit_code		# exit code of last process closed
int	pr_lastio			# index of last active process
int	pr_index			# index of current process
int	pr_oldipc			# old X_IPC handler

common	/prccom/ pr_pid, pr_status, pr_inchan, pr_infd, pr_outchan, pr_outfd,
	pr_nopen, pr_pstofd, pr_lastio, pr_last_exit_code, pr_index, pr_oldipc
