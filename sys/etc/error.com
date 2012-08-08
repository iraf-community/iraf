
bool	xerflg			# set when error is posted
int	xercod			# error code
int	nhandlers		# handler nesting level
int	err_restart		# YES during error restart, NO otherwise
char	xermsg[SZ_XERMSG]	# error message string
common	/xercom/ xerflg,xercod,nhandlers,err_restart,xermsg
