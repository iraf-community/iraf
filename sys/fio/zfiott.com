# ZFIOTT.COM -- State variables for the VOS terminal driver.

int	tty_kinchan			# kernel input channel of terminal
int	tty_koutchan			# kernel output channel of terminal
int	tty_inlogchan			# input spoolfile
int	tty_outlogchan			# output spoolfile
int	tty_pbinchan			# playback spoolfile
int	tty_delay			# playback delay/rec, msec
int	tty_ip				# pointer into tty_inbuf
int	tty_filter			# EPA of filter callback
int	tty_filter_key			# character key which triggers filter
bool	tty_ucasein			# map upper case input to lower case
bool	tty_ucaseout			# map output to upper case
bool	tty_shiftlock			# software shiftlock for ucasein mode
bool	tty_rawmode			# in raw terminal mode
bool	tty_logio			# logio logging in effect
bool	tty_login			# input logging in effect
bool	tty_logout			# output logging in effect
bool	tty_playback			# playback mode (cmd input from file)
bool	tty_verify			# pause when newline seen in input
bool	tty_passthru			# passthru mode (direct i/o to device)
char	tty_iofile[SZ_FNAME]		# name of logio spoolfile
char	tty_infile[SZ_FNAME]		# name of login spoolfile
char	tty_outfile[SZ_FNAME]		# name of logout spoolfile
char	tty_pbfile[SZ_FNAME]		# name of playback spoolfile
char	tty_tdevice[SZ_DEVNAME]		# terminal device at record time
char	tty_gdevice[SZ_DEVNAME]		# stdgraph device at record time
char	tty_inbuf[SZ_LINE]		# input line data buffer

common	/zttcom/ tty_kinchan, tty_koutchan, tty_inlogchan, tty_outlogchan,
	tty_pbinchan, tty_delay, tty_ip, tty_filter, tty_filter_key,
	tty_ucasein, tty_ucaseout, tty_shiftlock, tty_rawmode, tty_logio,
	tty_login, tty_logout, tty_playback, tty_verify, tty_passthru,
	tty_iofile, tty_infile, tty_outfile, tty_pbfile, tty_tdevice,
	tty_gdevice, tty_inbuf
