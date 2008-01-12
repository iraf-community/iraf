# Common for the Lroff text formatter.

int	right_margin			# working margins
int	left_margin
int	perm_right_margin		# permanent margins
int	perm_left_margin
int	in_magic_arg			# magic args for in/out procedures
int	out_magic_arg
int	soflag				# if YES, output standout mode chars
int	foflag				# if YES, output forms mode chars
int	justify				# right justify text
int	nls				# .LS nesting level
int	ls_indent			# .LS, def number of spaces to indent
int	sh_nskip			# .SH, def nlines to skip
int	ih_nskip			# .IH, def nlines to skip
int	ih_indent			# .IH, def nspaces to indent
int	nh_nskip			# .NH, def nlines to skip
int	nh_level[MAX_NHLEVEL]		# .NH, section level numbers
bool	standout_mode_enabled		# see input()

common	/lrfcom/ right_margin, left_margin, perm_right_margin,
	perm_left_margin, in_magic_arg, out_magic_arg, soflag,
	foflag, justify, nls, ls_indent, sh_nskip, ih_nskip, ih_indent,
	nh_nskip, nh_level, standout_mode_enabled
