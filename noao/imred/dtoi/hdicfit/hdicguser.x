# ICG_USER -- User default action

procedure icg_user (ic, gp, gt, cv, wx, wy, wcs, key, cmd)

pointer	ic			# ICFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
pointer	cv			# CURFIT pointer
real	wx, wy			# Cursor positions
int	wcs			# GIO WCS
int	key			# Cursor key
char	cmd[ARB]		# Cursor command

begin
	# Ring bell
	call printf ("\07")
end
