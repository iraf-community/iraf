# ING_UFIT -- User default action for interactive fitting commands

procedure ing_ufit (in, gp, gt, nl, wx, wy, wcs, key, cmd)

pointer	in			# INLFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
pointer	nl			# NLFIT pointer
real	wx, wy			# Cursor positions
int	wcs			# GIO WCS
int	key			# Cursor key
char	cmd[ARB]		# Cursor command

begin
	# Ring bell
	call printf ("\07\n")
end
