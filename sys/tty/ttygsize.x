# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ttyset.h>
include	<ttset.h>
include	<fset.h>


# TTYGSIZE -- Get the width and height of the terminal screen in characters.
# For a conventional terminal these values are given as constants in the
# termcap entry for the device.  If the terminal has a screen which can vary
# in size at runtime (e.g., a window on a workstation), then the screen size
# is queried at runtime.

procedure ttygsize (in, out, tty, width, height)

int	in, out			# input and output streams
pointer	tty			# terminal descriptor
int	width			# ncols on screen (out)
int	height			# nlines on screen (out)

int	ttystati(), ttstati()

begin
	width = 0
	height = 0

	# Retrieve actual values when not in stty playback or record mode.
	if (ttstati(in,TT_LOGIN) != YES && ttstati(in,TT_PLAYBACK) != YES)
	    call zttysz (out, width, height)

	# If we still do not know the screen width or height, use the values
	# given in the user environment, else in the termcap entry for the
	# device.

	if (width <= 0)
	    width = ttystati (tty, TTY_NCOLS)
	if (height <= 0)
	    height = ttystati (tty, TTY_NLINES)

end
