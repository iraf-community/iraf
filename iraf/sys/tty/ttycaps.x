# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"tty.h"

# TTYCAPS -- Return a pointer to the caplist field of an open TTY descriptor.

pointer procedure ttycaps (tty)

pointer	tty			# tty descriptor

begin
	return (P2C (tty + T_OFFCAP))
end
