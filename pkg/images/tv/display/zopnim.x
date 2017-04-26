# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZOPNIM -- Open an image display frame which is addressable as
# a binary file.

procedure zopnim (devinfo, mode, chan)

char	devinfo[ARB]		# packed devinfo string
int	mode			# access mode
int	chan

int	iischan[2]		# Kludge

begin
	call iisopn (devinfo, mode, iischan)
	chan = iischan[1]
end
