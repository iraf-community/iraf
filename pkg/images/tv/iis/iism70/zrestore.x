# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "../lib/ids.h"
include "iis.h"

# restore device, image, graphics data

procedure zdev_restore(fd)

int	fd			# file descriptor to read from

begin
end

procedure zim_restore(fd, frame)

int	fd
short	frame[ARB]		# frame numbers to restore

begin
end

procedure zgr_restore(fd, plane)

int	fd
short	plane[ARB]

begin
end
