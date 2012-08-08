# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "../lib/ids.h"
include "iis.h"

# save device, image, graphics data

procedure zdev_save(fd)

int	fd			# file descriptor to write to

begin
end

procedure zim_save(fd, frame)

int	fd
short	frame[ARB]		# frame numbers to save

begin
end

procedure zgr_save(fd, plane)

int	fd
short	plane[ARB]

begin
end
