# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_INLINE_KERNEL -- Identify a graphics stream for use with an inline
# kernel, i.e., with a kernel linked into the same process as the high level
# code which calls the GKI procedures.  At present there may be at most one
# inline kernel at a time.  The entry point addresses of the kernel procedures
# are passed in the array DD.  Subsequent GKI calls for the named stream will
# result in direct calls to the inline kernel without encoding and decoding
# GKI instructions, hence this is the most efficient mode of operation.

procedure gki_inline_kernel (stream, dd)

int	stream			# graphics stream to be redirected
int	dd[ARB]			# device driver for the kernel
include	"gki.com"

begin
	gk_type[stream] = TY_INLINE
	call amovi (dd, gk_dd, LEN_GKIDD)
end
