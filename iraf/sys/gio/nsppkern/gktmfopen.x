# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<mach.h>
include	<fset.h>
include	"gkt.h"

define	SZ_DDSTR	256


# GKT_MFOPEN -- Open the NSPP metacode output file.  The device is connected
# to FIO as a binary file.  Metacode output to the device will be spooled
# and then disposed of to the device when the file descriptor we return is
# later closed.

int procedure gkt_mfopen (tty, mode)

pointer	tty			# pointer to graphcap entry for device
int	mode			# access mode

int	fd
pointer	sp, ddstr
int	fopnbf(), ttygets()
extern	zopnpl(), zardpl(), zawrpl(), zawtpl(), zsttpl(), zclspl()
errchk	fopnbf

begin
	call smark (sp)
	call salloc (ddstr, SZ_DDSTR, TY_CHAR)

	# The DD string is used to pass device dependent information to the
	# NSPP graphics device driver.

	if (ttygets (tty, "DD", Memc[ddstr], SZ_DDSTR) <= 0)
	    call error (1, "nsppkern: missing DD parameter in graphcap")

	fd = fopnbf (Memc[ddstr], mode,
	    zopnpl, zardpl, zawrpl, zawtpl, zsttpl, zclspl)

	# Set the FIO buffer size to the size of a metafile record.
	call fseti (fd, F_BUFSIZE, SZ_MFRECORD)

	call sfree (sp)
	return (fd)
end
