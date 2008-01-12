# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fmset.h>
include	"fmio.h"

# FM_LFNAME -- Encode the pseudo-filename for an lfile.  This is necessary to
# pass all lfile info through FIO, when opening a file descriptor on an lfile.
#
# The filename syntax is "Tddd.fff" where
#
#       T       is 'B' or 'T' for text or binary
#       ddd     is the encoded descriptor pointer
#       fff     is the encoded lfile number

procedure fm_lfname (fm, lfile, type, lfname, maxch)

pointer	fm			#I FMIO descriptor
int	lfile			#I lfile number
int	type			#I file type, text or binary
char	lfname[maxch]		#O encoded lfile filename
int	maxch			#I max chars out

int	op
int	itoc()
errchk	fmio_bind, fmio_errchk

begin
	call fmio_bind (fm)
	call fmio_errchk (fm)
	if (maxch <= 0)
	    return

	op = 1
	if (type == TEXT_FILE)
	    lfname[op] = 'T'
	else
	    lfname[op] = 'B'
	op = min (maxch, op + 1)

	op = min (maxch, op + itoc (fm, lfname[op], maxch-op+1))
	lfname[op] = '.'
	op = min (maxch, op + 1)
	op = min (maxch, op + itoc (lfile, lfname[op], maxch-op+1))
	lfname[op] = EOS
end
