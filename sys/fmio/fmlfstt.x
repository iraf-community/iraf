# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<mach.h>
include	<fio.h>
include	"fmio.h"

# FM_LFSTATI -- Stat an lfile.

procedure fm_lfstati (lf, param, lvalue)

pointer	lf			#I lfile descriptor
int	param			#I parameter code
long	lvalue			#O parameter value

pointer	fm
int	chan

begin
	fm = LF_FM(lf)
	chan = FM_CHAN(fm)

	# Only the file size differs for each lfile.
	switch (param) {
	case FSTT_FILSIZE:
	    lvalue = LF_FSIZE(lf)
	case FSTT_BLKSIZE:
	    lvalue = FM_SZBPAGE(fm)
	case FSTT_OPTBUFSIZE:
	    lvalue = FM_OPTBUFSIZE(fm)
	case FSTT_MAXBUFSIZE:
	    lvalue = FM_MAXBUFSIZE(fm)
	}

	# For text lfiles, things appear to be SZB_CHAR larger.
	if (and (LF_FLAGS(lf), LFF_TEXTFILE) != 0)
	    lvalue = lvalue * SZB_CHAR
end
