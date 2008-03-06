# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<mach.h>
include	<fio.h>
include	"fmio.h"

# FM_LFSTAT[IL] -- Stat an lfile.

procedure fm_lfstatl (lf_chan, param, lvalue)

int	lf_chan			#I lfile descriptor
int	param			#I parameter code
long	lvalue			#O parameter value

pointer	lf, fm
int	chan

include "fmio.com"

begin
	lf = Memp[lf_ptrs+lf_chan]
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


procedure fm_lfstati (lf_chan, param, value)

int	lf_chan			#I lfile descriptor
int	param			#I parameter code
int	value			#O parameter value

long	lvalue

begin
	call fm_lfstatl (lf_chan,param,lvalue)
	value = lvalue
end
