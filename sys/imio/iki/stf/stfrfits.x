# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<imhdr.h>
include	<imio.h>
include	"stf.h"

# STF_RFITSHDR -- Process the FITS header cards into the STF descriptor and the
# spool file.  The following reserved keywords are recognized:
#
#	SIMPLE BITPIX DATATYPE NAXIS* GROUPS GCOUNT PCOUNT PSIZE 
#	PTYPE* PDTYPE* PSIZE* 
#
# All unrecognized cards, including HISTORY and COMMENT cards, blank lines,
# and any other garbage in the header, are preserved in the user area of the
# IMIO descriptor (i.e., in the spoolfile).  Certain of the standard reserved
# cards (GROUPS, GCOUNT, etc.) are saved in the IMIO user area for the sake
# of the user, although the real values of these parameters are maintained only
# in the STF descriptor.

procedure stf_rfitshdr (im, spool)

pointer	im		# image descriptor
int	spool		# spool file for user (nonreserved) header cards

char	ch
int	in, ip, ival, nchars
pointer	sp, pp, stf, lbuf, op
int	getline(), strncmp(), stridx(), ctoi()

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	stf = IM_KDES(im)
	in  = IM_HFD(im)

	repeat {
	    # Get the next input line.
	    nchars = getline (in, Memc[lbuf])
	    if (nchars == EOF)
		break

	    # Block it out to 80 chars (plus newline) if it is not already.
	    if (nchars != FITS_RECLEN + 1) {
		for (op=nchars;  op <= FITS_RECLEN;  op=op+1)
		    Memc[lbuf+op-1] = ' '
		Memc[lbuf+FITS_RECLEN]   = '\n'
		Memc[lbuf+FITS_RECLEN+1] = EOS
	    }
		
	    # Process the header card.
	    if (stridx (Memc[lbuf], "BDGNPS") > 0) {
		ch = Memc[lbuf]
		if (       strncmp (Memc[lbuf], "SIMPLE ", 7) == 0) {
		    ;
		} else if (strncmp (Memc[lbuf], "BITPIX ", 7) == 0) {
		    call stf_geti (Memc[lbuf], STF_BITPIX(stf))
		} else if (strncmp (Memc[lbuf], "DATATYPE", 8) == 0) {
		    call stf_gets (Memc[lbuf], STF_DATATYPE(stf), SZ_DATATYPE)

		} else if (strncmp (Memc[lbuf], "NAXIS", 5) == 0) {
		    ch = Memc[lbuf+5]
		    if (ch == ' ') {
			call stf_geti (Memc[lbuf], STF_NAXIS(stf))
		    } else if (IS_DIGIT (ch)) {
			ival = TO_INTEG(ch)
			call stf_geti (Memc[lbuf], STF_LENAXIS(stf,ival))
		    } else
			call putline (spool, Memc[lbuf])

		} else if (strncmp (Memc[lbuf], "GROUPS ", 7) == 0) {
		    call stf_getb (Memc[lbuf], STF_GROUPS(stf))
		    call imaddb (im, "GROUPS", STF_GROUPS(stf) == YES)

		} else if (strncmp (Memc[lbuf], "PSIZE", 5) == 0) {
		    if (Memc[lbuf+5] == ' ') {
			call stf_geti (Memc[lbuf], STF_PSIZE(stf))
			call imaddi (im, "PSIZE", STF_PSIZE(stf))
		    } else if (IS_DIGIT (Memc[lbuf+5])) {
			ip = 6; ip = ctoi (Memc[lbuf], ip, ival)
			pp = STF_PDES(stf,min(ival,MAX_PCOUNT))
			call stf_geti (Memc[lbuf], P_PSIZE(pp))
		    } else
			call putline (spool, Memc[lbuf])

		} else if (strncmp (Memc[lbuf], "GCOUNT ", 7) == 0) {
		    call stf_geti (Memc[lbuf], STF_GCOUNT(stf))
		    call imaddi (im, "GCOUNT", STF_GCOUNT(stf))
		} else if (strncmp (Memc[lbuf], "PCOUNT ", 7) == 0) {
		    call stf_geti (Memc[lbuf], STF_PCOUNT(stf))
		    call imaddi (im, "PCOUNT", STF_PCOUNT(stf))

		} else if (strncmp (Memc[lbuf], "PTYPE", 5) == 0) {
		    if (IS_DIGIT (Memc[lbuf+5])) {
			ip = 6; ip = ctoi (Memc[lbuf], ip, ival)
			pp = STF_PDES(stf,min(ival,MAX_PCOUNT))
			call stf_gets (Memc[lbuf], P_PTYPE(pp), SZ_PTYPE)
		    } else
			call putline (spool, Memc[lbuf])

		} else if (strncmp (Memc[lbuf], "PDTYPE", 6) == 0) {
		    if (IS_DIGIT (Memc[lbuf+6])) {
			ip = 7; ip = ctoi (Memc[lbuf], ip, ival)
			pp = STF_PDES(stf,min(ival,MAX_PCOUNT))
			call stf_gets (Memc[lbuf], P_PDTYPE(pp), SZ_PDTYPE)
		    } else
			call putline (spool, Memc[lbuf])

		} else
		    call putline (spool, Memc[lbuf])

	    } else if (strncmp (Memc[lbuf], "END ", 4) == 0) {
		break

	    } else
		call putline (spool, Memc[lbuf])
	}

	call sfree (sp)
end
