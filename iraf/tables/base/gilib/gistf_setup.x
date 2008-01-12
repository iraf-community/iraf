include <imio.h>
include <imhdr.h>
include <tbset.h>
include <mach.h>
include "gi.h"

# GISTFDES_SETUP -- Procedure to setup the stf group parameter
# descriptors from the table column information.

procedure gistfdes_setup (tp, stf, im)

pointer	tp			# i: Input table descriptor
pointer	stf			# i: STF descriptor
pointer im			# i: image descriptor

int	ncols, total, i, pp, colp
int	tbpsta(), tbcnum(), tbcigi(), totpix
long	sz_pixfile
int	open(), pfd
int	parnum
char	dtype, cbuf[SZ_LINE], uparm[SZ_KEYWORD]

string badtype "illegal group data parameter datatype"

begin

	ncols = tbpsta (tp, TBL_NCOLS)
	STF_PCOUNT(stf) = ncols

	total = 0
	do i = 1, ncols {
	   colp = tbcnum (tp, i)
	   pp = STF_PDES(stf, i)

	   # get datatype
	   P_LEN(pp) = 1
	   P_SPPTYPE(pp) = tbcigi (colp, TBL_COL_DATATYPE)
	   if (P_SPPTYPE(pp) < 0) { 	# table character type is negative
	      P_LEN(pp) = tbcigi (colp, TBL_COL_FMTLEN)
	      P_SPPTYPE(pp) = TY_CHAR
	   }
	   # get parameters name
	   call tbcigt (colp, TBL_COL_NAME, P_PTYPE(pp), SZ_PTYPE)
	   # get user parameter for the parameter name comment field.
	   iferr (call tbhgtt (tp, P_PTYPE(pp), P_COMMENT(pp), SZ_PARREC))
	        ;

	   # set up size and length.
	   switch (P_SPPTYPE(pp)) {
	   case (TY_BOOL):
	      # The STF kernel does not support this type, convert to
	      # INT
	      call strcpy ("INT*4", P_PDTYPE(pp), SZ_PDTYPE)
	      P_PSIZE(pp) = P_LEN(pp) * SZ_BOOL * SZB_CHAR * NBITS_BYTE 
	      P_SPPTYPE(pp) = TY_LONG
	   case (TY_INT):
	      call strcpy ("INT*4", P_PDTYPE(pp), SZ_PDTYPE)
	      P_PSIZE(pp) = P_LEN(pp) * SZ_INT * SZB_CHAR * NBITS_BYTE
	      # redefine type to LONG since this and SHORT are supported
	      # in STF.
	      P_SPPTYPE(pp) = TY_LONG
	      # see if column was INT*2 defined
	      call sprintf (uparm, SZ_KEYWORD, "I2COL%d")
		   call pargi (i)
	      call tbhfkr (tp, uparm, dtype, cbuf, parnum)
	      if (parnum > 0) {       # Column is TY_SHORT
	         P_PSIZE(pp) = P_LEN(pp) * SZ_SHORT * SZB_CHAR * NBITS_BYTE
		 P_SPPTYPE(pp) = TY_SHORT
	      }
	   case (TY_REAL):
	      call strcpy ("REAL*4", P_PDTYPE(pp), SZ_PDTYPE)
	      P_PSIZE(pp) = P_LEN(pp) * SZ_REAL * SZB_CHAR * NBITS_BYTE
	   case (TY_DOUBLE):
	      call strcpy ("REAL*8", P_PDTYPE(pp), SZ_PDTYPE)
	      P_PSIZE(pp) = P_LEN(pp) * SZ_DOUBLE * SZB_CHAR * NBITS_BYTE
	   case (TY_CHAR):
	      call sprintf (P_PDTYPE(pp), SZ_PDTYPE, "CH*%d")
		   call pargi(P_LEN(pp))
	      P_PSIZE(pp) = P_LEN(pp) * NBITS_BYTE
	   default:
	      call error (1, badtype)
	   }
	   total = total + P_PSIZE(pp)
	}
	STF_PSIZE(stf) = total
#	STF_SZGPBHDR(stf) = (STF_PCOUNT(stf) + 4) * (FITS_RECLEN + 1)

	# Redefine size of each group as the input file as STF_PSIZE of zero.
	totpix = STF_LENAXIS(stf,1)
	do i = 2, STF_NAXIS(stf)
	   totpix = totpix * STF_LENAXIS(stf,i)

	STF_SZGROUP(stf) = (totpix  * STF_BITPIX(stf) + 
		 	   STF_PSIZE(stf)) / (SZB_CHAR * NBITS_BYTE) 
	
	# Open pixfile
	sz_pixfile = STF_SZGROUP(stf) * STF_GCOUNT(stf)
	call falloc (IM_PIXFILE(im), sz_pixfile)
	pfd = open (IM_PIXFILE(im), READ_WRITE, BINARY_FILE)
	STF_PFD(stf) = pfd
	IM_PFD(im) = pfd
	IM_PIXOFF(im) = 1

end
