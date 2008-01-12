include	<imhdr.h>
include <mach.h>
include <tbset.h>
include <fset.h>
include <ctype.h>
include	"rfits.h"

# TAB_READ_DATA --  Read FITS table in row order as character strings
# and convert them to IRAF table rows. If the fits table contains a trailer
# file then the table descriptor 'tp' will have the descriptor for an
# ascii file.

procedure tab_read_data (fits_fd, ext, tp)

int	fits_fd		# FITS file descriptor
pointer	ext		# Extension data structure
pointer	tp 		# IRAF table descriptor

int	i, rowlen, blksize, nch
long	nlines, il, ncols
pointer	sp, bfp, tcp, ip,cp
int	rft_init_read_pixels(), rft_read_pixels()
int	tbpsta(), npix_record, fstati(), tbcnum()
int	rec_count

errchk	salloc, sfree, rft_init_read_pixels, rft_read_pixels, rft_scale_pix
errchk	rft_change_pix, rft_put_image_line, rft_pix_limits, smark
bool	trl		# If true convert fits table with trailer file to
			# ascii file

include	"rfits.com"
include "tab.com"

begin

	trl = false
	if (EXT_TTYPE(ext) == TRAILER_FILE || 
	    EXT_TTYPE(ext) == TXT_FILE )
	   trl = true
	rowlen = EXT_ROWLEN(ext)
	nlines = EXT_NROWS(ext)

	call smark (sp)
	if (!trl) {
	   ncols  = tbpsta (tp, TBL_MAXCOLS)
	   call salloc (cp, ncols, TY_INT)
	   call salloc (bfp, rowlen, TY_CHAR)
	   do i = 1, ncols
	      Memi[cp+i-1] = tbcnum (tp, i)
	} else
	   call salloc (bfp, rowlen+1, TY_CHAR)    # to put EOL

	npix_record = len_record * FITS_BYTE / EXT_BITPIX(ext)
	i = rft_init_read_pixels (npix_record, EXT_BITPIX(ext), LSBF, TY_CHAR)
	blksize = fstati (fits_fd, F_SZBBLK)
	if (mod (blksize, 2880) == 0)
	    blksize = blksize / 2880
	else
	    blksize = 1

	# Put EOS at the end, rft_red_pixels does not put one at rowlen.
	Memc[bfp+rowlen] = EOS

	rec_count = 0
	if (trl) {      		# Is a "trailer file", to ascii file
	   do il = 1, nlines {
	      nch = rft_read_pixels (fits_fd, Memc[bfp], rowlen,
	          rec_count, blksize)
	      if (nch != rowlen)
		  call printf ("Error reading FITS data\n")
	      for (i=rowlen+bfp-1; (Memc[i]!=EOS) && (IS_WHITE(Memc[i]));
		                    i=i-1) ;
	      Memc[i+1] = '\n'
	      Memc[i+2] = EOS
	      call putline (tp, Memc[bfp])
	   }
	} else if (ext_type == BINTABLE) {	# Is a Binary table extension
	   call ieesnanr (INDEFR)     # Enable ieee handling of NaN
	   call ieesnand (INDEFD)
	   byte_input = YES
	   call salloc (tcp, maxlen, TY_CHAR)
	   call salloc (ip, maxlen, TY_CHAR)
	   if (BIN_DTYNSP)  {
      call eprintf("\n Input file contains Non supported binary table data.\n")
      call eprintf("The data has more that on element per table element.\n")
	   }

	   do il = 1, nlines {
	       nch = rft_read_pixels (fits_fd, Memc[bfp], rowlen,
	           rec_count, blksize)
	       if (nch != rowlen)
		   call printf ("Error reading FITS data\n")

	       # DO NOT WRITE TABLE DATA IF IT CONTAINS 3D DATA, i.e.
	       # more than one element per cell
	       if (!BIN_DTYNSP) {
	         call rft_p3d_table_row (tp, ext, Memi[cp], Memc[bfp],
					Memc[tcp], Memc[ip], ncols, il)
	       }
	   }
	   byte_input = NO
	} else {			# Is regular table extension
	   do il = 1, nlines {
	       nch = rft_read_pixels (fits_fd, Memc[bfp], rowlen,
	           rec_count, blksize)
	       if (nch != rowlen)
		   call printf ("Error reading FITS data\n")
   
	       call rft_put_table_row (tp, ext, Memi[cp], Memc[bfp],
					rowlen,ncols, il)
	   }
	}

	call sfree (sp)
end
