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

procedure tab_read_data (fits_fd, ext, tp, fits)

int	fits_fd		# FITS file descriptor.
pointer	ext		# Extension data structure.
pointer	tp 		# IRAF table descriptor.
pointer	fits 		# FITS structure.

size_t	sz_val
long	l_val0, l_val1
int	j, ncols, i_val
long	nlines, il, rec_count, nch, len, junk
size_t	npix_record, blksize, rowlen
pointer	sp, bfp, tcp, ip, cp, i
bool	trl		# If true convert fits table with trailer file to
			# ascii file

long	rft_init_read_pixels(), rft_read_pixels(), fstatl(), lmod()
int	tbpsta()
pointer	tbcnum()
errchk	salloc, sfree, rft_init_read_pixels, rft_read_pixels, rft_scale_pix
errchk	rft_change_pix, rft_put_image_line, rft_pix_limits, smark
include	"rfits.com"

begin

	trl = false
	if (TAB_TYPE(ext) == TXT_FILE)
	   trl = true
	rowlen = EXT_ROWLEN(ext)
	nlines = EXT_NROWS(ext)

	call smark (sp)
	if (!trl) {
	   ncols  = tbpsta (tp, TBL_MAXCOLS)
	   sz_val = ncols
	   call salloc (cp, sz_val, TY_POINTER)
	   sz_val = rowlen
	   call salloc (bfp, sz_val, TY_CHAR)
	   do j = 1, ncols {
	      Memp[cp+j-1] = tbcnum (tp, j)
	   }
	} else {
	   sz_val = rowlen+1
	   call salloc (bfp, sz_val, TY_CHAR)    # to put EOL
	}

	npix_record = len_record * FITS_BYTE / EXT_BITPIX(ext)
	junk = rft_init_read_pixels (npix_record, EXT_BITPIX(ext), LSBF, TY_CHAR)
	blksize = fstatl (fits_fd, F_SZBBLK)
	l_val0 = blksize
	l_val1 = 2880
	if (lmod (l_val0, l_val1) == 0)
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
		  call eprintf ("Error reading FITS data\n")

	      i = rowlen+bfp-1
	      len = rowlen
	      while (IS_WHITE(Memc[i]) && len > 0) {
 		 i = i -1
		 len = len -1
	      }

	      Memc[i+1] = '\n'
	      Memc[i+2] = EOS
	      i_val = tp
	      call putline (i_val, Memc[bfp])
	   }
	} else if (FITS_XTEN(fits) == BINTABLE) { # Is a Binary table extension
	   call ieesnanr (INDEFR)     # Enable ieee handling of NaN
	   call ieesmapr (YES, YES)

	   call ieesnand (INDEFD)
	   call ieesmapd (YES, YES)

	   byte_input = YES
	   sz_val = BIN_MAXLEN(ext)
	   call salloc (tcp, sz_val, TY_CHAR)
	   call salloc (ip, sz_val, TY_CHAR)
	   if (BIN_DTYNSP(ext))  {
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
	       if (!BIN_DTYNSP(ext)) {
	         call rft_p3d_table_row (tp, ext, Memp[cp], Memc[bfp],
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
   
	       l_val0 = rowlen
	       call rft_put_table_row (tp, ext, Memp[cp], Memc[bfp],
				       l_val0, ncols, il)
	   }
	}
	call sfree (sp)
end
