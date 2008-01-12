include <fset.h>
include	<error.h>
include <mach.h>
include <finfo.h>
include <imio.h>
include <imhdr.h>
include "wfits.h"

# WFT_WRITE_FITZ -- Procedure to convert a single IRAF image or trailer file
# to a FITS file.

procedure wft_write_fitz (iraf_file, out_template, fits_file, fits_fd)

char	iraf_file[SZ_FNAME]	# IRAF file name
char	out_template[SZ_FNAME]	# FITS output template
char	fits_file[SZ_FNAME]	# FITS file name

int	chars_rec, dev_blk, nchars
pointer	im, fits

pointer	immap()
int	fits_fd
bool	gi_geis()
int	mtfile(), mtopen(), open(), fnldir(), fstati()

errchk	immap, imunmap, mtopen, close, smark, sfree, open
errchk	delete, wft_write_header, wft_write_image, wft_data_limits
errchk  wft_gi_opengr, wft_wgi_xdim 
int	gcount, gn, gi_gstfval(), tape
char	root[SZ_FNAME], extn[SZ_EXTN], temp[SZ_FNAME], line[SZ_LINE]
int	cl_index(), fnextn(), ip, junk, finfo(), itoc()
long    fi[LEN_FINFO]

define  err_ 99

include "wfits.com"

begin

	if (first_time == NO && extensions == YES) {
	   ext_type = IMAGE
	}

	# Open input image.
	im = immap (iraf_file, READ_ONLY, 0)

	# Do a preliminary input file validation
	#
	if (IM_NDIM(im) < 0 || IM_PIXTYPE(im) < 0 ||
	    IM_PIXTYPE(im) > 12) {
	    if (finfo(iraf_file, fi) != ERR) {
	       if (FI_SIZE(fi) == 0) 
	          call sprintf (temp, SZ_FNAME,
			"*** Null length header file `%s'")
	       else
	          call sprintf (temp, SZ_FNAME,
			"*** Bad image: '%s'")
	    } else
	       call sprintf (temp, SZ_FNAME, "*** Bad or empty image: '%s'")
	    call pargstr( iraf_file )
	    call put_in_log (temp)
	    call put_in_log (" \n")
	    call error (13,temp)	
	}

        iferr (gcount = gi_gstfval (im, "GCOUNT"))
	      gcount = 0

	# Create a new fits if
	if (gcount > 1 && extensions == YES)
	   ext_type = NULL
	
	# Allocate memory for program data structure.
	call calloc (fits, LEN_FITS, TY_STRUCT)

	allgroups = NO
	call imgcluster (iraf_file, IRAFNAME(fits), SZ_FNAME)
	call strcpy (IRAFNAME(fits), temp, SZ_FNAME)

	# See if name has '-' or '+' and escape them.
	#
	call pesc_dash (IRAFNAME(fits))
	nchars = fnldir (IRAFNAME(fits), IRAFNAME(fits), SZ_FNAME)
	call strcpy (temp[nchars+1], IRAFNAME(fits), SZ_FNAME)
	# Clear the escape character.
	call cesc_dash (IRAFNAME(fits))

	tape = mtfile (fits_file)
	if (gi_geis (im)) {    # Only for GEIS files
	   if (cl_index(iraf_file) == 0 && sdasmgcv < 0 && gcount > 1)
	         allgroups = YES

	   if (first_time == NO && allgroups == YES) {
	      # make a new output FITS filename
	      first_time = YES
	      file_number = file_number + 1
	      call close(fits_fd)
	      call mk_output_name (tape, 2, file_number, iraf_file,
				out_template, fits_file)
	   }

	   # Convert only if number of groups > 1 and there is a gpb.
	   if (gi_gstfval (im, "PSIZE") == 0 && gcount <= 1)
	        sdasmgcv = 0

	   # If sdasmgcv > 0  then append '_cvt' to the root.
	   if (sdasmgcv > 0) {
	      call iki_parse (IRAFNAME(fits), root, extn)
	      call strcat ("_cvt", root, SZ_FNAME)
	      if (extn[1] == EOS)
		 call strcpy("hhh", extn, SZ_EXTN)
	      call iki_mkfname (root, extn, IRAFNAME(fits), SZ_FNAME)
	   }
	} else 
	   sdasmgcv = -1

	if (tape == YES) {
	    if (blkfac > 10)
		chars_rec = (blkfac * FITS_BYTE) / (SZB_CHAR * NBITS_BYTE)
	    else
	        chars_rec = (blkfac * len_record * FITS_BYTE) / (SZB_CHAR *
	            NBITS_BYTE)
	    if (first_time == YES)
	       fits_fd = mtopen (fits_file, WRITE_ONLY, chars_rec)

	    dev_blk = fstati (fits_fd, F_MAXBUFSIZE)
	    if (dev_blk != 0 && chars_rec > dev_blk) {
		call flush (STDOUT)
		call error (0, "Blocking factor too large for tape drive")
	    }
	} else {
	    blkfac = 1
	    if (first_time == YES)
	       fits_fd = open (fits_file, NEW_FILE, BINARY_FILE)
	}

	# Write header and image.

	if (long_header == YES) {
	   if (sdasmgcv == NO) {
	      call printf (" -> %s ")
		 call pargstr (fits_file)
	   }
	   call printf ("\n")
	}

	# See if we want to write all the groups into
	# fits files.
        if (tape == NO) {
	   call zfnbrk (fits_file, ip, junk)
	   call strcpy (fits_file, root, junk-1)
	   junk =  fnextn (fits_file, extn, SZ_FNAME)
	}

	iferr {
	    call strcpy (fits_file, temp, SZ_FNAME)
	    if (tape == YES)
	       junk = itoc (file_number, temp, SZ_FNAME)
	    if (allgroups == YES) {
	       do gn = 1, gcount {
		  if (tape == NO && extensions == NO) {
		     call sprintf (temp, SZ_FNAME, "%s%03d")
		        call pargstr (root)
		        call pargi (gn)
		     call iki_mkfname (temp, extn, temp, SZ_FNAME)
		     call strcpy (temp, fits_file, SZ_FNAME)
		  } if (tape == YES && extensions == NO)
	             junk = itoc (file_number, temp, SZ_FNAME)
		  call wft_gi_opengr (im, gn, iraf_file, fits, 
				    fits_file, fits_fd)
	          call wft_write_header (im, fits, fits_fd)
	          if (short_header == YES) {
	             call print_key (IRAFNAME(fits), temp, im, fits)
		     if (extensions == YES)
			call strcpy("  IMAGE", temp, SZ_FNAME)
		  }
		  # See if we want to write IMAGE Xtension.
		  if (extensions == YES) {
		     ext_type = IMAGE 
		  } else { 			# One group per FITS file
	             if (tape == YES)
                        file_number = file_number + 1
		  }
	  	  call wft_write_image (im, fits, fits_fd)
	       }
	       if (extensions == YES)
		  call close (fits_fd)
	       ext_type = NULL
               # Get ready for next input file
	       if ( extensions == NO && tape == YES)
                  file_number = file_number - 1
	    } else {
	       call wft_write_header (im, fits, fits_fd)
	       if (short_header == YES) {
		   if (first_time == NO)
		      call strcpy("  IMAGE", temp, SZ_FNAME)
	           call print_key (IRAFNAME(fits), temp, im, fits)
		   call flush (STDOUT)
	       }
	       if (sdasmgcv > 0)
		  call wft_wgi_xdim (im, fits_file, fits, fits_fd)
	       else {
		  if (extensions == YES)
		     ext_type = IMAGE
		  call wft_write_image (im, fits, fits_fd)
	       }
	       if (long_header == YES)
	           call printf ("\n")
	       
	    }
	} then {
	    # Close files and cleanup.
	    call imunmap (im)
	    call close (fits_fd)
	    call mfree (fits, TY_STRUCT)
	    # Get the error message and send it to the calling
	    # routine error handler.
	    call errget (line, SZ_LINE)	    
	    call error(13,line)
	} else {
	    # Close files and cleanup.
	    call imunmap (im)
	    if (extensions == NO)
	       call close (fits_fd)
	    call mfree (fits, TY_STRUCT)
	}
        return

end

include	<ctype.h>

# CL_INDEX -- procedure to calculate the group number specification
# from a Geis file specification.

int procedure cl_index (imspec)


char	imspec[ARB]		# full image specification
int	gnum		# receives cluster index (default 0)
int	cl_size			# receives cluster size (default 0)

bool	is_ksection
int	ip, op, lbrack, level, ch, n
int	stridx()

begin
	ip = 1
	op = 1

	# Extract cluster name.  The first (unescaped) [ marks the start of
	# either the cl_index subscript or a section field.

	for (ch=imspec[ip];  ch != EOS && ch != '[';  ch=imspec[ip]) {
	    if (ch == '\\' && imspec[ip+1] == '[')
		ip = ip + 1

	    ip = ip + 1
	}

	lbrack      = ip
	gnum    = 0
	cl_size     = 0

	if (ch == EOS)
	    return(ch)

	# If we have a [...] field, determine whether it is a cl_index
	# subscript or a kernel or image section.  A cl_index subscript is
	# anything with the syntax [ddd] or [ddd/ddd]; anything else is a
	# kernel or image section.

	ip = ip + 1
	n  = 0

	for (ch=imspec[ip];  ch != EOS;  ch=imspec[ip]) {
	    if (IS_DIGIT(ch)) {
		n = (n * 10) + TO_INTEG(ch)
	    } else if (ch == '/') {
		gnum = max (n, 1)
		n = 0
	    } else if (ch == ']') {
		ip = ip + 1
		break
	    } else {
		# Not a cl_index subscript; must be a section.
		ip = lbrack
		n  = 0
		break
	    }
	    ip = ip + 1
	}

	if (gnum == 0)
	    gnum = n
	else
	    cl_size = n

	# The rest of the input string consists of the kernel and image
	# sections, if any.

	is_ksection = false
	level = 0
	op = 1

	if (imspec[ip] == '[') {
	    for (ch=imspec[ip];  ch != EOS;  ch=imspec[ip]) {
		if (ch == '[')
		    level = level + 1
		else if (ch == ']')
		    level = level - 1
		else if (!is_ksection)
		    if (stridx (imspec[ip], " 0123456789+-:*,") == 0)
			is_ksection = true

		ip = ip + 1

		if (level == 0)
		    break
	    }
	}
	return(gnum)

end

procedure pesc_dash (name)

char name[SZ_FNAME]

pointer sp, pp
int	i,j, np, stridx()
char   dash , plus

begin

	dash = '-'
	np = stridx(dash, name)
	plus = '+'
	if (np == 0)
	   np = stridx(plus, name)

	if (np != 0) {
	   call smark(sp)
	   call salloc(pp,SZ_FNAME,TY_CHAR)
	   j = 0
	   for (i=1; i<= SZ_FNAME ||name[i] == EOS; i=i+1) {

	       if (name[i] != '-' && name[i] != '+')
		  Memc[pp+j] = name[i]
	       else {
		  Memc[pp+j] = '\\'
		  j=j+1
		  Memc[pp+j] = name[i] 
	       }
	       j = j+ 1
	   }
	   call strcpy (Memc[pp], name, SZ_FNAME)
	   call sfree(sp)
	}
end

procedure cesc_dash (name)

char name[SZ_FNAME]

pointer sp, pp, np
int	i,j, stridx()
char   esc 

begin

	esc= '\\'
	np = stridx(esc, name)
 	if (np != 0) {
	   call smark(sp)
	   call salloc(pp,SZ_FNAME,TY_CHAR)
	   j = 0
	   for (i=1; i<= SZ_FNAME ||name[i] == EOS; i=i+1) {

	       if (name[i] != '\\')
		  Memc[pp+j] = name[i]
	       else {
		  if (name[i+1] == '-' || name[i+1] == '+') {
		     Memc[pp+j] = name[i+1]
		     i = i + 1
		  }
	       }
	       j = j+ 1
	   }
	   call strcpy (Memc[pp], name, SZ_FNAME)
	   call sfree(sp)
	}
end
