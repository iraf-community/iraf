include <error.h>
include <imhdr.h>
include <imio.h>
include <imset.h>
include <fio.h>
include <tbset.h>
include	"rfits.h"
#
# Revision History
#
# Nov 8 1990	Allow for dataless images to be created, with exception
#		of those fits header in which the keyword IRAFNAME is 
#		"null_image". A dataless fits file has NAXIS = 0.
#
#		Allow for FITS tables whose IRAFNAME content has an extension
#		'trl' (ascii trailer file of up 132 character per line) to
#		be converted directly to an ascii file.
#
# May 30 1997	Replace the checks on null_image with calls to chk_tabname

# RFT_READ_FITZ -- Convert a FITS file. An EOT is signalled by returning EOF.

int procedure rft_read_fitz (fitsfile, template, iraffile, ext_number)

char	fitsfile[SZ_FNAME]	# FITS file name
char	iraffile[SZ_FNAME]	# IRAF file name
char	template[SZ_FNAME]	# Template filename
int	ext_number		# Index of the FITS extension to retrieve

char	uscore
bool	gi_geis()
char	root[SZ_FNAME], cluster[SZ_FNAME]
char	tempf[SZ_FNAME], extn[SZ_FNAME]
char	tabfile[SZ_FNAME], seqfile[SZ_FNAME]
int	fits_fd, istat, pos1, pos2, ntab, nch, junk
int	nread, fd_usr, ncols, first_im, stat
bool    trl
pointer	im, imt, fits, tp, ext

int	rft_read_header(), mtopen(), strlen()
int	tab_read_header(), gstrmatch()
int	open(), gi_gstfval(), strcmp(), chk_tabname()
int	strldx(), tbpsta(), rft_image_ext(), fnroot()
int	bitpix_to_imtype()
pointer	tbtopn()

data	uscore / '_' /
errchk	smark, sfree, salloc, fits_reblock, rft_read_header, rft_read_image
errchk	mtopen, close, imunmap, frename, rft_opnim, mtopen
	
include	"rfits.com"

define  read_extn_ 99
begin
	stat = 0
	# Open input FITS data
	fits_fd = mtopen (fitsfile, READ_ONLY, 0)

	# Allocate memory for program data structure
	call calloc (fits, LEN_FITS, TY_STRUCT)

	FITS_XTEN(fits) = NO
	call pr_files (iraffile, fitsfile)

	call imgcluster (iraffile, cluster, SZ_FNAME)
	call iki_parse (cluster, root, extn)

	# 'gkey' can have the following values
	#  DEF_GPB:    Will create a default gp descriptor
	#  NONDEF_GPB: Will read the gp descriptor from a user supplied
	#	       template with the non_default gp keywords.
	#  NON_GPB:    Will not create a gp descriptor in the output SDAS
	#	       file. This value gets setup with the fits keyword
	#	       SDASMGNU, which indicates that the input fits file
	#	       contains a file with an extra dimension for the groups
	#	       and an attached table with the gp values.
	#  TO_MG:      Will create a multigroup Geis file from the input
	#	       FITS file and its attached table. The FITS header
	#	       should have the keyword SDASMGNU and OPSIZE to
	#	       accomplish this.
	#  IMH:	       If the output file is 'imh' type.


	if (strlen(template) != 0 && gkey == TO_MG) {
	   call eprintf("\n ****  Please revise your parameter file\n")
	   call error (1,"You cannot have a template header and xdimtogf=YES")
	}

	if (strlen(template) != 0)
	   gkey = NONDEF_GPB		

	# If we want to build a geis file, we should not have an imh
	# extension.
	if (strcmp(extn, "imh") == 0 && gkey == NONDEF_GPB)
           call error(1,"You cannot select the 'imh' extension and a template")

	# Open spool file to contain the fits header
	fd_usr = open ("fits_hdr", READ_WRITE, SPOOL_FILE)
	
	nread = rft_read_header (fits_fd, fd_usr, fits)

	# If we want to read an extension only, we need to skip the main data
	# unit
	if (ext_number > 0) {
	   call skip_mdata (fits_fd, fits)
	   call close (fd_usr)
	   call printf(" ... main FITS unit skipped.\n")
	   goto read_extn_
	}

	if (gkey == TO_MG && GCOUNT(fits) == -1)
	     gkey = DEF_GPB 

	if (force == NO &&
	    EXTEND(fits) == YES &&
	    SDASMGNU(fits) == NO &&
	    chk_tabname (IRAFNAME(fits)) == NO)
	    gkey = TO_FITS

	# Set the output file name
	call change_name (iraffile, fits)

	# Print descriptive info about the file
	call print_header (fits, fitsfile, iraffile)

	# The patch for freaky fits files with extensions
	if (gkey == TO_FITS) {
	    call mfree (fits, TY_STRUCT)
	    call close (fd_usr)
	    call close (fits_fd)

	    call fits_reblock (fitsfile, iraffile)

	    return (stat)
	}

	if (nread != EOF) {
	    iferr {
		call rft_opnim (template, iraffile, fd_usr, fits,
				nread, im, imt)
		call close (fd_usr)
	        call rft_read_image (fits_fd, fits, im)

	    } then {
	        call erract (EA_ERROR)
		return (stat)
	    }
	} else {
	    call mfree (fits, TY_STRUCT)
	    call close (fd_usr)
	    call close (fits_fd)
	    return (EOF)
	}

        if (NAXIS(fits) != 0) {
	   if (gi_geis (im)) {
	      # Update the wcs values since the STF_WCS routines did 
	      # not have the CRPIX nor the CRVAL's keywords in the user header.
              if (gkey == DEF_GPB)
	         call update_gpb (im,fits)
	      # Because of some bug in stf the value below does
	      # not get change to YES.
	      if (gkey == NON_GPB && gi_gstfval(im, "PCOUNT") == 0)
		 call gi_pstfval (im, "GROUPS", YES)
	   }
 	   if (gkey == NON_GPB || gkey == NONDEF_GPB)
	      call imunmap (imt)

	   first_im = YES
	   if (EXTEND(fits) == NO || ext_number == 0)
	      call imunmap(im)

        } else {   # if naxis==0, we'll create a dataless image
	   # There is no image created if the fits file contains
	   # a dummy header with a table in the first extension
	   if (chk_tabname (IRAFNAME(fits)) == NO) {
	      IM_PIXTYPE(im) = bitpix_to_imtype(fits)
	      if (strcmp(extn,"imh") != 0)
		 call wr_header(im,fits)
	      else
	         call imunmap(im)
	   }
	   first_im = NO
	}

	call strcpy (iraffile, tempf, SZ_FNAME)



	# Do not read extensions if we wanted to read the main FITS unit
	# only.
	if (ext_number == 0) {
	   call mfree (fits, TY_STRUCT)
	   call close (fits_fd)
	   return (stat)
	}
	# If the above main header contains an extension flag, then
	# lets see if we find TABLE, BINTABLE or IMAGE.
 read_extn_
	if (EXTEND(fits) == YES) {

	   # No ieee for tables in FITS 'TABLE' format
	   ieee = NO
	   FITSTYPE(fits) = EOS
	   # Append default extension.
	   call tbtext (root, tabfile, SZ_FNAME)
	   call strcpy (tabfile, seqfile, SZ_FNAME)
	   # Look for last character position in root of seqfile.
	   nch = gstrmatch (seqfile, ".tab", pos1, pos2)
	   if (nch == 0) pos1 = strlen(tabfile)
	   ntab = 1
	   call sprintf (seqfile[pos1], SZ_FNAME, "%02d.tab")
	        call pargi(ntab)
	   # look for more than one table in the current file

           repeat {
	      if (ext_number > 1)
	         call skip_extensions (ext_number, fits_fd)
	      tp = tbtopn (seqfile, NEW_FILE, 0)
	  
	      # Allocate space for the extension structure
	      call calloc (ext, LEN_EXTENSION, TY_STRUCT)

	      # Read FITS table header an user parameters if any, also
	      # create the table 'tbtcre'. Extension structure 'ext' is
	      # returned as well.
	      istat = tab_read_header (fits_fd, im, ext, tp, fits)

	      if (ext_number >1 && istat == EOF) {
		 call eprintf("\nEOF encountered. Extension number specified")
		 call eprintf(" is greater than number of extensions.\n")
	      }
	      # Ready to read a table extension. 
	      if (FITS_XTEN(fits) != IMAGE && first_im == YES) {
		 if (gkey == DEF_GPB)
		    call update_gpb (im,fits)
		 call imunmap(im)
		 first_im = NO
	      }
	      # If we had read a fits header for a FITS text file then.
	      trl = false
	      if (TAB_TYPE(ext) != SDAS_TABLE) {
		  trl = true
		  call gen_fname (IRAFNAME(fits), EXTNAME(ext), 
				  root, extn, SZ_FNAME)
	      }

	      # Istat will have the value below only after the
	      # first reading of the header. If the value is not
	      # encountered then it will read the whole table header
	      # before coming here.

	      if (istat == IMAGE) {
		 call tbtclo (tp)
		 if (gkey == IMH) {
		    call strcpy (root, tempf, SZ_FNAME)
		    call sprintf(tempf[strlen(tempf)+1],SZ_FNAME,"_%d.imh")
			 call pargi(ntab)
		    ntab = ntab + 1
		 } else {
		    call strcpy (seqfile, root, pos1+1)
		    call iki_mkfname (root, extn, iraffile, SZ_FNAME) 
		       if (gkey == DEF_GPB)
		          call strcpy (iraffile, tempf, SZ_FNAME)
		 }
		 junk = rft_image_ext (im, fits_fd, tempf, template, fits)
		 if (old_name == NO && gkey == DEF_GPB) {
	            ntab = ntab + 1
	            call sprintf (seqfile[pos1], SZ_FNAME, "%02d.tab")
	               call pargi(ntab)
	         }
		 first_im = NO
	      } else if (istat != EOF) {
		 # Now read the fits table data. See if we need to convert
		 # to a text file.
		 if (TAB_TYPE(ext) != SDAS_TABLE) {
		    call tbtclo (tp)
#		    call delete (seqfile)
		    nch = strldx(".", tabfile)
		    call strcpy (extn, tabfile[nch+1], SZ_FNAME)
		    # Open an ascii file to convert a fits table with
		    # a trailer file into an ascii file.
		    nch = strlen(seqfile)
		    call strcpy (extn, seqfile[nch-2], SZ_FNAME)
		    tp = open (seqfile, NEW_FILE, TEXT_FILE)
		    call tab_read_data (fits_fd, ext, tp, fits)
		    ncols = 1
		    call close (tp)
	         } else {   
		    call tab_read_data (fits_fd, ext, tp, fits)
		    ncols = tbpsta(tp, TBL_NCOLS)
		    call tbtclo (tp)
		 }
		  
		 # Print table information to STDOUT.
		 call prtab_info (fits, ext, seqfile, ncols)
	      } else  # istat is EOF
		 call tbtclo (tp)

	      if (istat != IMAGE && istat != EOF) {
	         call strcpy (seqfile, tempf, SZ_FNAME)
	         ntab = ntab + 1
	         call sprintf (seqfile[pos1], SZ_FNAME, "%02d.tab")
	             call pargi(ntab)
	      }
	      # Now free up the memory allocated in ext.
	      call ext_free (ext)
	   } until (istat == EOF || ext_number > 0)
	   # The last extension was IMAGE.
	   if (im != NULL) {
	      if (gkey == DEF_GPB)
		 call update_gpb (im,fits)
	      call imunmap(im)
	      EXTEND(fits) = NO
	   }
           # Get rid of the sequential number if only 
	   # one table is present.
	   if (old_name == NO && ntab == 2 && FITS_XTEN(fits) != IMAGE) {
	      call frename (tempf, tabfile)
	      if (short_header == YES ) {
		 nch = fnroot (tabfile, root, SZ_FNAME)
		 if (!trl) {
	            call printf("%17t renamed to %s\n")
	  	        call pargstr(tabfile)
		 } else {
	            call printf("%17t renamed to %s.%s\n")
	  	        call pargstr(root)
		        call pargstr(extn)
	         }
	      }
	   }
	} # EXTEND(fits)

	call mfree (fits, TY_STRUCT)
	call close (fits_fd)
	return (stat)
end
procedure ext_free(ext)
pointer ext
begin

	call mfree (EXT_PDISP(ext), TY_CHAR)
	call mfree (EXT_PFORM(ext), TY_CHAR)
	call mfree (EXT_PUNIT(ext), TY_CHAR)
	call mfree (EXT_PDSIZE(ext), TY_INT)
	call mfree (EXT_PDTYPE(ext), TY_INT)
	call mfree (EXT_PTYPE(ext), TY_CHAR)
	call mfree (EXT_PNULL(ext),TY_CHAR)
	call mfree (EXT_PSCAL(ext),TY_DOUBLE)
	call mfree (EXT_PZERO(ext), TY_DOUBLE)
	call mfree (EXT_PCW(ext), TY_INT)
	call mfree (EXT_PBCOL(ext), TY_INT)
	call mfree (ext, TY_STRUCT)
end
# CHANGE_NAME -- Procedure to change the name of the temporary file name
#		'iraffile'  to the value of IRAFNAME(fits)

procedure change_name (iraffile, fits)
char	iraffile[SZ_FNAME]
pointer fits


char	root[SZ_FNAME]
char	nroot[SZ_FNAME], nextn[SZ_EXTN]
char	extn[SZ_FNAME]
int	nc, cl_index, cl_size, len_name, count
pointer sp, bf, dp, tname

int	strmatch(), strcmp(), strncmp(), strlen(), fnldir(), access()
int	gstrcpy(), chk_tabname()

include "rfits.com"

begin
	call smark (sp)
	call salloc (bf, SZ_FNAME, TY_CHAR)
	call salloc (dp, SZ_FNAME, TY_CHAR)
	call salloc (tname, SZ_FNAME, TY_CHAR)

	call imgcluster (iraffile, Memc[bf], SZ_FNAME)
	call iki_parse (Memc[bf], root, extn)

	if (old_name == NO) {
	    if (chk_tabname (IRAFNAME(fits)) == YES) {
		call gen_fname (IRAFNAME(fits), "", nroot, nextn, SZ_FNAME)
		if (strcmp (nroot, "null_image") == 0) {
		    call strcpy (nroot, iraffile, SZ_FNAME)
		} else {
		    call iki_mkfname (root, nextn, iraffile, SZ_FNAME)
		}

	    } else if (gkey == TO_FITS) {
		call strcpy ("fits", nextn, SZ_EXTN)
		call iki_mkfname (root, nextn, iraffile, SZ_FNAME)
	    }

	} else {
	    len_name = strlen(IRAFNAME(fits))
	    if (len_name == 0) {
		call strcpy (iraffile, IRAFNAME(fits), SZ_FNAME)
	    } else if (NAXIS(fits) == 0) {
		call strcpy (IRAFNAME(fits), iraffile, SZ_FNAME)
	    }

	    if (chk_tabname (IRAFNAME(fits)) == YES) {
		call gen_fname (IRAFNAME(fits), "", nroot, nextn, SZ_FNAME)
		call iki_mkfname (nroot, nextn, iraffile, SZ_FNAME)

	    } else {
		if (len_name != 0) {
		    # Get directory prefix from 'iraffile'.
		    nc = fnldir (Memc[bf], Memc[dp], SZ_FNAME)
		    call gparse (IRAFNAME(fits), Memc[bf], nroot, 
				 nextn,cl_size,cl_index)
		    call pesc_dash (Memc[bf])

		    if (nextn[1] == EOS) {
			if (gkey == TO_FITS) {
			    call strcpy ("fits", nextn, SZ_EXTN)
			} else {
			    call strcpy ("hhh", nextn, SZ_EXTN)
			}
		    }

		    if (gkey == TO_MG)
			if (strmatch (nroot, "_cvt") != 0)
			    nroot[strlen(nroot)-3] = EOS 

		    call strcat (nroot, Memc[dp], SZ_FNAME)

		    # Now copy 'hhh' new_extensio into 'imh' (extn).
		    if (strcmp(extn,nextn) != 0)  # Extensions are diferent
			if (strcmp(extn,"imh") != 0 && 
			    strcmp(nextn,"imh") == 0)
			    call strcpy(extn, nextn, SZ_EXTN)

		    # Now the other way around.
		    if (gkey == IMH)
			call strcpy("imh",nextn, SZ_EXTN)

		    if (cl_index > 0) {
			call sprintf (Memc[bf],SZ_FNAME,"_%d")
			call pargi(cl_index)
			call strcat(Memc[bf], Memc[dp], SZ_FNAME)
		    }

		    # Append original extension to the root 
		    # if we have a fits file

		    if (gkey == TO_FITS && strncmp (nextn, "fit", 3) != 0) {
			call sprintf (Memc[bf], SZ_FNAME, "_%s")
			call pargstr (nextn)
			call strcpy ("fits", nextn, SZ_EXTN)
			call strcat(Memc[bf], Memc[dp], SZ_FNAME)
		    }

		    call iki_mkfname (Memc[dp], nextn, iraffile, SZ_FNAME)
		    call cesc_dash (iraffile)

		} else if (gkey == TO_FITS) {
		    call strcpy ("fits", nextn, SZ_EXTN)
		    call iki_mkfname (root, nextn, iraffile, SZ_FNAME)
		    call strcpy (iraffile, IRAFNAME(fits), SZ_FNAME)

		} else {
		    call strcpy (extn, nextn, SZ_EXTN)
		}
	    }
	}

	# See if the file already exists. If it does generate
	# another file name

	count = 0
	while (access (iraffile,0,0) == YES) {
	    count = count + 1
	    if (count == 1) {
		call strcpy (iraffile, Memc[tname], SZ_FNAME)
		nc = gstrcpy (root, nroot, SZ_FNAME)
		nroot[nc+2] = EOS
	    } else {
		nroot[nc+1] = 'a' + (count - 2)
	    }
	    call iki_mkfname (nroot, nextn, iraffile, SZ_FNAME)
	}

	if (count > 0) {
	    call eprintf("\n Warning: Cannot rename %s to %s\n")
	    call pargstr (iraffile)
	    call pargstr (Memc[tname])
	}

	call sfree(sp)
end

define	NONGPB_HDR	"fitsio$non_gpb.hhh"
define	LEN_CARDP1	81
# The following definition is necessary to open the template
# file 'non_gpb.hhh' which has the keywrod "GROUPS = F" 
# plus PCOUNT and GCOUNT to zero also.

procedure rft_opnim (template, iraffile, fd_usr, fits, nread, im, imt)

char	template[SZ_FNAME]	# template file name
char	iraffile[SZ_FNAME]	# output image name
int	fd_usr			# fits header spool file des.
pointer fits			# fits descriptor
int	nread			# number of header lines in the fits header
pointer	im			# output image descriptor
pointer	imt			# o: template image pointer

pointer	ua
int	i, fd, maxlines, max_lenuser, cl_index,cl_size
char	cluster[SZ_FNAME], tmp[SZ_FNAME], root[SZ_FNAME] 

bool	gi_geis()
int	stropen(), strlen(), chk_tabname()
pointer	immap()

errchk  immap
include "rfits.com"

begin	
	if (chk_tabname (IRAFNAME(fits)) == YES)
	   return        

	# If template is specified, the user has chosen to create a
	# non_default gpb descriptor. This will reset any previous
	# value, e.g. gkey = NON_GPB if the keyword SDASMGCV was
	# present in the fits header.
	if (EXTEND(fits) == YES ||
	    FITS_XTEN(fits) == IMAGE ) { # We might have a FITS file with IMAGE
				   # extension
	   if (template[1] != EOS) gkey = NONDEF_GPB

	   # See if there is a groups specification in the output geis file;
	   # i.e. '[cl_index/cl_size]'.
           call gparse (IRAFNAME(fits), cluster, tmp, root,cl_size,cl_index)

           if (cl_size > 1 && gkey != IMH) {
	      gkey = -1
	      call sprintf (iraffile[strlen(iraffile)+1], SZ_FNAME, "[1/%d]")
			call pargi (cl_size)
	   } else if (cl_index > 1  && gkey != IMH) {
	      gkey = IMAGE
	      call sprintf (iraffile[strlen(iraffile)+1], SZ_FNAME, "[%d]")
		  call pargi (cl_index)
	      call print_header (fits, "", iraffile)
	   } else if (FITS_XTEN(fits) == IMAGE) { # We have a new IMAGE
	      call imunmap(im)
	      if (gkey != IMH)
	         gkey = DEF_GPB
	      if (old_name == YES)
		 call change_name (iraffile, fits)
	      call print_header (fits, "", iraffile)
	   }

	}


	# Create IRAF image header.
	   if (gkey == NONDEF_GPB) {
	   imt = immap (template, READ_ONLY, 0)
	   im  = immap (iraffile, NEW_COPY, imt)
	} else if (gkey == NON_GPB) {
	   imt = immap (NONGPB_HDR, READ_ONLY, 0)
	   im  = immap (iraffile, NEW_COPY, imt)
	} else if (gkey == IMAGE) {
	   call gi_newgrp (im, cl_index, IRAFMIN(fits), IRAFMAX(fits), 0)
	} else
	   im = immap (iraffile, NEW_IMAGE, 0)
	# reset the naxis things
	IM_NDIM(im) = NAXIS(fits)

	do i = 1, IM_NDIM(im)
	   IM_LEN(im,i) = NAXISN(fits,i)
	
	# Now copy the fits header lines onto the IM_USERAREA
	maxlines = (ARB - 3700)/LEN_CARD
	if (nread > maxlines) {
	   call printf ("=== %d fits header lines discarded\n")
		call pargi (nread - maxlines)
	   call printf ("Maximun number of lines is: %d\n")
		call pargi (maxlines)
	   nread = maxlines
	} 
	IM_LENHDRMEM(im) = nread*LEN_CARD + LEN_IMHDR + 81*32
	call realloc (im, IM_LENHDRMEM(im) + LEN_IMDES, TY_STRUCT)
	max_lenuser = (IM_LENHDRMEM(im) + LEN_IMDES - IMU)*SZ_STRUCT
	ua = IM_USERAREA(im) 
	fd = stropen (Memc[ua], max_lenuser, NEW_FILE)
	call seek (fd_usr, BOFL)

	if (gkey == DEF_GPB && gi_geis (im))
	   call rft_create_gpb (im, fd)

	call fcopyo (fd_usr, fd)
	call close (fd)

	iferr {
	   call imgstr (im, "TARGNAME", IM_TITLE(im), SZ_OBJECT)
	} then {
	   iferr (call imgstr (im, "OBJECT", IM_TITLE(im), SZ_OBJECT))
	         IM_TITLE(im) = EOS
	}
end


# PRINT_HEADER -- Routine to  print header information.

procedure print_header (fits, fitsfile, iraffile)

pointer fits
char	fitsfile[SZ_FNAME]
char	iraffile[SZ_FNAME]

char	root[SZ_FNAME]
char	extn[SZ_EXTN]
int	k, strlen(), itab
int	len_name
pointer sp, bf

include "rfits.com"

begin
	if (short_header == NO)
	   return

	call smark (sp)
	call salloc (bf, SZ_FNAME, TY_CHAR)
	call imgcluster (iraffile, Memc[bf], SZ_FNAME)
	call iki_parse (Memc[bf], root, extn)

	# set itab for tape or disk file
	itab = 16
	if (tape == YES)
	   itab = 5

	len_name = strlen(IRAFNAME(fits))
	if (len_name == 0)
	    call strcpy (iraffile, IRAFNAME(fits), SZ_FNAME)
	else if (NAXIS(fits) == 0 && len_name == 0)
	    call strcpy (IRAFNAME(fits), iraffile, SZ_FNAME)
		 

	if (fitsfile[1] == EOS) { 
	   call printf("%*t  ")
	   call pargi(itab)
	}
	call printf ("%-19.19s ")
	    call pargstr(iraffile)

	do k = 1, NAXIS(fits) {
	   call printf("%-5.5d")
	       call pargi(NAXISN(fits,k))
	}
	

	k= 6 + (2-NAXIS(fits))*5  # 6, 11, 16
#	if (NAXIS(fits) == 0) 
#	   call printf("%16t")
#	if (NAXIS(fits) == 1) 
#	   call printf("%11t")
#	if (NAXIS(fits) == 2) 
#	   call printf("%6t")

	call printf("%*t")
	  call pargi(k)
	call printf("%-2.2d %-10.10s ") 
	    call pargi(BITPIX(fits))
	    call pargstr(DATE(fits))
	   
	if (tape == YES)
	   call printf ("%-26.26s")
	else
	   call printf ("%-15.15s")
	call pargstr (OBJECT(fits))

	call printf("\n")

	# See if fitsfile and/or iraffile are too long and put
	# in the following line
	do k = 1, 3 {
	   if (strlen(fitsfile) > 16*k) {
	      call printf ("%-16.16s ")
		  call pargstr (fitsfile[16*k+1])
	      if (strlen(Memc[bf]) > 19*k) {
		 call printf ("%-19.19s ")
		     call pargstr (Memc[bf+19*k])
	      }
	      call printf ("\n")
	   } else
	      if (strlen(Memc[bf]) > 19*k) {
		 call printf ("%*t %-19.19s \n")
		     call pargi (itab)
		     call pargstr (Memc[bf+19*k])
	      }
	}
	call sfree(sp)
end

# PRTAB_INFO -- Procedure to print table information to STDOUT

procedure prtab_info (fits, ext, seqfile, ncols)

pointer	fits			#primary header descriptor
pointer	ext			#extension descriptor
char	seqfile[SZ_FNAME]	#sequential table filename
int	ncols			#NUmber of columns in table.

int	strlen(), k, fnldir(), fnroot(), fnextn(), chk_tabname()
pointer sp, bf, rr, sf
char	extn[SZ_FNAME]
int	itab
bool    trl

include "rfits.com"

begin
	call smark(sp)
	call salloc (bf, SZ_FNAME, TY_CHAR)
	call salloc (sf, SZ_FNAME, TY_CHAR)

	# set tab value for tape or disk file
	itab = 17
	if (tape == YES)
	   itab = 5
	   
	trl = false
	if (TAB_TYPE(ext) == TXT_FILE)
	   trl = true
	# save 'seqfile' 
	call strcpy (seqfile, Memc[sf], SZ_FNAME)
	if (old_name == YES) {
	   call salloc (rr, SZ_FNAME, TY_CHAR)
	   if (chk_tabname (IRAFNAME(fits)) == YES) {
	       call gen_fname (IRAFNAME(fits), EXTNAME(ext), 
			       Memc[rr], extn, SZ_FNAME)

	   } else if (strlen (EXTNAME(ext)) != 0) {
	       call pesc_dash(EXTNAME(ext))
	       k = fnroot (EXTNAME(ext), Memc[rr], SZ_FNAME)
	       call cesc_dash(Memc[rr])
	       k = fnextn (EXTNAME(ext), extn, SZ_FNAME)

 	   } else {
	       call pesc_dash(Memc[sf])
	       k = fnroot (Memc[sf], Memc[rr], SZ_FNAME)
	       k = fnextn (Memc[sf], extn, SZ_FNAME)
	       call cesc_dash(Memc[sf])
	   }

	   # a 'trl' file will always have the extension.
	   if (extn[1] == EOS) 
	      call strcpy ("tab", extn, SZ_FNAME)
	   k = fnldir (Memc[sf], Memc[bf], SZ_FNAME)
	   call strcat (Memc[rr], Memc[bf], SZ_FNAME)
	   call iki_mkfname (Memc[bf], extn, EXTNAME(ext), SZ_FNAME)

	   iferr (call frename (seqfile, EXTNAME(ext) )) {
		  call printf ("Cannot rename %s to %s\n")
	  	       call pargstr (Memc[sf])
		       call pargstr (EXTNAME(ext))
	   } else if (short_header == YES) {
	          call printf ("%*t %-19.19s ")
		     call pargi (itab)
	  	     call pargstr (EXTNAME(ext))
		  call strcpy (EXTNAME(ext), Memc[sf], SZ_FNAME)
	   }
	} else {
	    if (short_header == YES) {
	       call printf ("%*t %-19.19s ")
		    call pargi (itab)
         	    call pargstr (Memc[sf])
	    }
	}

	if (short_header == YES) {

	   call printf ("%-4.4d %-5.5d")
		call pargi (EXT_ROWLEN(ext))
		call pargi (EXT_NROWS(ext))
	   call printf(" Ncols=%3d ")
		call pargi (ncols)

	   call printf("\n")
	}
	call sfree(sp)
end
define  X_BITPIX          Memi[$1]
define  X_NAXIS           Memi[$1+2]    
define  X_PCOUNT          Memi[$1+3]    
define  X_GCOUNT          Memi[$1+4]    
define  X_NAXISN          Memi[$1+5+$2-1]

# SKIP_EXTENSIONS -- Procedure to skip a number of extensions in the
#		     FITS file.
procedure skip_extensions (n_extensions, fits_fd)
int	n_extensions		# Number of extensions to skip
int	fits_fd			# FITS file descriptor

pointer xt,sp
char	card[LEN_CARD]
int	i, rft_init_read_pixels(), get_min_info()
int	stat, k

include "rfits.com"
begin
        call smark(sp)
	call salloc (xt, 103, TY_INT)   # 4+ naxisn could be up to 99

	do k = 1, n_extensions - 1 {
	   i = rft_init_read_pixels (len_record, FITS_BYTE, LSBF, TY_CHAR)
	   repeat {
	      stat = get_min_info(fits_fd, card,xt)
	      if (stat == EOF) {
#		 call printf ("EOF encountered\n")
		 return
	      }
           } until (stat == YES)   # stat == YES if END card encountered.
	   call skip_xdata (fits_fd, xt)
        }
	call sfree(sp)
end
int procedure get_min_info (fits_fd, card, xt)
int	fits_fd
char    card[ARB]
pointer xt

int	strmatch(), rft_read_pixels()
int	nchar, i, ctoi(), k, j, stat, nrec

begin
	stat = rft_read_pixels (fits_fd, card, LEN_CARD, nrec, 1)
	if (stat == EOF) return (EOF)

	i = COL_VALUE
	if (strmatch (card, "^END     ") != 0) {
	   return(YES)
	} else if (strmatch (card, "^BITPIX  ") != 0) {
	   nchar = ctoi (card, i, X_BITPIX(xt))	
	} else if (strmatch (card, "^NAXIS   ") != 0) {
	   nchar = ctoi (card, i, X_NAXIS(xt))	
	} else if (strmatch (card, "^PCOUNT  ") != 0) {
	   nchar = ctoi (card, i, X_PCOUNT(xt))	
	} else if (strmatch (card, "^GCOUNT  ") != 0) {
	   nchar = ctoi (card, i, X_GCOUNT(xt))	
        } else if (strmatch (card, "^NAXIS") != 0) {
	   k = strmatch (card, "^NAXIS")
	   nchar = ctoi (card, k, j)
	   nchar = ctoi (card, i, X_NAXISN(xt, j))
	}
	return(NO)
end
include <mach.h>
include <fset.h>
define  NB_DOUBLE   64

procedure skip_xdata (fits_fd, xt)

int	fits_fd		# FITS file descriptor
pointer	xt		# FITS data structure

int	i, npix, npix_record, blksize, nrec
long	nlines, il, pc
pointer	tempbuf,sp,pp

int	fstati(), bitpix, gc
data	tempbuf /NULL/
int	rft_init_read_pixels(), rft_read_pixels()
errchk	rft_init_read_pixels, rft_read_pixels

include "rfits.com"

begin
	if (X_NAXIS(xt) == 0) {
	    return
	}

	npix = X_NAXISN(xt, 1)
	nlines = 1
	do i = 2, X_NAXIS(xt)
	    nlines = nlines * X_NAXISN(xt, i)

	# FITS data is converted to type  LONG.  If BITPIX is not one
	# of the MII types then rft_read_pixels returns an ERROR.

	bitpix = abs (X_BITPIX(xt))
	if (tempbuf != NULL)
	    call mfree (tempbuf, TY_LONG)
	if (bitpix != NB_DOUBLE)
	   call malloc (tempbuf, npix, TY_LONG)
	else
	   call malloc (tempbuf, npix*2, TY_LONG)

	npix_record = len_record * FITS_BYTE / bitpix
	i = rft_init_read_pixels (npix_record, bitpix, LSBF, TY_LONG)
	blksize = fstati (fits_fd, F_SZBBLK)
	if (mod (blksize, 2880) == 0)
	    blksize = blksize / 2880
	else
	    blksize = 1

        gc = X_GCOUNT(xt)
	pc = X_PCOUNT(xt)
	if (pc > 0) {
	   call smark(sp)
	   call salloc (pp, pc, TY_LONG)
	}
	repeat {
	   # If there are PCOUNT pixel of GROUP info then read that.
	   # This is for Small Group of data format.
	   if (pc > 0) {
	      if (rft_read_pixels (fits_fd, Meml[pp], pc,
	         nrec, blksize) != pc)
	         call printf ("Error reading FITS data\n")
	   }

	   do il = 1, nlines {

	       # Read in image line
	       i = rft_read_pixels (fits_fd, Meml[tempbuf], npix,
	           nrec, blksize)
	       if (i != npix)
		call printf ("Error reading FITS data\n")

	   }
	   gc = gc - 1
	} until (gc < 1)
	if (pc > 0)
	   call sfree (sp)
end
#SKIP_MDATA -- skip main FITS data unit

procedure skip_mdata(fits_fd, fits)
int	fits_fd
pointer fits

pointer xt,sp
int	i

begin
        call smark(sp)
	call salloc (xt, 103, TY_INT)   # 4+ naxisn could be up to 99

	X_BITPIX(xt) = BITPIX(fits)
	X_NAXIS(xt) = NAXIS(fits)
	do i = 1, NAXIS(fits)
	  X_NAXISN(xt, i) = NAXISN(fits, i)

	X_GCOUNT(xt) = 1
	X_PCOUNT(xt) = 0

	call skip_xdata (fits_fd, xt)

	call sfree(sp)
end
procedure pr_files (iraffile, fitsfile)
char	iraffile[SZ_FNAME], fitsfile[ARB]

pointer	sp,pp
int	stridx(), len,k,id1,id2
include "rfits.com"
begin
	call smark(sp)
	call salloc(pp, SZ_FNAME, TY_CHAR)
	if (long_header == YES) {
	   call printf ("\n**** FILE: %s\n ")
		call pargstr (iraffile)
	}
	if (short_header == YES) {
	   if (tape == YES) {
	      # Get the file number from the 'fitsfile' name.
	      id1 = stridx ("[", fitsfile) + 1
	      id2 = stridx ("]", fitsfile) - 1
	      len = id2-id1+1
	      do k = 0, len-1
		 Memc[pp+k] = fitsfile[id1+k]
	      Memc[pp+len] = EOS
	      call printf ("%-5.5s")
		call pargstr (Memc[pp])
	   } else {
	      call printf("%-16.16s ")
		call pargstr (fitsfile)
	   }
	}
	call sfree(sp)
end
#Procedure to convert very naively from FITS BIPIX values  to IM_PIXTYPE.
# This is mainly when we have a FITS file with NAXIS=0, for other cases
# BSCALE and NZERO values needs to be taken into consideration.

int procedure bitpix_to_imtype(fits)

pointer fits
int	imtype, strcmp()

begin
	switch (BITPIX(fits)) {
	case  8:
	   imtype = TY_SHORT              # Convert from byte to short
	case 16:
	   if (strcmp (FITSTYPE(fits), "USHORT") == 0) 
	       imtype = TY_USHORT
	   else
	       imtype = TY_SHORT
	case 32:
	   imtype = TY_LONG
	case -32:
	   imtype = TY_REAL
	case -64:
	   imtype = TY_DOUBLE
	default:
	   imtype = TY_SHORT
	}
	return (imtype)
end


define  HDR_TEMPLATE    "dev$pix.hhh"   # used by fmkcopy to create new header

# WR_HEADER -- Procedure to practically copy the imio user area (UA) to an
#	       output GEIS image header. This routine will be called only
#	       when IM_NDIM(im) is zero; which is the same as when the input
#	       FITS image has NAXIS==0.
# NZ	Nov 28 1995

procedure wr_header(im, fits)

pointer	im			# image descriptor

pointer	sp, fname, lbuf, fits
int	in, out, junk, width, getline()

bool	fnullfile()
int	stropen(), strcmp(), open(), protect(), strlen() #ditto-dlb
errchk	fmkcopy, open, stropen, fprintf

begin
	if (fnullfile (IM_HDRFILE(im)))
	    return

	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	# Open a new header file with a unique, temporary name.  Make a copy
	# of the template file rather than of the old header file.  Since
	# we also block header lines out to 80 chars automatically, this
	# means that we can read any old text file but will always generate
	# a new header file of the standard type when the header is updated.

	call mktemp (IM_HDRFILE(im), Memc[fname], SZ_FNAME)
	call fmkcopy (HDR_TEMPLATE, Memc[fname])
	out = open (Memc[fname], APPEND, TEXT_FILE)

	# Write out the standard, reserved header parameters.

	call fprintf (out, "SIMPLE  =%21s /%81t\n")
	    call pargstr ("F")
	call fprintf (out, "BITPIX  =%21d /%81t\n")
	    call pargi (BITPIX(fits))

	iferr(call imgstr (im, "DATATYPE",Memc[lbuf], SZ_LINE)) {
	  switch(BITPIX(fits)) {
	  case 8,16:
	      if (strcmp (FITSTYPE(fits), "USHORT") == 0) 
		  call strcpy("UNSIGNED*2", Memc[lbuf], SZ_LINE)
	      else
		  call strcpy("INTEGER*2", Memc[lbuf], SZ_LINE)
	  case 32:
	     call strcpy("INTEGER*4", Memc[lbuf], SZ_LINE)
	  case -32:
	     call strcpy("REAL*4", Memc[lbuf], SZ_LINE)
	  case -64:
	     call strcpy("REAL*8", Memc[lbuf], SZ_LINE)
	  default:
	     call strcpy("INTEGER*2", Memc[lbuf], SZ_LINE)
	  }
	}  
	call fprintf (out, "DATATYPE= '%*.*s'%32t/%81t\n")
	   width = max(8, strlen(Memc[lbuf]))
	   call pargi (-width) # force left-justified field
	   call pargi (width)
	   call pargstr (Memc[lbuf])


	call fprintf (out, "NAXIS   =%21d /%81t\n")
	    call pargi (IM_NDIM(im))


	in = stropen (Memc[IM_USERAREA(im)], ARB, READ_ONLY)
	while (getline(in, Memc[lbuf]) != EOF)
	     call putline (out, Memc[lbuf])
	call close (in)

	# End of FITS header.
	call fprintf (out, "END%81t\n")
	call close (out)

	# Replace the original header file with the new one, even if the
	# original header is a protected file.  Transfer any file protection
	# to the new file.

	if (IM_HFD(im) != NULL)
	    call close (IM_HFD(im))

	if (protect (IM_HDRFILE(im), QUERY_PROTECTION) == YES) {
	    iferr (junk = protect (IM_HDRFILE(im), REMOVE_PROTECTION))
		call erract (EA_ERROR)
	    iferr (junk = protect (Memc[fname], SET_PROTECTION))
		call erract (EA_ERROR)
	}

	iferr (call delete (IM_HDRFILE(im)))
	    call erract (EA_ERROR)
	iferr (call rename (Memc[fname], IM_HDRFILE(im)))
	    call erract (EA_ERROR)

        # Free all buffer space allocated by IMIO.
	call imrmbufs (im)
	call mfree (im, TY_STRUCT)

end
