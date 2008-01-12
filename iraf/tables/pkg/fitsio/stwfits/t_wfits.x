# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include <error.h>
include <fset.h>
include <syserr.h>
include <clset.h>
include "wfits.h"
include "dfits.h"

# T_WFITS -- This procedure converts a series of IRAF image files to
# FITS image files.
# This version of the writer supports the standard FITS table format.
# if the keyword "extensions" is set then the main image FITS file can
# have one or more tables following in the same output file. The same
# if no iraf image is specified has input file.
#
# JULY 1991. Add support for BINARY tables. Nelson Zarate
# June 1992. Allow for output file to be  a directory. THe output FITS
#	     files are the root of the input file plus the 'fit' extension.
#	     If the original extension is not 'imh' or 'hhh' then the 'fit'
#	     is appended to the original filename. NZ
# Dec 10 92  Change SZ_FNAME to SZ_EXTN in this routine at the line
#            'junk =  fnextn (fits_files, oextn, SZ_EXTN)'
#            
#	     Do not allow binary table with gftoxdim = yes.
# Nov 5 93   Correct problem when using extension+; that the output
#	     fits had a 001 appended to the root when only one fits
#	     file was created.


procedure t_wfits()

bool	newtape
char	fits_files[SZ_FNAME], in_fname[SZ_FNAME], out_fname[SZ_FNAME]
char	log_file[SZ_FNAME], format_file[SZ_FNAME]
int	list, nfiles, fits_record, open(), clstati()

char	line[SZ_LINE], iextn[SZ_EXTN]
bool	clgetb(), table
double	clgetd()
int	imtopenp(), imtlen(), strlen(), wft_get_bitpix(), clgeti(), imtgetim()
int	mtfile(), strmatch(), btoi(), fnextn(), strcmp()
int	save_sdasmg, save_ieee, tape, save_scale
int	fntopnb(), flist, fntgfnb() 
int	fntlenb(), nimages, strldxs(), access(), strncmp()
int     nch, fits_fd, nerrors, ip

data	fits_record/2880/
string	version "STSDAS-FITSIO 21-Feb-1996"
include "wfits.com"
include "dfits.com"
define  error_ 99

begin
	call iki_init
	call fseti(STDOUT,F_FLUSHNL, YES)

        # Open iraf_files template and determine number of files in list
	list = imtopenp ("iraf_files")
	nimages = imtlen (list)

	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	bitpix = wft_get_bitpix (clgeti ("bitpix"))
	extensions = btoi (clgetb ("extensions"))
#	def_fmt = btoi (clgetb ("def_tab_precision"))
	# Since we have TDISP now, we can set the above to YES all
	# the time. (Jan 1995)
	def_fmt = YES
	bintable = btoi (clgetb ("binary_tables"))

	# Set the extension type; can be TABLE, BINTABLE or IMAGE
	ext_type = NULL
	
	# The user interface has been changed (Feb 1990). Now we will not
	# need the user parameter 'allgroups' (the program needs it although),
	# the user parameter  'sdasmgcv' has been change to 'gftoxdim' for
	# "Group Format To Xtra Dimension".
	# We will still use the the variable name sdasmgcv 
	# (SDAS Multigroup ConVersion)
	sdasmgcv = btoi (clgetb ("gftoxdim"))
	if (sdasmgcv == 0) 
	   sdasmgcv = -1
	allgroups = NO
	
	# If extensions is set see if sdasmgcv is also.
	if (sdasmgcv > 0 && extensions == YES) {
	   call eprintf ("Warning: Do not set \'extensions\' flag and\n")
	   call eprintf ("        \'gftoxdim\' flag together\n")
	   goto error_
	}

	ieee = btoi (clgetb ("ieee"))

	# Force the recomputation of datamin and datamax?
#	force_minmax = btoi (clgetb ("force_minmax"))

	# Allow only one kind of output
	if (long_header == YES)
	   short_header = NO

	# Get length of record in FITS bytes
	len_record = fits_record
	blkfac = clgeti ("blocking_factor")

	# Get scaling parameters
	scale = btoi (clgetb ("scale"))
	bscale = 1.0d0
	bzero = 0.0d0
	if (scale == YES) {
	    autoscale = btoi (clgetb ("autoscale"))
	    if (autoscale == NO) {
		bscale = clgetd ("bscale")
		bzero = clgetd ("bzero")
	    }
	} else {
	    autoscale = NO
	    bscale = 1.0d0
	    bzero = 0.0d0
	}

	# Get output file name. If no tape file number is given for output,
	# the user is asked if the tape is blank or contains data.
	# If the tape is blank output begins at BOT, otherwise at EOT

	tape = NO
	call clgstr ("fits_files", fits_files, SZ_FNAME)
	tape = mtfile (fits_files)
	if (tape == YES) {
	   if (fits_files[strlen(fits_files)] != ']') {
	      newtape = clgetb ("newtape")
	      if (newtape) {
		 call sprintf (fits_files[strlen(fits_files) + 1],
		              SZ_FNAME, "%s")
		      call pargstr ("[1]")
	      } else {
		call sprintf (fits_files[strlen(fits_files) + 1],
		              SZ_FNAME, "%s")
		     call pargstr ("[EOT]")
	      }
	   } else
	      newtape = false
	} else {
	   flist = fntopnb (fits_files, NO)
	   nfiles = fntlenb (flist)
	   if ((nfiles >= 1) && (nfiles != nimages) && fits_files[1] == '@') {
	      call eprintf(
		"T_WFITS: Input and output lists are not the same length\n")
	      goto error_
	   }
	}


        # Print a header if short_header is selected
    	log_fd = 0
        if (short_header == YES) {
           call clgstr ("format_file", format_file, SZ_FNAME)
	   call xt_stripwhite (format_file)
           call clgstr ("log_file", log_file, SZ_FNAME)
	   call xt_stripwhite (log_file)
	   if (strcmp(log_file, "none") != 0)
	      log_fd = open (log_file, NEW_FILE, TEXT_FILE)
	   if (strcmp(format_file, "default") == 0)
	      call strcpy ("tables$pkg/fitsio/format.mip", format_file,
		          SZ_FNAME)
           call dfread_formats (format_file)
           if (nkeywords > 0)
              call print_titles
        }

	# Save value of sdasmgcv since this will get reset for the
	# current input file if there is no gpb for example.
	# Also save the 'ieee' value for if the input image is 
	# of type integer it will get reset.
	# These variables are stored in a common area, to be used
	# throughout the program.
	save_sdasmg = sdasmgcv
	save_ieee = ieee
	save_scale = scale

	# Loop through the list of output files.

	file_number = 1
	nerrors = 0		# Setup the number of error on this loop.
	while (imtgetim(list, in_fname, SZ_FNAME) != EOF) {

	    sdasmgcv = save_sdasmg
	    ieee = save_ieee
	    scale = save_scale
	       
	    # print id string
	    if (long_header == YES) {
		if (tape == YES) {
		   call printf ("File %d: %s     ")
		        call pargi (file_number)
		        call pargstr (in_fname)
		} else {
		   call printf ("%s   ")
		        call pargstr (in_fname)
		}
	    }

	    # Make output filename. If single file output to disk, use name
	    # fits_file. If multiple file output to disk, the file number
	    # is added to the output file name.

	    if (ext_type == NULL || extensions == NO) {
	       first_time = YES
	       if (nfiles >= 1 && fits_files[1] == '@') {
		  if (fntgfnb (flist, out_fname, SZ_FNAME) == EOF) {
		     call eprintf ("Error reading output file name\n")
		     goto error_
	          }
	       } else
	       call mk_output_name (tape, nimages, file_number, in_fname,
				fits_files, out_fname)
	    }

    	    nch = fnextn (in_fname, iextn, SZ_EXTN)

	    # If file extension is empty  we are going to assume 
	    # is an image.  Everything else could be a  table.
#         Needs to see if extn == cmh then there is no data file 'cmd', hence
#         is an ascii file.
	    table = true
	    if (strncmp("cmh",iextn,3) == 0) {
	       call strcpy (in_fname, line, SZ_LINE)
	       ip = strldxs("cmh", line)
	       line[ip] = 'd'
	       if (access(line,0,0) == YES)  # Is an image
		  table = false
	    } else if (strmatch(iextn,"??h") > 0)
	       table = false
	    if (nch == 0 || !table ) {
		iferr (call wft_write_fitz (in_fname, fits_files, out_fname,
					    fits_fd)) {
		   call errget(line,SZ_LINE)
		   call eprintf ("\007ERROR: %s\n")
			call pargstr (line)
		   call put_in_log (line)
      	           if (nimages == file_number ) goto error_
		   nerrors = nerrors+ 1      # accumulate the errors
		   if (nerrors > 10)	{        # and exit after 10.
			 call eprintf(
			    "*** ERRROR: more than 10 errors, exiting...\n")
			 goto error_
		   }
		   file_number = file_number - 1
	        }
	    } else {
		iferr (call tab_write_fitz (in_fname, out_fname, fits_fd)) {
		   call errget(line,SZ_LINE)
		   call eprintf ("\007ERROR: %s\n")
		      call pargstr (line)
		   call put_in_log (line)
                   if (nimages == file_number ) goto error_
		      nerrors = nerrors+ 1      # accumulate the errors
		      if (nerrors > 10)	{        # and exit after 10.
			 call eprintf(
			    "*** ERRROR: more than 10 errors, exiting...\n")
			 goto error_
		      }
		}
	    }
	    if (ext_type == NULL || extensions == NO)
	       file_number = file_number + 1

            if (extensions == YES)
                first_time = NO
	}
	if (extensions == YES) {
	   call close(fits_fd)
	   # Rename the output fits files from 'name001' to 'name'.
	   if (file_number == 1 && nimages > 1 && tape == NO) {
	      if (short_header == YES) {
		  call printf("  ... %s  renamed to %s\n")
		       call pargstr(out_fname)
		       call pargstr(fits_files)
	      }
	      call frename (out_fname, fits_files)
	   }
	}
        if (log_fd != 0) {
 	   call prt_par(log_fd, version, fits_files, format_file, 
		log_file, bitpix, blkfac, save_sdasmg, allgroups, 
		save_ieee, save_scale, bscale, bzero, autoscale, 
		newtape)
#		newtape, force_minmax)
	   call close (log_fd)
	}
	call imtclose (list)
	return

error_
	call flush(STDOUT)
	call flush(STDERR)
	if (clstati(CL_PRTYPE) == PR_HOST)
	   call rf_exit(123)

#	call erract(EA_FATAL)
end

# MK_OUTPUT_NAME -- Make output filename based on the input name information,
#		    and whether output goes to a tape.

procedure mk_output_name (tape, nfiles, file_number, in_fname,
				fits_files, out_fname)
int 	tape			# i: (YES, NO) is output goes to a tape.
int	nfiles			# i: Number of files resolve by template expand.
int 	file_number		# i: Current number of output files.
char	in_fname[SZ_FNAME]	# i: input filename.
char	fits_files[SZ_FNAME]	# i: outout file template or directory.
char    out_fname[SZ_FNAME]	# o: output FITS name.

char	iextn[SZ_EXTN], oextn[SZ_EXTN], root[SZ_FNAME], temp[SZ_FNAME]
char	dirname[SZ_FNAME], ch[1]
int	nch, isdir, ip, junk, isdirectory()
int	fnextn(), strmatch(), fnroot(), stridxs(), strcmp()

begin

    	nch = fnextn (in_fname, iextn, SZ_EXTN)
	# If no extension is supplied on the input file,
	# force to hhh.
	if (nch == 0) {
	   call strcat (".hhh", in_fname, SZ_FNAME)
	   call strcpy ("hhh", iextn, 3)
	}

	if (tape == YES) {
	   if (file_number >= 2 && strmatch(fits_files,"[EOT]") == 0) {
	      call strcpy ("[", ch, 1)
	      call sprintf (fits_files[stridxs(ch, fits_files)], SZ_FNAME, "%s")
	           call pargstr ("[EOT]")
	   }
	   call strcpy (fits_files, out_fname, SZ_FNAME)
	} else {
	   isdir = NO
	   if (isdirectory (fits_files, dirname, SZ_FNAME) > 0) {
	      if (strcmp ("./", dirname) == 0)
	      dirname[1] = EOS
	      isdir = YES
	   }
	   if (isdir == YES) { # Now form "file.fit" or "file.c1h.fit"
	      call strcpy ("fit", oextn, SZ_EXTN)
              junk = fnroot (in_fname, root, SZ_FNAME)
              if (strcmp("imh", iextn) == 0 ||
                 	strcmp("hhh", iextn) == 0) {
                 call iki_mkfname(root, oextn, temp, SZ_FNAME)
                 call strcpy (dirname, out_fname, SZ_FNAME)
                 call strcat (temp, out_fname,  SZ_FNAME)
              } else {
                 call iki_mkfname(root, iextn, temp, SZ_FNAME)
                 call strcpy (dirname, out_fname, SZ_FNAME)
                 call strcat (temp, out_fname,  SZ_FNAME)
                 call iki_mkfname(out_fname, oextn, out_fname, SZ_FNAME)
              }
           } else { # output file is specified.
	      call zfnbrk (fits_files, ip, junk)
	      call strcpy (fits_files, root, junk-1)
	      junk =  fnextn (fits_files, oextn, SZ_EXTN)
              call strcpy (fits_files, out_fname, SZ_FNAME)
              if (nfiles > 1) {
                 call sprintf (out_fname, SZ_FNAME, "%s%03d")
                       call pargstr(root)
                       call pargi (file_number)
                 call iki_mkfname(out_fname, oextn, out_fname, SZ_FNAME)
              }
           }
	}

end
