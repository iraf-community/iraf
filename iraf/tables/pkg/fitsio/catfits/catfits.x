# Copyright restrictions apply - see tables$copyright.tables 
# 
# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include <error.h>
include <fset.h>
include "../stwfits/dfits.h"
include "catf.h"

define	MAX_RANGES	100

# CATFITS -- Procedure to read catalog a fits file either from tape
# disk. The user can select one line output or the entire header. If
# line output is chosen, then a log_file can be created. The one line
# information is user selectable with the ascii file in the string
# 'format_file'.

procedure t_catfits()

char	infile[SZ_FNAME]	# fits file
char	in_fname[SZ_FNAME]	# input file name
char	file_list[SZ_LINE]	# list of tape files
char	format_file[SZ_FNAME]	# input file name with format information
				# for one line output per fits file.
char    log_file[SZ_FNAME]	# Name of output log file

pointer	list, fi
int	lenlist, junk
int	range[MAX_RANGES*2+1], nfiles, file_number, stat, fits_record

bool	clgetb()
int	btoi(), decode_ranges(), get_next_number(), fntgfnb()
int	fntlenb(), read_tape_only(), mtfile(), open(), ext_number
pointer	fntopnb(), fits_fd, mtopen(), mtneedfileno(), get_ext_number()
data	fits_record/2880/
include	"catfits.com"
include "../stwfits/dfits.com"

begin
	# Set up the standard output to flush on a newline
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get RFITS parameters.
	call clgstr ("fits_file", infile, SZ_FNAME)
	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	print_ext = btoi (clgetb ("ext_print"))
	len_record = fits_record

	if (long_header ==  YES)
	   short_header = NO
	# Compute the number of files to be converted
	if (mtfile(infile) == YES)  {
	    list = NULL
            if (mtneedfileno (infile) == YES)
	        call clgstr ("file_list", file_list, SZ_LINE)
	    else
	        call strcpy ("1", file_list, SZ_LINE)
	} else {
	    ext_number = get_ext_number (infile)
	    list = fntopnb (infile, YES)
	    lenlist = fntlenb (list)
	    call sprintf  (file_list, SZ_LINE, "1-%d")
		call pargi (lenlist)
	}


        # Get format file name if short_header is selected
    	log_fd = 0
        if (short_header == YES) {
           call clgstr ("format_file", format_file, SZ_FNAME)
           call clgstr ("log_file", log_file, SZ_FNAME)
           if (log_file[1] != EOS)
              log_fd = open (log_file, NEW_FILE, TEXT_FILE)
	   if (format_file[1] == EOS)
	      call strcpy ("tables$pkg/fitsio/format.mip", format_file,
			        SZ_FNAME)
           call dfread_formats (format_file)
	   # Print the keywords in format_file as one line title.
           if (nkeywords > 0)
              call print_titles
        }

	# Decode the ranges
	if (decode_ranges (file_list, range, MAX_RANGES, nfiles) == ERR)
	    call error (1, "T_RFITS: Illegal file number list")

	# Allocate memory to handle the different type of outputs
	  if (short_header == YES)
	     call calloc (fi, LEN_SINFO+(MAX_TABLE*SZ_OBJECT)/SZ_STRUCT,
			TY_STRUCT)
	  else if (long_header == YES)
	     call calloc (fi, LEN_SINFO, TY_STRUCT)
	EXT_NUMBER(fi) = ext_number
	# Read successive FITS files, convert and write into a numbered
	# succession of output IRAF files.

	# See if extension number has been specified.
	file_number = 0
	while (get_next_number (range, file_number) != EOF) {

	    # Get input file name
	    if (list != NULL) {
                if (fntgfnb (list, in_fname, SZ_FNAME) == EOF)
		    call error (0, "t_catf: Error reading input file name")
	    } else {
                if (mtneedfileno (infile) == YES)
		   call mtfname (infile, file_number, in_fname, SZ_FNAME)
	        else{
		   call mtparse (infile, junk,0,file_number,junk,junk,0)
		   call strcpy (infile, in_fname, SZ_FNAME)
		}
	    }

	    iferr (fits_fd = mtopen(in_fname, READ_ONLY, 0)) { 
		call eprintf("ERROR: cannot open input fits file: %s\n")
		     call pargstr(in_fname)
		break
	    }
	    stat = read_tape_only (fi, fits_fd, in_fname, file_number)
	    call close (fits_fd)
	    if (stat == EOF)
		break
	    EXT_NUMBER(fi) = -1
	}

	if (list != NULL)
	    call fntclsb (list) 
	
	# Append to the log_file an explanation of the keywords in
	# format_file.
	if (short_header == YES && log_file[1] != EOS)
	   call cat_explanation (fi)

	call mfree (fi, TY_STRUCT)
	call close (log_fd)
end

int procedure get_ext_number (infile)
char infile[ARB]
int	dn,ipos,junk,strlen(),ctoi(),strldx()
int	ext_number 
begin
	ext_number = -1
	call trimh(infile)
	dn = strlen(infile)
	if (infile[dn] == ']') {
	   ipos = strldx ("[", infile)
	   if (dn - ipos > 5)
	      call error (13,"Bad Fits Extension Number")
	   ipos = ipos+1
	   dn = ipos - 1
	   if (ipos != 1) {
	      junk = ctoi(infile,ipos,ext_number)
	      infile[dn]=EOS
	   }
	}
	return(ext_number)
end
