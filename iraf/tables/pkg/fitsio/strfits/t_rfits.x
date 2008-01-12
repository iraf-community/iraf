include <error.h>
include <fset.h>
include <ctype.h>
include "rfits.h"

define	MAX_RANGES	100
define  SZ_STRTYPE      8

# RFITS -- Read FITS format data.
#
# JULY 1991. Add support for BINARY tables.  Nelson Zarate
# AUGUST 1991. Add support for LOGICAL datatype on FITS files.
# September 1993. Add support for IMAGE extension and BYTE datatype for 
#		  binary tables.
# March 1994.     Full support to create 'imh' images.

procedure t_rfits()

char	infile[SZ_FNAME]		# fits file
char	outfile[SZ_FNAME]		# IRAF file
char	in_fname[SZ_FNAME]		# input file name
char	out_fname[SZ_FNAME]		# output file name
char	file_list[SZ_LINE]		# list of tape files
char	template[SZ_FNAME]		# template file
char	cluster[SZ_FNAME], tmp[SZ_FNAME]
char    root[SZ_FNAME], extn[SZ_EXTN], extn2[SZ_EXTN]

pointer	list, outlist
int	range[MAX_RANGES*2+1], len_inlist, len_outlist, file_number
int	offset, stat, fits_record, junk

bool	clgetb()
int	rft_get_image_type(), clgeti(), mtfile(), strlen(), btoi()
int	rft_read_fitz(), decode_ranges(), get_next_number(), fntgfnb()
int	fntlenb(), save_old_name, fnldir(), strcmp()
int	ipos, dn, save_gkey, save_xdim, fnroot(), fnextn()
pointer	fntopnb()
real	clgetr()
char    str_type[SZ_STRTYPE]
int	cl_index, cl_size, xdimtogf, ext_number, lendir, strldx(),ctoi()
data	fits_record/2880/
include	"rfits.com"

begin
	call post_host_errhandler()
	# Set up the standard output to flush on a newline
	call iki_init
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get RFITS parameters.
	call clgstr ("fits_files", infile, SZ_FNAME)
	call clgstr ("iraf_files", outfile, SZ_FNAME)
	call clgstr ("template", template, SZ_FNAME)
	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	call clgstr ("datatype", str_type, SZ_FNAME)
	blank = clgetr ("blank")
	scale = btoi (clgetb ("scale"))
	xdimtogf = btoi (clgetb ("xdimtogf"))
	old_name = btoi (clgetb ("oldirafname"))
	force = btoi (clgetb ("force"))
	offset = clgeti ("offset")

	len_record = fits_record
	data_type = rft_get_image_type (str_type)
	if (strcmp(template, "none") == 0)
	   template[1] = EOS

	# Allow only one type of output
	if (short_header == YES)
	   long_header = NO
	if (short_header == YES && long_header == YES)
	   short_header = NO


	# Ext_number indicates the extension number we want to read; zero (0)
	# is for the main FITS unit only, 1 for the 1st extension,etc.
	#
	ext_number = -1

	# Compute the number of files to be converted
	tape = mtfile (infile)
	if (tape == YES)  {
	    list = NULL
	    if (infile[strlen(infile)] != ']')
	        call clgstr ("file_list", file_list, SZ_LINE)
	    else
	        call strcpy ("1", file_list, SZ_LINE)
	    if (short_header == YES) {
	       call printf ("FILE# IRAFNAME            Dimensions    ")
	       call printf (" BP   DATE   OBJECT\n")
	    }
	} else {
	    # See if the last character is the end of FITS extension
	    # number spec.
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
	    list = fntopnb (infile, YES)
	    len_inlist = fntlenb (list)
	    if (len_inlist > 0) {
	        call sprintf  (file_list, SZ_LINE, "1-%d")
		    call pargi (len_inlist)
	    } else
	        call sprintf  (file_list, SZ_LINE, "0")
	    if (short_header == YES) {
	       call printf ("Fits_file        IRAFNAME           ")
	       call printf (" Dimensions     BP DATE       OBJECT\n")
	    }
	}
	# Decode the ranges
	if (decode_ranges (file_list, range, MAX_RANGES, len_inlist) == ERR)
	    call error (1, "T_RFITS: Illegal file number list")

	# Read successive FITS files, convert and write into a numbered
	# succession of output IRAF files.

        outlist = fntopnb (outfile, NO)
	len_outlist = fntlenb (outlist)

	if ((len_outlist > 1) && (len_outlist != len_inlist))
	   call error (0,
              "T_RFITS: Output and input lists have different lengths")

	# See if there is a group specification in the output geis file;
	# i.e. '[cl_index/cl_size]'.
	call gparse (outfile, cluster, root, extn,cl_size,cl_index)
	call strcpy (cluster, out_fname, SZ_FNAME)

	# Create output filename with multigroup syntax, disable old_name
	# parameter since we cannot rename the output GEIS file to whatever
	# the IRAFNAME FITS keyword has.
	if (cl_size > 1) {
	   old_name = NO
	   call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME, "[1/%d]")
		   call pargi (cl_size)
	   xdimtogf = NO   # reset the flag to avoid unwanted Warning message
        }

	# See if there is an extension.
	call see_extn (extn, template, out_fname, cluster)
	
	# Initialize the type of output file (gkey) for "imh" files.
	gkey = 0
	if (strcmp (extn, "imh") == 0)
	   gkey = IMH
	if (gkey == IMH && xdimtogf == YES)
	   call error (1, "You cannot select the 'imh' extension and xdimtogf")

	file_number = 0
	save_old_name = old_name
	save_gkey = gkey
	save_xdim = xdimtogf

	while (get_next_number (range, file_number) != EOF) {
	    gkey = save_gkey
	    old_name = save_old_name
	    xdimtogf = save_xdim
	    # Set the type of output file.
	    # For the explanation on the values see fits_read.x
	    if (gkey != IMH)
	       gkey = DEF_GPB
	    if (xdimtogf == YES)
	       gkey = TO_MG
	    # Get input file name
	    if (list != NULL) {
		junk = fntgfnb (list, in_fname, SZ_FNAME)
	    } else { 					#is a tape
	        call strcpy (infile, in_fname, SZ_FNAME)
	        if (infile[strlen(infile)] != ']') {
		    call sprintf (in_fname[strlen(in_fname)+1], SZ_FNAME,
		        "[%d]")
		        call pargi (file_number)
		}
	    }


	    # Get output file name
            if (cl_index > 1) {
	       template[1] = EOS
	       call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME, "[%d]")
		       call pargi (cl_index)
	    } else if (len_inlist > len_outlist && cl_size <= 0)  {
	       lendir = fnldir (out_fname, tmp, SZ_FNAME)
	       junk = fnroot (out_fname, tmp, SZ_FNAME)
	       junk = fnextn (out_fname, extn2, SZ_EXTN)

	       # Copy the directory prefix if any, since fnroot strip it off.
	       call strcpy (cluster, root, lendir)
	       call strcat (tmp, root, SZ_FNAME)
	       call sprintf (root[strlen(root)+1], SZ_FNAME, "%03d")
		      call pargi (file_number + offset)
	       call iki_mkfname (root, extn, out_fname, SZ_FNAME)
	     
	    } else if (len_outlist > 1) {
		old_name = NO  # disable for list
		if (fntgfnb (outlist, out_fname, SZ_FNAME) == EOF)
		   call error (0, "T_RFITS: Error reading output file name")
	        call gparse (out_fname, cluster, root, extn,cl_size,cl_index)
	        call see_extn (extn, template, out_fname, cluster)
	    }

	    if (len_inlist > 1 && cl_size > 0)
	       cl_index = cl_index + 1

	    # Convert FITS file to the output IRAF file.
	    # If EOT is reached then exit.
	    # If an error is detected then print a warning and continue with
	    # the next file.

	    iferr {
	       stat = rft_read_fitz (in_fname, template,
			      out_fname, ext_number)
	    } then {
 		call eprintf("AFTER RFT_READ_FITS\n")
#		call set_status
		call erract (EA_FATAL)
	    } else {
		if (stat == EOF) {
		  call eprintf("EOF encountered\n")
		  break
		}
	    }			
	    # Reset the original output name.
	    call strcpy (cluster, out_fname, SZ_FNAME)
	}

	if (list != NULL)
	    call fntclsb (list) 
end
include <clset.h>
# POST_HOST_ERRHANDLER -- Set an Onerror routine to be executed at task 
# termination time. If an error has occurred in the calling task, the 
# errcode number is pass to rf_exit routine, otherwise the errcode is OK
# and no 'rf_exit' routine is called. This will only work when the process
# is a PR_HOST type.
#
procedure post_host_errhandler()
extern pevh()
int clstati()
begin
	if (clstati(CL_PRTYPE) == PR_HOST)
	call onerror(pevh)
	end
procedure pevh(ic)
int ic
begin
	if (ic != 0){
	   call fio_cleanup(0)
	   call rf_exit(ic)
	}
end

define NTYPES 7
# RFT_GET_IMAGE_TYPE -- Convert a character to and IRAF image type.

int procedure rft_get_image_type (s)

char	s[ARB]
char    keyword[SZ_STRTYPE]
int	type, strcmp()

begin

	if (strcmp (keyword, "default") == 0) 
	   type = ERR
	else if (strcmp (keyword, "unsigned") == 0) 
	   type = TY_USHORT
	else if (strcmp (keyword, "short") == 0) 
	   type = TY_SHORT
	else if (strcmp (keyword, "integer") == 0) 
	   type = TY_INT
	else if (strcmp (keyword, "real") == 0) 
	   type = TY_REAL
	else if (strcmp (keyword, "double") == 0) 
	   type = TY_DOUBLE
	else if (strcmp (keyword, "complex") == 0) 
	   type = TY_COMPLEX
	else
	   type = ERR    # impossible case 

	return(type)
end

procedure gparse (infile, cluster, root, extn,cl_size,cl_index)
char 	infile[ARB], cluster[ARB],root[ARB],extn[ARB]
int	cl_size,cl_index

pointer sp,pp
int	junk, fnroot(), fnextn(), strlen()
int	clus_len

begin
	call smark(sp)
	call salloc(pp, SZ_FNAME, TY_CHAR)


	cl_size = -1
	cl_index = -1
	call imparse (infile, cluster, SZ_FNAME, Memc[pp],
	              SZ_FNAME,  Memc[pp], SZ_FNAME, cl_index, cl_size)
	junk =  fnroot (cluster, root, SZ_FNAME)
	junk =  fnextn (cluster, extn, SZ_EXTN)

	# The first comparision is to avoid a bug in the 
	# fnroot routine in gparse.  March  94
	clus_len = strlen(cluster)
        if (root[1] == '.') {
	   cluster[clus_len] = EOS
	   if (clus_len == 1)
	      call strcpy ("tmp", cluster, SZ_FNAME)
	   else 
	      call strcat ("tmp", cluster, SZ_FNAME)
        } else if (root[1] == EOS)
	   call strcat ("tmp", cluster, SZ_FNAME)

	call sfree(sp)
end

procedure see_extn (extn, template, out_fname, cluster)

char extn[ARB], template[ARB],out_fname[ARB],cluster[ARB]

int  strcmp(),strlen(),envfind()

pointer sp,pp
string	noextn  "T_RFITS: Template filename must have extension"

include "rfits.com"

begin
	call smark(sp)
	call salloc(pp, SZ_FNAME, TY_CHAR)

	if (extn[1] == EOS || strcmp(extn, "tab") == 0 ) {
	   # No extension encountered. Get the user's 'imtype' value.
	   if (envfind ("imtype", extn, SZ_FNAME) <= 0) {
	      # No imtype found, choose 'hhh'.
	      call strcpy ("hhh", extn, SZ_EXTN)	      

	   } else if (strlen (template) != 0) {
	      # Extension encountered. If there is a template file
	      # get its extension and use that for the output file.
	       call iki_parse (template, Memc[pp], extn)
	       if (extn[1] == EOS)
		   call error (0, noextn)
		       
	   } else if (strcmp (extn, "imh") != 0) {
	       # Force the extension to geis if it is not imh, 
	       # as imh and geis are the only two extensions
	       # strfits can handle in a general way
	       call strcpy ("hhh", extn, SZ_FNAME)
	   } 

	   call iki_mkfname (cluster, extn, out_fname, SZ_FNAME)
	   call strcpy (out_fname, cluster, SZ_FNAME)
	}

	if (strcmp (extn, "imh") == 0) 
	    gkey = IMH

	call sfree(sp)
end
