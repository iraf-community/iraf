include	<fset.h>

# T_PFMERGE -- Merge photometry files written by PHOT, PSF, PEAK, GROUP, 
# NSTAR or ALLSTAR into one photometry file. The fields ID, XCENTER,
# YCENTER, MAG, and MKSKY are read from each input file and written
# without change to the output file. The input files may be text files or
# binary ST tables files. The output file will have the same file type as the
# the first input file. The header of the output file will be the header
# of the first input file.

procedure t_pfmerge ()

pointer	inphotfiles		# the input photometry files
pointer	outphotfile		# the output photometry file
bool	verbose			# verbose output ?

int	plist, lplist, infd, outfd, first_file, in_text, out_text
pointer	sp, infname
bool	clgetb()
int	fstati(), fntopnb(), fntlenb(), fntgfnb(), access(), open()
pointer	tbtopn()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some working memory.
	call smark (sp)
	call salloc (inphotfiles, SZ_FNAME, TY_CHAR)
	call salloc (outphotfile, SZ_FNAME, TY_CHAR)
	call salloc (infname, SZ_FNAME, TY_CHAR)

	# Get the various task parameters.
	call clgstr ("inphotfiles", Memc[inphotfiles], SZ_FNAME)
	call clgstr ("outphotfile", Memc[outphotfile], SZ_FNAME)
	verbose = clgetb ("verbose")

	# Open the input file list and determine the file type of the first
	# file.
	plist = fntopnb (Memc[inphotfiles], NO)
	lplist = fntlenb (plist)
	if (lplist <= 0) {
	    call fntclsb (plist)
	    call sfree (sp)
	    return
	}
	if (fntgfnb (plist, Memc[infname], SZ_FNAME) == EOF) {
	    call fntclsb (plist)
	    call sfree (sp)
	    return
	} else {
	    out_text =  access (Memc[infname], 0, TEXT_FILE)
	    call fntrewb (plist)
	}

	# Open the output file.
	if (out_text == YES)
	    outfd = open (Memc[outphotfile], NEW_FILE, TEXT_FILE)
	else
	    outfd = tbtopn (Memc[outphotfile], NEW_FILE, 0)

	# Loop over the list of input files
	first_file = YES
	while (fntgfnb (plist, Memc[infname], SZ_FNAME) != EOF) {

	    # Print message.
	    if (verbose) {
		call printf ("Merging photometry file %s into %s\n")
		    call pargstr (Memc[infname])
		    call pargstr (Memc[outphotfile])
	    }

	    # Open the input file.
	    in_text =  access (Memc[infname], 0, TEXT_FILE)
	    if (verbose && (in_text != out_text)) {
		if (in_text == YES)
		    call eprintf ("File %s is not an ST table file.\n")
		else
		    call eprintf ("File %s is not a text file.\n")
		call pargstr (Memc[infname])
	    }
	    if (in_text == YES)
	        infd = open (Memc[infname], READ_ONLY, TEXT_FILE)
	    else
	        infd = tbtopn (Memc[infname], READ_ONLY, 0)

	    # Now merge the files.
	    call dp_pfmerge (infd, outfd, in_text, out_text, first_file)

	    # Close the photometry file. 
	    if (in_text == YES)
	        call close (infd)
	    else
	        call tbtclo (infd)

	    first_file = NO
	}

	# Close the output file.
	if (out_text == YES)
	    call close (outfd)
	else
	    call tbtclo (outfd)

	# Close the input file list.
	call fntclsb (plist)

	call sfree(sp)
end	
