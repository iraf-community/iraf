# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include	<ctype.h>
include <mach.h>

define	MAX_RANGES	100
define  TABSIZE           8

# T_RCARDIMAGE -- Procedure to read cardimages tapes. Documentation in
# rcardimage.hlp.

procedure t_rcardimage()

bool	join, verbose
char	infile[SZ_FNAME], outfile[SZ_FNAME]
char	file_list[SZ_LINE]

char	in_fname[SZ_FNAME], out_fname[SZ_FNAME]
int	offset, nlines, file_number, ncards, range[MAX_RANGES*2+1], nfiles
int	lenlist, junk
pointer	list

bool	clgetb()
pointer	fntopnb()
int	btoi(), clgeti(), mtfile(), strlen(), decode_ranges()
int	get_next_number(), fntlenb(), fntgfnb()
include "rcardimage.com"

begin
	# Get parameters.
	call clgstr ("cardfile", infile, SZ_FNAME)
	call clgstr ("textfile", outfile, SZ_FNAME)

	# Make up a file list
	if (mtfile (infile) == YES) {
	    list = NULL
	    if (infile[strlen(infile)] != ']')
	        call clgstr ("file_list", file_list, SZ_LINE)
	    else
	        call strcpy ("1", file_list, SZ_LINE)
	} else {
	    list = fntopnb (infile, YES)
	    lenlist = fntlenb (list)
	    call sprintf (file_list, SZ_LINE, "1-%d")
	        call pargi (lenlist)
	}

	# Decode ranges
	if (decode_ranges (file_list, range, MAX_RANGES, nfiles) == ERR)
	    call error (1, "Illegal file number list")
	    
	# Set up formatting parameters
	card_length = min (SZ_LINE, clgeti ("card_length"))
	if (mod (card_length, SZB_CHAR) != 0)
	    call error (2, "A card must fit in an integral number of chars")
	max_line_length = min (SZ_LINE, clgeti ("max_line_length"))
	join = clgetb ("join")
	if (join)
	    call clgstr ("contn_string", contn_string, SZ_LINE)
	else
	    contn_string[1] = EOS
	entab = btoi (clgetb ("entab"))
	trim = btoi (clgetb ("trim"))
	ebcdic = btoi (clgetb ("ebcdic"))
	ibm = btoi (clgetb ("ibm"))
	if (ibm == YES && ebcdic == YES)
	    call error (3, "Ibm and ebcdic cannot both be true.")

	offset = clgeti ("offset")
	verbose = clgetb ("verbose")

	# Read successive cardimage files, convert and write into a numbered
	# succession of output textfiles.

	file_number = 0
	while (get_next_number (range, file_number) != EOF) {

	    # get input file name
	    if (list != NULL)
		junk = fntgfnb (list, in_fname, SZ_FNAME)
	    else {
	        call strcpy (infile, in_fname, SZ_FNAME)
	        if (infile[strlen(infile)] != ']') {
		    call sprintf (in_fname[strlen(in_fname)+1], SZ_FNAME,
		        "[%d]")
		        call pargi (file_number)
		}
	    }

	    # get output file name
	    call strcpy (outfile, out_fname, SZ_FNAME)
	    if (nfiles > 1) {
		call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME, "%03d")
		    call pargi (file_number + offset)
	    }

	    # Copy the cardimage file to the output text file.  If a read
	    # error occurs, try next file.  If a zero length file is read,
	    # meaning that EOT was reached prematurely, merely exit, deleting
	    # the zero length output file.

	    iferr {
		if (verbose) {
		    call printf ("File: %s -> %s: ")
			call pargstr (in_fname)
			call pargstr (out_fname)
		    call flush (STDOUT)
		}

	        call rc_cardfile_to_textfile (in_fname, out_fname, nlines,
		    ncards)

		if (verbose) {
		    call printf ("%d card images -> %d text lines\n")
			call pargi (ncards)
			call pargi (nlines)
		    call flush (STDOUT)
		}

	    } then {
		call erract (EA_FATAL)
	    } else if (nlines == 0) {			# EOT reached
		if (verbose) {
		    call printf ("EOT encountered at file %s\n")
			call pargi (file_number + offset)
		    call flush (STDOUT)
		}
		call delete (out_fname)
		break
	    }
	}

	if (list != NULL)
	    call fntclsb (list)
end


# RC_CARDFILE_TO_TEXTFILE -- Copy a cardfile to a new textfile.
# Outputs the number of cards read and lines written.

procedure rc_cardfile_to_textfile (in_fname, out_fname, nlines, ncards)

char	in_fname[ARB], out_fname[ARB]
char	lbuf[SZ_LINE], tempbuf[SZ_LINE]
int	in, out, nchars, nlines, ncards

int	mtopen(), open(), rc_fetchcard()
errchk	mtopen, open, rc_fetchcard, putline, strentab, close
include "rcardimage.com"

begin
	in = mtopen (in_fname, READ_ONLY, 0)
	out = open (out_fname, NEW_FILE, TEXT_FILE)

	ncards = 0
	iferr {
	    nchars = rc_fetchcard (in, lbuf, ncards)
	    for (nlines = 0;  nchars != EOF;  nlines = nlines + 1) {
	        if (entab == YES) {
		    call strentab (lbuf, tempbuf, max_line_length, TABSIZE)
		    call putline (out, tempbuf)
	        } else
	            call putline (out, lbuf)
	        nchars = rc_fetchcard (in, lbuf, ncards)
	    }
	 } then
	    call erract (EA_WARN)

	call close (in)
	call close (out)
end


# RC_FETCHCARD -- Procedure to read card images and join those images prefixed
# by an identifying continuation string with the previous image(s).
# Returns number of characters in line or EOF.

int procedure rc_fetchcard (fd, outline, cp)

char	outline[ARB]
int	fd, cp

bool	newfile
char	instring[SZ_LINE * SZ_SHORT]
int	ip, op, npacked_chars, strsize
int	rc_card_to_text(), strlen(), strncmp()
errchk	rc_card_to_text
data	newfile/true/
include "rcardimage.com"

begin
	ip = 1
	op = 1
	strsize = strlen (contn_string)

	# Get first line of file.
	if (newfile) {
	    npacked_chars = rc_card_to_text (fd, instring)
	    newfile = false
	}

	while (npacked_chars != EOF) {
	    # Count cards and file output buffer.
	    while (instring[ip] != EOS  &&  op < max_line_length) {
		outline[op] = instring[ip]
		ip = ip + 1
		op = op + 1
	    }
	    cp = cp + 1

	    # Check for continuation string in next line, move pointer if yes.
	    npacked_chars = rc_card_to_text (fd, instring)

	    if ((strsize != 0) &&
		(strncmp (instring, contn_string, strsize) == 0) &&
		(npacked_chars != EOF)) {
		ip = strsize + 1
	    } else {
		# Output line, remove whitespace, add newline and delimit string
		if (trim == YES)
		    while (op >= 2 && IS_WHITE (outline[op-1]))
			op = op -1
		outline[op] = '\n'
		outline[op+1] = EOS
		return (op)
	    }
	}

	# Initialize for new file.
	newfile = true
	return (EOF)
end


# RC_CARD_TO_TEXT -- Procedure to transform a packed card image to a text image.

int procedure rc_card_to_text (fd, card)

char	card[ARB]
int	fd

int	npacked_chars, nchars
int	read()
errchk	read, ebcdic_to_ascii, ibm_to_ascii 
include "rcardimage.com"

begin
	npacked_chars = read (fd, card, card_length/SZB_CHAR)
	if (npacked_chars == EOF)
	    return (EOF)
	nchars = npacked_chars * SZB_CHAR
	if (ebcdic == YES) {
	    call achtbs (card, card, nchars)
	    call ebcdic_to_ascii (card, card, nchars)
	} else if (ibm == YES) {
	    call achtbs (card, card, nchars)
	    call ibm_to_ascii (card, card, nchars)
	} else
	    call chrupk (card, 1, card, 1, nchars)
	card[nchars+1] = EOS
	return (nchars)
end
