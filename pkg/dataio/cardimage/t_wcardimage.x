# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <error.h>
include <mach.h>

define	TABSIZE		8

# Procedure to write cardimage files. See wcardimage.hlp for documentation.

procedure t_wcardimage()

char    out_file[SZ_FNAME]		# input file name list
char	in_file[SZ_FNAME]		# output file name list
bool    verbose				# verbose mode ?

char	out_fname[SZ_FNAME]
int	ncards, file_number, nlines, list, len_list

bool	clgetb()
int	fstati(), clpopni(), clplen(), mtfile(), mtneedfileno()
int	clgeti(), clgfil(), strlen(), btoi()
include "wcardimage.com"

begin
	# Flush standard output on newline
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get the parameters.
	list = clpopni ("textfile")
	len_list = clplen (list)

	# Get name of output file.
	# If no tape file number is given tape output then the program
	# asks whether the tape is blank or contains data.
	# If it is blank the tape begins writing at file 1 otherwise at EOT.
	# Note that at some point this code needs to be modified to
	# accept an output file name template.

	call clgstr ( "cardfile", out_file, SZ_FNAME)
	if (mtfile (out_file) == YES) {
	    if (mtneedfileno (out_file) == YES) {
		if (! clgetb("new_tape"))
		    call mtfname (out_file, EOT, out_fname, SZ_FNAME)
		else
		    call mtfname (out_file, 1, out_fname, SZ_FNAME)
	    } else
		call strcpy (out_file, out_fname, SZ_FNAME)
	}

	# Get card_length and determine whether it fits in an integral number
	# of characters.

	card_length = min (SZ_LINE, clgeti ("card_length"))
	if (mod (card_length, SZB_CHAR) !=  0)
	    call error (1, "A card must fit in an integral number of chars.")

	# Get number of cards per physical block and convert to packed chars.
	cards_per_blk = clgeti ("cards_per_blk")

	# Get the formatting parameters.
	call clgstr ("contn_string", contn_string, SZ_LINE)
	    if (strlen (contn_string) > card_length)
		call error (2,
		    "Continuation string cannot be > card_length chars.")
	detab = btoi (clgetb ("detab"))

	# Get the character type parameters.
	ebcdic = btoi (clgetb ("ebcdic"))
	ibm = btoi (clgetb ("ibm"))
	if (ibm == YES && ebcdic == YES)
	    call error (3, "Ibm and ebcdic cannot both be true.")

	verbose = clgetb ("verbose")

	file_number = 1
	while (clgfil (list, in_file, SZ_FNAME) !=  EOF) {
	    if (mtfile (out_file) == NO) {
		if (len_list > 1) {
		    call sprintf (out_fname[1], SZ_FNAME, "%s%03d")
			call pargstr (out_file)
		        call pargi (file_number)
		} else
		    call strcpy (out_file, out_fname, SZ_FNAME)
	    } else {
		if (file_number == 2)
		    call mtfname (out_fname, EOT, out_fname, SZ_FNAME)
	    }

	    # Copy text file to cardimage file.

	    iferr {
		if (verbose) {
		    call printf ("File: %s -> %s: ")
			call pargstr (in_file)
			call pargstr (out_fname)
		}

		call wc_textfile_to_cardfile (in_file, out_fname, ncards,
		    nlines)

		if (verbose) {
		    call printf ("%d lines read -> %d cards written\n")
			call pargi (nlines)
			call pargi (ncards)
		}
	    } then {
		call flush (STDOUT)
		call erract (EA_FATAL)
	    } else if (ncards == 0) {
		if (verbose)
		    call printf ("\tInput file is binary or empty\n")
	    }

	    file_number = file_number + 1
	}
end


# WC_TEXTFILE_TO_CARDFILE -- Reads a textfile from disk and outputs a card
# image file to tape or disk.

procedure wc_textfile_to_cardfile (in_file, out_fname, ncards, nlines)

char    in_file[ARB]			# input file name
char	out_fname[ARB]			# output file name
int     ncards				# number of card images
int     nlines				# number of text lines

char    linebuf[SZ_LINE]
int     in, out, nchars, chars_per_blk
int     mtopen(), open(), access(), wc_fetchline(), mtfile()
errchk  mtopen, open, access, wc_fetchline, write, close, wc_text_to_card
include "wcardimage.com"

begin
	nlines = 0
	ncards = 0

	if (access (in_file, READ_ONLY, TEXT_FILE) != YES)
	    return

	# Open the file.
	in = open (in_file, READ_ONLY, TEXT_FILE)
	if (mtfile (out_fname) == YES) {
	    chars_per_blk = cards_per_blk * card_length / SZB_CHAR
	    out = mtopen (out_fname, WRITE_ONLY, chars_per_blk)
	} else
	    out = open (out_fname, NEW_FILE, BINARY_FILE)

	# Write file.
	nchars = wc_fetchline (in, linebuf, nlines, card_length+1)
	while (nchars != EOF) {
	    call wc_text_to_card (linebuf, nchars, linebuf)
	    call write (out, linebuf, card_length/SZB_CHAR)
	    ncards = ncards + 1
	    nchars = wc_fetchline (in, linebuf, nlines, card_length+1)
	}
	
	call close (in)
	call close (out)
end


# WC_TEXT_TO_CARD -- Convert text string into a packed cardimage string
# removing the newline character if necessary, padding with blanks
# if required and optionally translating from ascii to ebcdic or ibm
# ebcdic.

procedure wc_text_to_card (line, nchars, card)

char    line[ARB]			# input text line
int	nchars				# number of chars in line
char	card[ARB]			# output packed card image

int     init, ip
errchk	ascii_to_ebcdic, ascii_to_ibm, achtsb, chrpak
include "wcardimage.com"

begin
	# Pad with blanks.
	init = nchars
	if (line[init] != '\n')
	    init = init + 1
	for (ip=init;  ip <= card_length;  ip=ip+1)
	    line[ip] = ' '

	# Pack the line.
	if (ebcdic == YES) {
	    call ascii_to_ebcdic (line, card, card_length)
	    call achtsb (card, card, card_length)
	} else if (ibm == YES) {
	    call ascii_to_ibm (line, card, card_length)
	    call achtsb (card, card, card_length)
	} else
	    call chrpak (line, 1, card, 1, card_length)
end


# WC_FETCHLINE -- Procedure to fetch a line of text and split it into pieces
# <= maxch characters long, optionally prefixing the remainders of lines
# with a character string.

int procedure wc_fetchline (fd, linebuf, lp, maxch)

int    fd				# input file descriptor
char   linebuf[ARB]			# output chunk of text
int    lp				# number of lines read
int    maxch				# maximum size of chunk of text

char   line[SZ_LINE], inline[SZ_LINE]
int    nchars, ip, op, offset, strsize
int    getline(), gstrcpy(), strlen(), strncmp()
errchk  getline(), strdetab()
include "wcardimage.com"
data   ip /1/

begin
	# Get new line and detab if requested.
	if (ip == 1) {
	    if (detab == YES) {
		nchars = getline (fd, inline)
		call strdetab (inline, line, SZ_LINE, TABSIZE)
	    } else 
		nchars = getline (fd, line)
	    if (nchars == EOF)
		return (EOF)

	    lp = lp + 1
	    offset = 0
	    strsize = strlen (contn_string)
	    if (strsize != 0 && strncmp (line, contn_string, strsize) == 0)
		call eprintf ("Warning: Line matches continuation string\n")

	} else {
	    # Otherwise determine length of continuation string.
	    offset = gstrcpy (contn_string, linebuf, SZ_LINE)
	}

	# Copy maxch characters to the output buffer.
	op = offset + 1
	while (line[ip] != EOS && op < maxch && line[ip] != '\n') {
	    linebuf[op] = line[ip]
	    ip = ip + 1
	    op = op + 1
	}

	# Add newline and EOS reset pointer.
	linebuf[op] = '\n'
	linebuf[op+1] = EOS
	if (line[ip] == EOS || line[ip] == '\n')
	    ip = 1

	return (op)
end
