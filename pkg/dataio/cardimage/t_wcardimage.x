# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <error.h>
include <mach.h>

define	TABSIZE		8

# Procedure to write cardimage files. See wcardimage.hlp for documentation.

procedure t_wcardimage()

bool    verbose
char    out_file[SZ_FNAME], in_file[SZ_FNAME], out_fname[SZ_FNAME]
int	strsize, ncards
int     file_number, nlines, list, len_list

int	btoi()
bool	clgetb()
int	clpopni(), clplen(), mtfile(), strldxs(), clgeti(), clgfil(), strlen()
include "wcardimage.com"

begin
	# Flush standard output on newline
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get the parameters.
	list = clpopni ("textfile")
	len_list = clplen (list)

	# Get name of output file.
	# If no tape file number is given tape output then the program
	# asks whether the tape is blank or contains data.
	# If it is blank the tape begins writing at file 1 otherwise at EOT.

	call clgstr ( "cardfile", out_file, SZ_FNAME)
	if (mtfile (out_file) == YES) {
	    strsize = strlen (out_file)
	    if (strldxs ("]", out_file) != strsize) {
		if (! clgetb("new_tape")) {
		    call sprintf (out_file[strsize+1], SZ_FNAME, "%s")
			call pargstr ("[EOT]")
		} else {
		    call sprintf (out_file[strsize+1], SZ_FNAME, "%s")
		        call pargstr ("[1]")
		}
	    }
	}

	# Get card_length and determine whether it fits in an integral number
	# of characters.

	card_length = min (SZ_LINE, clgeti ("card_length"))
	if (mod (card_length, SZB_CHAR) !=  0)
	    call error (1, "A card must fit in an integral number of chars.")

	# Get number of cards per physical block and convert to packed chars.
	cards_per_blk = clgeti ("cards_per_blk")

	call clgstr ("contn_string", contn_string, SZ_LINE)
	    if (strlen (contn_string) > card_length)
		call error (2,
		    "Continuation string cannot be > card_length chars.")

	detab = btoi (clgetb ("detab"))

	ebcdic = btoi (clgetb ("ebcdic"))
	ibm = btoi (clgetb ("ibm"))
	if (ibm == YES && ebcdic == YES)
	    call error (3, "Ibm and ebcdic cannot both be true.")

	verbose = clgetb ("verbose")

	file_number = 1
	while (clgfil (list, in_file, SZ_FNAME) !=  EOF) {
	    if (mtfile (out_file) == NO) {
		if (len_list > 1) {
		    call strcpy (out_file, out_fname, SZ_FNAME)
		    call sprintf (out_fname[strlen(out_fname)+1],
			SZ_FNAME, "%03d")
		        call pargi (file_number)
		} else
		    call strcpy (out_file, out_fname, SZ_FNAME)
	    } else {
		if (file_number == 2) {
		    call sprintf (out_file[strldxs("[", out_file)],
			SZ_FNAME, "%s")
			call pargstr ("[EOT]")
		}
		call strcpy (out_file, out_fname, SZ_FNAME)
	    }

	    # Copy text file to cardimage file.

	    iferr {
		if (verbose) {
		    call printf ("File: %s -> %s: ")
			call pargstr (in_file)
			call pargstr (out_fname)
		    call flush (STDOUT)
		}

		call wc_textfile_to_cardfile (in_file, out_fname, ncards,
		    nlines)

		if (verbose) {
		    call printf ("%d lines read -> %d cards written\n")
			call pargi (nlines)
			call pargi (ncards)
		    call flush (STDOUT)
		}
	    } then {
		call erract (EA_FATAL)
	    } else if (ncards == 0) {
		if (verbose) {
		    call printf ("\tInput file is binary or empty\n")
		    call flush (STDOUT)
		}
	    }
	    file_number = file_number + 1
	}
end


# WC_TEXTFILE_TO_CARDFILE -- Reads a textfile from disk and outputs a card
# image file to tape or disk.

procedure wc_textfile_to_cardfile (in_file, out_fname, ncards, nlines)

char    in_file[ARB]
char	out_fname[ARB]
int     ncards
int     nlines

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

	in = open (in_file, READ_ONLY, TEXT_FILE)
	if (mtfile (out_fname) == YES) {
	    chars_per_blk = cards_per_blk * card_length / SZB_CHAR
	    out = mtopen (out_fname, WRITE_ONLY, chars_per_blk)
	} else
	    out = open (out_fname, NEW_FILE, BINARY_FILE)

	# write file
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

char    line[ARB], card[ARB]
int     init, ip, nchars

errchk	ascii_to_ebcdic, ascii_to_ibm, achtsb, chrpak
include "wcardimage.com"

begin
	# pad with blanks
	init = nchars
	if (line[init] != '\n')
	    init = init + 1
	for (ip=init;  ip <= card_length;  ip=ip+1)
	    line[ip] = ' '

	# pack the line
	if (ebcdic == YES) {
	    call ascii_to_ebcdic (line, card, card_length)
	    call achtsb (card, card, card_length)
	} else if (ibm == YES) {
	    call ascii_to_ibm (line, card, card_length)
	    call achtsb (card, card, card_length)
	} else
	    call chrpak (line, 1, card, 1, card_length)
end


# WC_FETCHLINE -- Procedure to fetch a line of text and split into pieces
# <= maxch characters long optionally prefixing the remainders of lines
# with a character string.

int procedure wc_fetchline (fd, linebuf, lp, maxch)

int    fd
char   linebuf[ARB]
int    lp
int    maxch

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
