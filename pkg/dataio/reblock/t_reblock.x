# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <error.h>
include <ctype.h>
include <mach.h>
include "reblock.h"

define	MAX_RANGES	100

# T_REBLOCK -- Procedure to copy binary files optionally changing the blocking
# factor. Further documentation in bincopy.hlp.

procedure t_reblock ()

bool	verbose			# print messages
char	infiles[SZ_FNAME]	# input files
char	file_list[SZ_LINE]	# list of tape file numbers
char	outfiles[SZ_FNAME]	# output files
char	padchar			# character for padding blocks and records

char	in_fname[SZ_FNAME], out_fname[SZ_FNAME]
int	nfiles, file_number, file_cnt, range[2 * MAX_RANGES + 1]
int	outparam[LEN_OUTPARAM], strsize, offset

bool	clgetb()
int	mtfile(), strlen(), decode_ranges(), strldxs()
int	btoi(), clgeti(), get_next_number()
include "reblock.com"

begin
	# get input and output file(s)
	call clgstr ("infiles", infiles, SZ_FNAME)
	call clgstr ("outfiles", outfiles, SZ_FNAME)

	# get input file names
	if (mtfile (infiles) == YES) {
	    intape = YES
	    if (infiles[strlen(infiles)] != ']')
	        call clgstr ("file_list", file_list, SZ_LINE)
	    else
		call strcpy ("1", file_list, SZ_LINE)
	} else {
	    intape = NO
	    call strcpy ("1", file_list, SZ_LINE)
	}

	if (decode_ranges (file_list, range, MAX_RANGES, nfiles) == ERR)
	    call error (0, "Illegal file number list.")
	offset = clgeti ("offset")

	# Get output file names
	if (mtfile (outfiles) == YES) {
	    outtape = YES
	    strsize = strlen (outfiles)
	    if (strldxs ("]", outfiles) != strsize) {
	        if (! clgetb ("newtape")) {
		    call sprintf (outfiles[strsize + 1], SZ_FNAME, "%s")
		        call pargstr ("[EOT]")
		} else {
		    call sprintf (outfiles[strsize + 1], SZ_FNAME, "%s")
			call pargstr ("[1]")
		}
	    }
	} else
	    outtape = NO

	# Get block and record sizes
        szb_outblock = clgeti ("outblock")
	if (outtape == NO)
	    szb_outblock = INDEFI
	szb_inrecord = clgeti ("inrecord")
	szb_outrecord = clgeti ("outrecord")
	if (IS_INDEFI(szb_inrecord) && !IS_INDEFI(szb_outrecord))
	    szb_inrecord = szb_outrecord
	if (IS_INDEFI(szb_outrecord) && !IS_INDEFI(szb_inrecord))
	    szb_outrecord = szb_inrecord
	    
	pad_block = btoi (clgetb ("pad_block"))

	if (szb_inrecord < szb_outrecord)
	    pad_record = YES 

	if (szb_inrecord > szb_outrecord)
	    trim_record = YES

	if (pad_block == YES || pad_record == YES) {
	    call clgstr ("padchar", padchar, SZ_LINE)
	    if (IS_DIGIT (padchar))
		padvalue = TO_INTEG (padchar)
	    else
		padvalue = padchar
	}

	# Tape to disk always requires reblocking
	if (intape == YES && outtape == NO)
	    reblock = YES
	else if (pad_record == YES || pad_block == YES || trim_record == YES)
	    reblock = YES
	else if (!IS_INDEFI(szb_outblock) || !IS_INDEFI(szb_inrecord) ||
	    !IS_INDEFI(szb_outrecord))
	    reblock = YES
	else
	    reblock = NO

	# Get remaining parameters.
	nskip = max (0, clgeti ("skipn"))
	ncopy = clgeti ("copyn")
	if (IS_INDEFI(ncopy))
	    ncopy = MAX_INT
	byteswap = btoi (clgetb ("byteswap"))
	wordswap = btoi (clgetb ("wordswap"))
	verbose = clgetb ("verbose")

	# Loop through the files
	file_cnt = 1

	file_number = 0
	while (get_next_number (range, file_number) != EOF) {

	    call strcpy (infiles, in_fname, SZ_FNAME)
	    if (intape == YES && infiles[strlen(infiles)] != ']') {
		call sprintf (in_fname[strlen(in_fname) + 1], SZ_FNAME, "[%d]")
		    call pargi (file_number)
	    }

	    if (outtape == NO) {
	        call strcpy (outfiles, out_fname, SZ_FNAME)
		if (nfiles > 1) {
		    call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME,
		        "%03d")
			call pargi (file_number + offset)
		}
	    } else {
		if (file_cnt == 2) {
		    call sprintf (outfiles[strldxs("[", outfiles)], SZ_FNAME,
		        "%s")
			call pargstr ("[EOT]")
		}
		call strcpy (outfiles, out_fname, SZ_FNAME)
	    }

	    iferr {
		if (verbose) {
		    call printf ("File: %s -> %s: ")
			call pargstr (in_fname)
			call pargstr (out_fname)
		    call flush (STDOUT)
		}

		call reb_reblock_file (in_fname, out_fname, outparam)

		if (verbose) {
		    if (intape == YES)
			call printf ("[skip %d blks] ")
		    else
		        call printf ("[skip %d recs] ")
		    call pargi (nskip)
		    call printf ("blks r/w %d/%d ")
		        call pargi (BLKS_RD(outparam))
		        call pargi (BLKS_WRT(outparam))
		    if (reblock == YES) {
		        call printf ("recs r/w %d/%d\n")
			    call pargi (RECS_RD(outparam))
			    call pargi (RECS_WRT(outparam))
		    } else
			call printf ("\n")
		    call flush (STDOUT)
		}

	    } then {
	        call eprintf ("Cannot read file %s\n")
		    call pargstr (in_fname)
	    } else if (BLKS_RD(outparam) == 0) {
	        if (verbose) {
		    call printf ("Empty file: %s\n")
		        call pargstr (in_fname)
		    call flush (STDOUT)
	        }
		break
	    } else {
		file_cnt = file_cnt + 1
            }
	}
end
