# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <error.h>
include <ctype.h>
include <mach.h>
include "reblock.h"

define	MAX_RANGES	100
define	SZ_PADCHAR	10

# T_REBLOCK -- Procedure to copy binary files optionally changing the blocking
# factor. Further documentation in reblock.hlp.

procedure t_reblock ()

char	infiles[SZ_FNAME]	# list of input files
char	file_list[SZ_LINE]	# list of tape file numbers
char	outfiles[SZ_FNAME]	# list of output files
char	padchar[SZ_PADCHAR]	# character for padding blocks and records
bool	verbose			# print messages ?

char	in_fname[SZ_FNAME], out_fname[SZ_FNAME], cval
int	inlist, outlist, len_inlist, len_outlist, file_number, file_cnt
int	range[2 * MAX_RANGES + 1]
int	outparam[LEN_OUTPARAM], offset, ip

bool	clgetb()
int	fstati(), mtfile(), mtneedfileno(), fntopnb(), fntlenb(), fntgfnb()
int	decode_ranges(), btoi(), clgeti(), get_next_number(), cctoc()
include "reblock.com"

begin
	# Flush on a newline if the output is not redirected.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get the input and output file(s).
	call clgstr ("infiles", infiles, SZ_FNAME)
	call clgstr ("outfiles", outfiles, SZ_FNAME)

	# Get the input file names.
	if (mtfile (infiles) == YES) {
	    inlist = NULL
	    intape = YES
	    if (mtneedfileno (infiles) == YES)
	        call clgstr ("file_list", file_list, SZ_LINE)
	    else
		call strcpy ("1", file_list, SZ_LINE)
	} else {
	    inlist = fntopnb (infiles, NO)
	    len_inlist = fntlenb (inlist)
	    intape = NO
	    if (len_inlist > 0) {
		call sprintf (file_list, SZ_LINE, "1-%d")
		    call pargi (len_inlist)
	    } else
	        call strcpy ("0", file_list, SZ_LINE)
	}

	# Decode the tape file number list.
	if (decode_ranges (file_list, range, MAX_RANGES, len_inlist) == ERR)
	    call error (0, "Illegal file number list.")
	offset = clgeti ("offset")

	# Get the output file names.
	if (mtfile (outfiles) == YES) {
	    outlist = NULL
	    len_outlist = len_inlist
	    outtape = YES
	    if (mtneedfileno (outfiles) == YES) {
	        if (! clgetb ("newtape"))
		    call mtfname (outfiles, EOT, out_fname, SZ_FNAME)
		else
		    call mtfname (outfiles, 1, out_fname, SZ_FNAME)
	    } else
		call strcpy (outfiles, out_fname, SZ_FNAME)
	} else {
	    outlist = fntopnb (outfiles, NO)
	    len_outlist = fntlenb (outlist)
	    outtape = NO
	}
	if ((len_inlist > 1) && (len_outlist != 1) &&
	    (len_outlist != len_inlist))
	    call error (0,
	        "The number of input and output files is not equal")

	# Get the block and record sizes.
        szb_outblock = clgeti ("outblock")
	if (outtape == NO)
	    szb_outblock = INDEFI
	szb_inrecord = clgeti ("inrecord")
	szb_outrecord = clgeti ("outrecord")
	if (IS_INDEFI(szb_inrecord) && !IS_INDEFI(szb_outrecord))
	    szb_inrecord = szb_outrecord
	if (IS_INDEFI(szb_outrecord) && !IS_INDEFI(szb_inrecord))
	    szb_outrecord = szb_inrecord

	# Get the pad and trim parameters.
	pad_block = btoi (clgetb ("pad_block"))
	if (szb_inrecord < szb_outrecord)
	    pad_record = YES 
	else
	    pad_record = NO
	if (szb_inrecord > szb_outrecord)
	    trim_record = YES
	else
	    trim_record = NO
	if (pad_block == YES || pad_record == YES) {
	    call clgstr ("padchar", padchar, SZ_PADCHAR)
	    ip = 1
	    if (cctoc (padchar, ip, cval) <= 0)
		cval = ' '
	    if (IS_DIGIT (cval))
		padvalue = TO_INTEG (cval)
	    else
		padvalue = cval
	}

	# Tape to disk always requires reblocking.
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

	    # Construct the input file name.
	    if (intape == YES) {
		if (mtneedfileno (infiles) == YES)
		    call mtfname (infiles, file_number, in_fname, SZ_FNAME)
		else
	            call strcpy (infiles, in_fname, SZ_FNAME)
	    } else if (fntgfnb (inlist, in_fname, SZ_FNAME) != EOF)
		;

	    # Construct the output file name.
	    if (outtape == NO) {
		if (len_inlist > 1 && len_outlist == 1) {
		    call sprintf (out_fname[1], SZ_FNAME, "%s%03d")
		        call pargstr (outfiles)
		    if (intape == YES)
		        call pargi (file_number + offset)
		    else
		        call pargi (file_cnt)
	        } else if (fntgfnb (outlist, out_fname, SZ_FNAME) != EOF)
		    ;
	    } else if (file_cnt == 2)
		call mtfname (out_fname, EOT, out_fname, SZ_FNAME)

	    iferr {

		if (verbose) {
		    call printf ("File: %s -> %s: ")
			call pargstr (in_fname)
			call pargstr (out_fname)
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
		}

	    } then {
		call flush (STDOUT)
	        call eprintf ("Cannot read file %s\n")
		    call pargstr (in_fname)
	    } else if (BLKS_RD(outparam) == 0) {
	        if (verbose) {
		    call printf ("Empty file: %s\n")
		        call pargstr (in_fname)
	        }
		break
	    } else {
		file_cnt = file_cnt + 1
            }
	}

	if (inlist != NULL)
	    call fntclsb (inlist)
	if (outlist != NULL)
	    call fntclsb (outlist)
end
