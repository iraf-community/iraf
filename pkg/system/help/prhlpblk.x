# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	"help.h"

# PR_HELPBLOCK -- Print a normal helpblock.  Open the help file and search
# for the named help block.  If found and not printing single section or
# parameter, print manpage title.  Process helpblock through Lroff.

procedure pr_helpblock (fname, pakname, modname, blktype, ctrl)

char	fname[ARB]
char	pakname[ARB]
char	modname[ARB]
int	blktype
pointer	ctrl

bool	block_found, help_file, at_eof
int	fd, lmarg, rmarg, soflag, foflag, i, nblocks
pointer	sp, hb, block_name, ps

bool	streq()
int	open(), hb_getnextblk()
pointer	ps_open()
extern	hinput(), houtput()
errchk	hb_getnextblk, pr_block_header

begin
	call smark (sp)
	call salloc (hb, LEN_HBSTRUCT, TY_STRUCT)
	call salloc (block_name, SZ_FNAME, TY_CHAR)

	iferr (fd = open (fname, READ_ONLY, TEXT_FILE)) {
	    call sfree (sp)
	    call erract (EA_ERROR)
	    return
	}

	# Search through the help file for the named help block, of type
	# help or system.  Decode the help block header into the HB struct.

	H_IN(ctrl) = fd
	HB_LINENO(hb) = 0
	H_EOF(ctrl) = NO
	lmarg = H_LMARGIN(ctrl)
	rmarg = H_RMARGIN(ctrl)
	at_eof = false
	help_file = false
	nblocks = 0
	call strcpy (modname, Memc[block_name], SZ_FNAME)

	# Output standout mode control chars (soflag) only if told to do so.
	# Only output forms mode control chars if using Manpage format output
	# filter.
		
	soflag = H_SOFLAG(ctrl)
	foflag = H_MANPAGE(ctrl)

	# Process all help blocks in the file which list the current key
	# (modname) in their title block, and which are of the desired
	# type, i.e., hlp or sys.

	while (!at_eof && H_QUIT(ctrl) != YES) {
	    block_found = false
	    repeat {
		at_eof = (hb_getnextblk (hb, ctrl) == EOF)
		if (!at_eof) {
		    help_file = true

		    if (HB_TYPE(hb) == blktype || blktype == NULL) {
			# Search keyword list for the module name.  If no
			# module name was given, match anything.

			if (modname[1] == EOS) {
			    block_found = true
			    nblocks = nblocks + 1
			} else {
			    for (i=1;  i <= HB_NKEYS(hb);  i=i+1)
				if (streq (HB_KEY(hb,i), modname)) {
				    block_found = true
				    nblocks = nblocks + 1
				    break
				}
			}

			# If no module name, use first key as help block name.
			if (modname[1] == EOS)
			    call strcpy (HB_KEY(hb,1), Memc[block_name],
				SZ_FNAME)
		    }
		}
	    } until (block_found || at_eof)

	    if (at_eof) {
		# If cannot find block, something is wrong, because the
		# help directory said there was a block of the desired
		# type in this file.  If no help blocks at all were found
		# in the file, the best thing we can do is to simply print
		# the file; hopefully it contains unformatted text.  If the
		# file contains help blocks, but none with the correct key
		# and type, then we print a warning message.

		if (!help_file) {
		    call close (fd)
		    # Print header only if there is more than a single list
		    # element.
		    if (H_LENTL(ctrl) > 1 && H_OPTION(ctrl) != O_REFERENCES)
			call pr_modname (ctrl, pakname, modname)
		    call pr_file (fname, ctrl, pakname)
		    call sfree (sp)
		    return
		} else if (nblocks == 0) {
		    call eprintf ("Cannot find help block for `%s' in `%s'\n")
			call pargstr (modname)
			call pargstr (fname)
		    break
		} else
		    break
	    }

	    # Clear the screen and print the block header, if not printing a
	    # single section or parameter.

	    H_EOF(ctrl) = NO
	    if (H_SECNAME(ctrl) == EOS && H_PARNAME(ctrl) == EOS)
		call pr_block_header (hb, Memc[block_name], ctrl)


	    # Finally!!  Call Lroff to format the help text.
	    iferr {
		if (H_FORMAT(ctrl) == HF_TEXT) {
		    call lroff (hinput, ctrl, houtput, ctrl, lmarg, rmarg,
		        soflag, foflag)
		} else if (H_FORMAT(ctrl) == HF_HTML) {
		    call lroff2html (H_IN(ctrl), H_OUT(ctrl), Memc[block_name],
			HB_SECTION(hb), HB_TITLE(hb), H_PARNAME(ctrl),
			H_SECNAME(ctrl))
		} else if (H_FORMAT(ctrl) == HF_PS) {
		    ps = ps_open (H_OUT(ctrl), YES)
		    call sprintf (Memc[block_name], SZ_LINE, "%s (%s)")
			call pargstr (Memc[block_name])
			call pargstr (HB_SECTION(hb))
		    call ps_header (ps, Memc[block_name], HB_TITLE(hb), 
			Memc[block_name])
		    call lroff2ps (H_IN(ctrl), H_OUT(ctrl), ps,
			H_PARNAME(ctrl), H_SECNAME(ctrl))
		}
	    } then
		call erract (EA_WARN)
	}

	call close (fd)
	call sfree (sp)
end
