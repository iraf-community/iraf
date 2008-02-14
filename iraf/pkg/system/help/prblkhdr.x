# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"help.h"

# PR_BLOCK_HEADER -- Print the help block header.  Clear screen first if
# enabled.  Print header in the form
#
#		 MODNAME (section)      title      MODNAME (section) 
#
# followed by a blank line.

procedure pr_block_header (hb, modname, ctrl)

pointer	hb		# encode help block header
char	modname[ARB]	# module name
pointer	ctrl		# help control block

char	blank
int	n, center, offset, lmargin, rmargin
pointer	sp, lbuf, edge, op, hbuf
int	strlen(), gstrcpy()

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (edge, SZ_LINE, TY_CHAR)
	call salloc (hbuf, SZ_LINE, TY_CHAR)

	# Clear screen.
	if (H_FORMAT(ctrl) == HF_TEXT) {
	    if (H_SOFLAG(ctrl) == YES)
	        call houtput (ctrl, "\f")
	} else {
	    call sfree (sp)
	    return
	}

	lmargin = H_LMARGIN(ctrl)
	rmargin = min (H_RMARGIN(ctrl), SZ_LINE-1)

	# Initialize the output line to blanks.
	blank = ' '
	call amovkc (blank, Memc[lbuf], SZ_LINE)

	n = strlen (HB_TITLE(hb))
	center = (lmargin + rmargin) / 2
	offset = max (lmargin, center - n/2)

	# Center help block title in output line.
	call amovc (HB_TITLE(hb), Memc[lbuf+offset], min(n,rmargin-offset))

	# Format the MODNAME (section) into the "edge" buffer.  MODNAME is the
	# module name (one of the keys), transformed to upper case.

	if (HB_NKEYS(hb) >= 1) {
	    op = edge + gstrcpy (modname, Memc[edge], SZ_LINE)
	    call strupr (Memc[edge])
	} else {
	    Memc[edge] = EOS
	    op = edge
	}

	if (HB_SECTION(hb) != EOS) {
	    call sprintf (Memc[op], SZ_LINE-SZ_KEY, " (%s)")
		call pargstr (HB_SECTION(hb))
	}

	n = strlen (Memc[edge])
	call amovc (Memc[edge], Memc[lbuf+lmargin-1], n)
	call amovc (Memc[edge], Memc[lbuf + rmargin - n], n)

	Memc[lbuf+rmargin] = '\n'
	Memc[lbuf+rmargin+1] = EOS

	# Write out the header line, followed by a blank line.
	call houtput (ctrl, Memc[lbuf])
	call houtput (ctrl, "\n")

	call sfree (sp)
end
