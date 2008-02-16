# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"help.h"

# PR_SUMMARY -- Print a summary of the headers of all help blocks in a
# helpfile.  Open the help file and search for the successive blocks.
# Print nice diagnostics describing the contents of the file. 

define	TABSTOP		50

procedure pr_summary (fname, ctrl)

char	fname[ARB]
pointer	ctrl

int	fd, i, n, nlines
pointer	sp, hb, lbuf
int	open(), gstrcat(), hb_getnextblk(), strlen(), getline(), strmatch()

begin
	call smark (sp)
	call salloc (hb, LEN_HBSTRUCT, TY_STRUCT)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	iferr (fd = open (fname, READ_ONLY, TEXT_FILE)) {
	    call eprintf ("Cannot open helpfile `%s'\n")
		call pargstr (fname)
	    call sfree (sp)
	    return
	}
	HB_LINENO(hb) = 0
	H_IN(ctrl) = fd

	# Search through the help file for the named help block, of type
	# help or system.  Decode the help block header into the HB struct.

	call houtput (ctrl, "\n")
	call sprintf (Memc[lbuf], SZ_LINE, "==> %s <==\n")
	    call pargstr (fname)
	call houtput (ctrl, Memc[lbuf])

	while (H_EOF(ctrl) != EOF && hb_getnextblk (hb, ctrl) != EOF) {
	    # Describe help block.
	    call sprintf (Memc[lbuf], SZ_LINE, "%6d ")
		call pargi (HB_LINENO(hb))

	    # Print keyword names.
	    if (HB_NKEYS(hb) > 0)
		n = gstrcat (HB_KEY(hb,1), Memc[lbuf], SZ_LINE)
	    else
		n = strlen (Memc[lbuf])

	    for (i=2;  i <= HB_NKEYS(hb);  i=i+1) {
		n = gstrcat (", ", Memc[lbuf], SZ_LINE)
		n = gstrcat (HB_KEY(hb,i), Memc[lbuf], SZ_LINE)
	    }

	    # Advance to tabstop and print section label.
	    repeat {
		Memc[lbuf+n] = ' '
		n = n + 1
	    } until (n >= TABSTOP)
	    Memc[lbuf+n] = EOS

	    n = gstrcat (HB_SECTION(hb), Memc[lbuf], SZ_LINE)
	    if (HB_TYPE(hb) != TY_HLP) {
		call sprintf (Memc[lbuf+n], SZ_LINE-n, " (%s)")
		    call pargstr (HB_TYPESTR(hb))
	    }

	    call strcat ("\n", Memc[lbuf], SZ_LINE)
	    call houtput (ctrl, Memc[lbuf])

	    # Print block title, if any, on next line.
	    if (strlen (HB_TITLE(hb)) > 0) {
		call sprintf (Memc[lbuf], SZ_LINE, "       %s\n")
		    call pargstr (HB_TITLE(hb))
		call houtput (ctrl, Memc[lbuf])
	    }

	    # Count and print number of lines in help block.
	    for (nlines=0;  getline (fd, Memc[lbuf]) != EOF;  nlines=nlines+1)
		if (strmatch (Memc[lbuf], "^.{endhelp}") > 0)
		    break
	    call sprintf (Memc[lbuf], SZ_LINE, "       block size %d lines\n")
		call pargi (nlines)

	    call houtput (ctrl, Memc[lbuf])
	    call houtput (ctrl, "\n")
	    HB_LINENO(hb) = HB_LINENO(hb) + nlines + 1
	}
	    
	call close (fd)
	call sfree (sp)
end
