include "igi.h"

# # 8/20/91 Removed ^Ls. ZGL
##  12 June 1992  Modified pdlist().  ZGL
##  1/27/93  Fixed dummy array declaration (ARB).
##  3/15/93  Add capability to write to file specified in the optional
##  argument.


procedure ig_dlist (igs)

#  IG_DLIST -- List the data vectors on STDOUT.  Write to a temporary 
#  file and page it.

pointer	igs		# igi parameters structure

pointer	igps		# Parameters structure descriptor
bool	cols[6]
pointer	tokvals		# Token value structure
int	token
int	sf
pointer	sp, tempfile

int	gettok(), open()

begin
	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	call lcmdcat (igs, YES)

	token = gettok (igs)

	if (IS_NEWCOMMAND (token)) {
	    # No argument;  write to STDOUT

	    if (STDOUT_REDIR(igs) == YES) {
		# Standard output redirected;  don't page temp file
		sf = STDOUT

		if (DEBUG_OUTPUT(igs) == YES) {
		    call eprintf ("Output data to STDOUT")
		}

	    } else {
		call smark  (sp)
		call salloc (tempfile, SZ_FNAME, TY_CHAR)
		call mktemp ("tmp$igi_data", Memc[tempfile], SZ_FNAME)
		sf = open (Memc[tempfile], TEMP_FILE, TEXT_FILE)

		if (DEBUG_OUTPUT(igs) == YES) {
		    call eprintf ("Output data list file:  %s ")
			call pargstr (Memc[tempfile])
		}
	    }

	} else {
	    # Argument specifies output file
	    if (token != IDENTIFIER && token != STRING)
		return

	    call lcmdcat (igs, NO)
	    call cmdcat (igs, NO)

	    sf = open (LOP_VALC(tokvals), NEW_FILE, TEXT_FILE)

	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("Output data list file:  %s ")
		    call pargstr (LOP_VALC(tokvals))
	    }
	}

	cols[1] = MG_XDATAP(igps) != NULL
	cols[2] = MG_YDATAP(igps) != NULL
	cols[3] = MG_EDATAP(igps) != NULL
	cols[4] = MG_PDATAP(igps) != NULL
	cols[5] = MG_LDATAP(igps) != NULL
	cols[6] = MG_SDATAP(igps) != NULL

	call pdlist (sf, cols,
	    Memr[MG_XDATAP(igps)], MG_XNPTS(igps),
	    Memr[MG_YDATAP(igps)], MG_YNPTS(igps),
	    Memr[MG_EDATAP(igps)], MG_ENPTS(igps),
	    Memr[MG_PDATAP(igps)], MG_PNPTS(igps),
	    Memr[MG_LDATAP(igps)], MG_LNPTS(igps),
	    Memr[MG_SDATAP(igps)], MG_SNPTS(igps))

	if (sf != STDOUT) {
	    # Output file
	    call close (sf)

	    if (IS_NEWCOMMAND (token)) {
		# Temporary page file
		call gpagefile (GIO_GP(igs), Memc[tempfile], 
		    "Plot Data Vectors")

		call delete (Memc[tempfile])
		call sfree (sp)
	    }
	}
end


procedure pdlist (sf, cols, x, xn, y, yn, e, en, p, pn, l, ln, s, sn)

#  PDLIST -- List the plot data vectors using the cl pager
##  12 June 1992  Change to prevent spooling and paging output if STDOUT
##  redirected.  ZGL
##  15 March 1993  Reorganize to move file-related stuff to caller.
##  Add coment character to first line, header.  ZGL
# --

int	sf
bool	cols[ARB]
real	x[ARB], y[ARB], e[ARB], p[ARB], l[ARB], s[ARB]
int	xn, yn, en, pn, ln, sn

int	i, n
int	ptypen, ptypes
real	size

begin
	if (xn + yn + en + pn == 0 || 
	    (!cols[1] && !cols[2] && !cols[3] && !cols[4])) {
	    call eprintf ("# No data ")
	    return
	}

	call fprintf (sf, "#     ")
	if (cols[1])
	    call fprintf (sf, "       X ")
	if (cols[2])
	    call fprintf (sf, "       Y ")
	if (cols[3])
	    call fprintf (sf, "   Error ")
	if (cols[4])
	    call fprintf (sf, "   Point  Vert.  Style   Size")
	if (cols[5])
	    call fprintf (sf, "  Limits ")
	if (cols[6])
	    call fprintf (sf, " Scratch ")
	call fprintf (sf, "\n")

	n = xn
	n = max (n, yn)
	n = max (n, en)
	n = max (n, pn)

	do i = 1, n {
	    call fprintf (sf, "%5d ")
		call pargi (i)

	    if (cols[1])
		if (i <= xn) {
		    call fprintf (sf, "%8g ")
			call pargr (x[i])
		} else
		    call fprintf (sf, "         ")

	    if (cols[2])
		if (i <= yn) {
		    call fprintf (sf, "%8g ")
			call pargr (y[i])
		} else
		    call fprintf (sf, "         ")

	    if (cols[3])
		if (i <= en) {
		    call fprintf (sf, "%8g ")
			call pargr (e[i])
		} else
		    call fprintf (sf, "         ")

	    if (cols[4]) 
		if (i <= pn) {
		    call ptypns (p[i], ptypen, ptypes, size)
		    call fprintf (sf, "%8g %6d %6d %6.2f")
			call pargr (p[i])
			call pargi (ptypen)
			call pargi (ptypes)
			call pargr (size)
		}

	    if (cols[5])
		if (i <= ln) {
		    call fprintf (sf, "%8g ")
			call pargr (l[i])
		} else
		    call fprintf (sf, "         ")

	    if (cols[6])
		if (i <= sn) {
		    call fprintf (sf, "%8g ")
			call pargr (s[i])
		} else
		    call fprintf (sf, "         ")

	    call fprintf (sf, "\n")
	}
end
