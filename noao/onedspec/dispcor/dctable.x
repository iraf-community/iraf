include	<imhdr.h>
include	<mach.h>
include	"dctable.h"
include	<smw.h>


# DC_TABLE -- Set default wavelengths.
# This may be specified by the task parameters alone, from a reference image,
# or from a text table.  A reference image or table allows separate
# wavelength parameters for each aperture.  The text table columns are the
# aperture number, starting wavelength, ending wavelength, wavelength
# interval per pixel, and number of pixels.  Any of these values may be
# INDEF.

procedure dc_table (table, naps)

pointer	table			# Table pointer (returned)
int	naps			# Number of apertures (returned)

int	i, j, ap, nw, fd, clgeti(), open(), fscan(), nscan(), btoi(), nowhite()
double	ws, we, dw, clgetd()
pointer	sp, fname, tbl, mw, sh, immap(), smw_openim()
bool	clgetb()
errchk	smw_openim(), shdr_open()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call clgstr ("table", Memc[fname], SZ_FNAME)

	# Set defaults.
	naps = 0
	call malloc (table, 10, TY_INT)
	call malloc (Memi[table], TBL_LEN, TY_STRUCT)
	tbl= Memi[table]
	TBL_W1(tbl) = clgetd ("w1")
	TBL_W2(tbl) = clgetd ("w2")
	TBL_DW(tbl) = clgetd ("dw")
	TBL_NW(tbl) = clgeti ("nw")
	TBL_WMIN(tbl) = MAX_REAL
	TBL_WMAX(tbl) = -MAX_REAL
	TBL_NWMAX(tbl) = 0
	TBL_CONFIRM(tbl) = btoi (clgetb ("confirm"))

	# Read a reference image or table if specified and add entries to
	# the table array.

	if (nowhite (Memc[fname], Memc[fname], SZ_FNAME) > 0) {
	    ifnoerr (fd = immap (Memc[fname], READ_ONLY, 0)) {
		mw = smw_openim (fd)
		call shdr_open (fd, mw, 1, 1, INDEFI, SHHDR, sh)
		if (DC(sh) == DCLINEAR || DC(sh) == DCLOG) {
		    do j = 1, IM_LEN(fd,2) {
			call shdr_open (fd, mw, j, 1, INDEFI, SHHDR, sh)
			call dc_getentry (false, AP(sh), table, naps, i)
			tbl = Memi[table+i]
			TBL_AP(tbl) = AP(sh)
			TBL_NW(tbl) = SN(sh)
			TBL_W1(tbl) = W0(sh)
			TBL_W2(tbl) = W1(sh)
			TBL_DW(tbl) = WP(sh)
		    }
		}
		call shdr_close (sh)
		call smw_close (mw)
		call imunmap (fd)
	    } else {
		ifnoerr (fd = open (Memc[fname], READ_ONLY, TEXT_FILE)) {
		    while (fscan (fd) != EOF) {
			call gargi (ap)
			call gargd (ws)
			call gargd (we)
			call gargd (dw)
			call gargi (nw)
			if (nscan() < 5)
			    next

			call dc_getentry (false, ap, table, naps, i)
			tbl = Memi[table+i]
			TBL_AP(tbl) = ap
			TBL_W1(tbl) = ws
			TBL_W2(tbl) = we
			TBL_DW(tbl) = dw
			TBL_NW(tbl) = nw
		    }
		    call close (fd)
		} else
		    call error (1, "Can't access wavelength table")
	    }
	}

	# If ignoreaps=yes then replace INDEFs in the default entry with
	# the first non-INDEF entry.

	if (clgetb ("ignoreaps") && naps > 0) {
	    tbl= Memi[table]
	    if (IS_INDEFD(TBL_W1(tbl)))
		TBL_W1(tbl) = TBL_W1(Memi[table+1])
	    if (IS_INDEFD(TBL_W2(tbl)))
		TBL_W2(tbl) = TBL_W2(Memi[table+1])
	    if (IS_INDEFD(TBL_DW(tbl)))
		TBL_DW(tbl) = TBL_DW(Memi[table+1])
	    if (IS_INDEFI(TBL_NW(tbl)))
		TBL_NW(tbl) = TBL_NW(Memi[table+1])
	}

	call sfree (sp)
end


# DC_GETENTRY -- Get entry from wavelength table.  Return the index.  Allocate
# a new entry if needed.

procedure dc_getentry (apflag, ap, table, naps, index)

bool	apflag		# Ignore aperture numbers?
int	ap		# Aperture
pointer	table		# Wavelength table
int	naps		# Number of apertures
int	index		# Table index of entry

pointer	tbl

begin
	for (index=1; index<=naps; index=index+1)
	    if (apflag || TBL_AP(Memi[table+index]) == ap)
		return

	naps = naps + 1
	if (mod (naps, 10) == 0)
	    call realloc (table, naps+10, TY_INT)
	call malloc (Memi[table+naps], TBL_LEN, TY_STRUCT)

	index = naps
	tbl = Memi[table+index]
	TBL_AP(tbl) = ap
	TBL_W1(tbl) = TBL_W1(Memi[table])
	TBL_W2(tbl) = TBL_W2(Memi[table])
	TBL_DW(tbl) = TBL_DW(Memi[table])
	TBL_NW(tbl) = TBL_NW(Memi[table])
	TBL_WMIN(tbl) = TBL_WMIN(Memi[table])
	TBL_WMAX(tbl) = TBL_WMAX(Memi[table])
	TBL_NWMAX(tbl) = TBL_NWMAX(Memi[table])
	TBL_CONFIRM(tbl) = TBL_CONFIRM(Memi[table])
end
