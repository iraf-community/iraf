include	<mach.h>
include	"disptable.h"


# DC_TABLE -- Get default wavelengths.  This may be specified by the task
# parameters alone or from a table.  The table allows separate wavelength
# parameters for each aperture.  The table columns are the aperture
# number, starting wavelength, ending wavelength, wavelength interval per
# pixel, and number of pixels.  Any of these values may be INDEF.

procedure dc_table (table, naps)

pointer	table			# Table pointer (returned)
int	naps			# Number of apertures (returned)

int	i, ap, nw, fd, clgeti(), open(), fscan(), nscan(), btoi()
real	w1, w2, dw, clgetr()
pointer	sp, fname, tbl
bool	clgetb()
errchk	open

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call clgstr ("table", Memc[fname], SZ_FNAME)

	# Set defaults.
	naps = 0
	call malloc (table, 10, TY_INT)
	call malloc (Memi[table], TBL_LEN, TY_STRUCT)
	tbl= Memi[table]
	TBL_W1(tbl) = clgetr ("w1")
	TBL_W2(tbl) = clgetr ("w2")
	TBL_DW(tbl) = clgetr ("dw")
	TBL_NW(tbl) = clgeti ("nw")
	TBL_WMIN(tbl) = MAX_REAL
	TBL_WMAX(tbl) = -MAX_REAL
	TBL_NWMAX(tbl) = 0
	TBL_CONFIRM(tbl) = btoi (clgetb ("confirm"))

	# Read a table if specified and add entries to the table array.
	iferr {
	    fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	    while (fscan (fd) != EOF) {
	        call gargi (ap)
	        call gargr (w1)
	        call gargr (w2)
	        call gargr (dw)
	        call gargi (nw)
	        if (nscan() < 5)
		    next

		call dc_getentry (false, ap, table, naps, i)
		tbl = Memi[table+i]
		TBL_AP(tbl) = ap
		TBL_W1(tbl) = w1
		TBL_W2(tbl) = w2
		TBL_DW(tbl) = dw
		TBL_NW(tbl) = nw
	    }
	    call close (fd)
	} then
	    ;

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
