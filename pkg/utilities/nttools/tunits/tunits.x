include <tbset.h>
include "tunits.h"

#* HISTORY *
#* B.Simon	07-Jan-99	Original

# TUNITS -- Convert table column from one set of units to another

procedure tunits ()

#--
pointer	table		# table name
pointer column		# column name
pointer newunits	# new column units
pointer	oldunits	# old column units
pointer	abrevtab	# table of unit abbreviations
pointer	unittab		# table of unit conversions
bool	verbose		# print diagnostic messages

double	factor
int	type
pointer	sp, tp, cp, ab, ut, punit1, punit2

string	nocolumn  "Column not found"
string	unitblank "Units parameter is blank"
string	notfloat  "Table column is not floating point"

bool	clgetb(), isblank()
double	find_factor()
int	tbcigi()
pointer	tbtopn(), read_abrev(), read_units(), parse_units()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (column, SZ_FNAME, TY_CHAR)
	call salloc (newunits, SZ_FNAME, TY_CHAR)
	call salloc (oldunits, SZ_FNAME, TY_CHAR)
	call salloc (abrevtab, SZ_FNAME, TY_CHAR)
	call salloc (unittab, SZ_FNAME, TY_CHAR)

	# Read required task parameters

	call clgstr ("table", Memc[table], SZ_FNAME)
	call clgstr ("column", Memc[column], SZ_FNAME)
	call clgstr ("newunits", Memc[newunits], SZ_FNAME)
	call clgstr ("oldunits", Memc[oldunits], SZ_FNAME)
	call clgstr ("abrevtab", Memc[abrevtab], SZ_FNAME)
	call clgstr ("unittab", Memc[unittab], SZ_FNAME)
	verbose = clgetb ("verbose")

	# Open table, find column

	tp = tbtopn (Memc[table], READ_WRITE, NULL)
	call tbcfnd (tp, Memc[column], cp, 1)
	if (cp == NULL)
	    call tuniterr (nocolumn, Memc[column])

	# Read column units if old units are blank

	if (isblank (Memc[oldunits]))
	    call tbcigt (cp, TBL_COL_UNITS, Memc[oldunits], SZ_FNAME)

	call strlwr (Memc[oldunits])
	call strlwr (Memc[newunits])

	# Check to see if units are not blank

	if (isblank (Memc[oldunits]))
	    call tuniterr (unitblank, "oldunits")

	if (isblank (Memc[newunits]))
	    call tuniterr (unitblank, "newunits")

	# Check to see if column is floating point

	type = tbcigi (cp, TBL_COL_DATATYPE)
	if (type != TY_REAL && type != TY_DOUBLE)
	    call tuniterr (notfloat, Memc[column])

	# Read units and abbreviation tables into hashes

	ab = read_abrev (Memc[abrevtab])
	ut = read_units (ab, Memc[unittab])

	# Convert units to internal form

	punit1 = parse_units (ab, Memc[oldunits])
	punit2 = parse_units (ab, Memc[newunits])

	# Find conversion factor between units

	factor = find_factor (ut, punit1, punit2, verbose)

	# Apply conversion factor to table column

	call convert_col (tp, cp, Memc[newunits], factor)

	# Close table and free allocated memory

	call tbtclo (tp)

	call free_abrev (ab)
	call free_units (ut)

	call free_unstr (punit1)
	call free_unstr (punit2)

	call sfree (sp)
end
