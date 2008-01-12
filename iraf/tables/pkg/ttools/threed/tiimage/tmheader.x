include <tbset.h>

#  TM_HEADER  --  Decode column info in image header.
#
#
#
#
#  Revision history:
#  ----------------
#  30-Jan-97  -  Task created (I.Busko)
#  21-May-97  -  Changes from code review (IB)


procedure tm_header (im, colname, colunits, colfmt)

pointer	im			# image pointer
char	colname[SZ_COLNAME]	# column name	
char	colunits[SZ_COLUNITS]	# column units	
char	colfmt[SZ_COLFMT]	# column print format
#--
pointer	sp, kwval
int	colnum

string	corrupt  "Corrupted header in input image."

bool	streq()
int	imaccf(), nscan()

begin
	if (imaccf (im, "COLDATA") == NO)
	    call error (1, "No column information in image header.")

	call smark (sp)
	call salloc (kwval, SZ_LINE, TY_CHAR)

	# Get keyword value.
	call imgstr (im, "COLDATA", Memc[kwval], SZ_LINE)

	# Read fields.
	call sscan (Memc[kwval])
	call gargi (colnum)
	if (nscan() < 1) call error (1, corrupt)
	call gargwrd (colname, SZ_COLNAME) 
	if (nscan() < 1) call error (1, corrupt)
	call gargwrd (colunits, SZ_COLUNITS)
	if (nscan() < 1) call error (1, corrupt)
	call gargwrd (colfmt, SZ_COLFMT)
	if (nscan() < 1) call error (1, corrupt)

	# Decode custom-encoded values.
	if (streq (colunits, "default"))
	    colunits[1] = EOS
	if (streq (colfmt, "default"))
	    colfmt[1] = EOS

	call sfree (sp)
end



