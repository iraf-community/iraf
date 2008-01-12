include	<tbset.h>

#  TIHEADER  --  Routines for retrieving header-stored scalars.
#
#  Details such as keyword names and encoding are defined by the
#  way task txtable creates the same keywords.
#
#
#
#  TIHKI   --  Look for keyword and return integer value, or 0 if not found.
#  TIHMAX  --  Return maximum number of header-stored scalars.
#  TIHNSC  --  Return actual number of scalars in header.
#  TIHROW  --  Return original row value stored by txtable task.
#  TIHDEC  --  Decode column description in header keyword.
#
#
#
#  Revision history:
#  ----------------
#  20-Jan-97  -  Task created (I.Busko)
#  17-Mar-97  -  Revised after code review (IB)



#  TIHMAX  --  Return maximum number of header-stored scalars.

int procedure tihmax (tp)

pointer	tp		# table pointer

int	tihki()

begin
	return (tihki (tp, "TCTOTAL"))
end




#  TIHROW  --  Return original row value (stored by txtable task).

int procedure tihrow (tp)

pointer	tp		# table pointer

int	tihki()

begin
	return (tihki (tp, "ORIG_ROW"))
end




#  TIHNSC  --  Return actual number of scalars in header.

int procedure tihnsc (tp)

pointer	tp		# table pointer
#--
pointer	sp, kwname, kwval
int	dtype, parnum
int	i, ntot, nscalar

int	tihmax()

begin
	call smark (sp)
	call salloc (kwval,  SZ_PARREC, TY_CHAR)
	call salloc (kwname, SZ_LINE,   TY_CHAR)
	nscalar = 0

	ntot = tihmax (tp)
	do i = 1, ntot {
	    call sprintf (kwname, SZ_LINE, "TCD_%03d")
	        call pargi (i)
	    call tbhfkr (tp, kwname, dtype, kwval, parnum)
	    if (parnum > 0)
                nscalar = nscalar + 1
	}

	call sfree (sp)
	return (nscalar)
end





#  TIHDEC  --  Decode column description in header keyword. The detailed
#              format depends on how task txtable does the encoding.

bool procedure tihdec (tp, kn, colname, colunits, colfmt, datatype, lenfmt)

pointer	tp			# i: table pointer
int	kn			# i: keyword number
char	colname[ARB]		# o: column name
char	colunits[ARB]		# o: column units
char	colfmt[ARB]		# o: column print format
int	datatype		# o: column data type
int	lenfmt			# o: format lenght
#--
pointer	sp, kwname, kwval, dtype
int	parnum
bool	found

string	corrupt  "Corrupted header in input table."

int	nscan(), strncmp()
bool	streq()

begin
	call smark (sp)
	call salloc (kwval,  SZ_PARREC, TY_CHAR)
	call salloc (kwname, SZ_LINE,   TY_CHAR)
	call salloc (dtype,  SZ_LINE,   TY_CHAR)

	# Build column description keyword name.
	call sprintf (Memc[kwname], SZ_LINE, "TCD_%03d")
	    call pargi (kn)

	# Look for it.
	call tbhfkr (tp, Memc[kwname], datatype, Memc[kwval], parnum)

	if (parnum > 0) {

	    # Found; parse the 5 fields.
	    call sscan (Memc[kwval])
	    call gargwrd (colname, SZ_COLNAME) 
	    if (nscan() < 1) call error (1, corrupt)
	    call gargwrd (colunits, SZ_COLUNITS)
	    if (nscan() < 1) call error (1, corrupt)
	    call gargwrd (colfmt, SZ_COLFMT)
	    if (nscan() < 1) call error (1, corrupt)
	    call gargwrd (Memc[dtype], SZ_LINE)
	    if (nscan() < 1) call error (1, corrupt)
	    call gargi (lenfmt)
	    if (nscan() < 1) call error (1, corrupt)

	    # Translate from human-readable encoding to sdas table encoding.
	    if (streq (colunits, "default")) 
	        call strcpy ("", colunits, SZ_COLUNITS)
	    if (streq (colfmt, "default")) 
	        call strcpy ("", colfmt, SZ_COLFMT)
	    if (streq (Memc[dtype], "boolean")) datatype = TY_BOOL
	    if (streq (Memc[dtype], "short"))   datatype = TY_SHORT
	    if (streq (Memc[dtype], "integer")) datatype = TY_INT
	    if (streq (Memc[dtype], "long"))    datatype = TY_LONG
	    if (streq (Memc[dtype], "real"))    datatype = TY_REAL
	    if (streq (Memc[dtype], "double"))  datatype = TY_DOUBLE
	    if (strncmp (Memc[dtype], "character_", 10) == 0) { 
	        call sscan (Memc[dtype+10])
	            call gargi (datatype)
	        datatype = -datatype
	    }
	    found = true
	} else 
	    found = false

	call sfree (sp)
	return (found)
end




#  TIHKI  --  Look for keyword and return integer value, or 0 if not found.
#             Zero is never expected as a valid result because this routine
#             is used to retrieve either the maximum number of header-stored
#             scalars (zero means no scalars) or the original table row number.

int procedure tihki (tp, keyword)

pointer	tp		# table pointer
char	keyword[ARB]	# keyword
#--
pointer	sp, kwval
int	dtype, parnum, par

int	tbhgti()

begin
	call smark (sp)
	call salloc (kwval, SZ_PARREC, TY_CHAR)
	call tbhfkr (tp, keyword, dtype, kwval, parnum)
	if (parnum > 0)
	    par = tbhgti (tp, keyword)
	else
	    par = 0
	call sfree (sp)
	return (par)
end
