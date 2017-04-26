include <error.h>
include <ctotok.h>
include "pexamine.h"

# PT_INIT - Initialize the pexamine structure.

pointer procedure pt_init (photcols, usercols, xcol, ycol, xpos, ypos, hcol)

char	photcols[ARB]		# the list of photometry columns
char	usercols[ARB]		# the list of user columns
char	xcol[ARB]		# the name of the x column
char	ycol[ARB]		# the name of the y column
char	xpos[ARB]		# the name of the x coord column
char	ypos[ARB]		# the name of the y coord column
char	hcol[ARB]		# the name of the histogram column

pointer	px
bool	streq()
int	strdic()

begin
	# Preload the daophot and apphot photometry fields.
	if (streq ("DAOPHOT", photcols) || streq ("daophot", photcols))
	    call strcpy (PX_DAOCOLS, photcols, PX_SZCOLNAME * (PX_MAXNCOLS + 1))
	else if (streq ("APPHOT", photcols) || streq ("apphot", photcols))
	    call strcpy (PX_APCOLS, photcols, PX_SZCOLNAME * (PX_MAXNCOLS + 1))

	# Allocate space for the pexamine structure and the column lists.
	call malloc (px, LEN_PXSTRUCT, TY_STRUCT)

	# Initialize the requested column information.
	PX_RNPHOT(px) = 0; PX_RNUSER(px) = 0; PX_RNCOLS(px) = 0
	call malloc (PX_RCOLNAMES(px), PX_SZCOLNAME * (PX_MAXNCOLS + 1),
	    TY_CHAR)
	Memc[PX_RCOLNAMES(px)] = EOS

	# Initialize the stored column information.
	PX_NPHOT(px) = 0; PX_NUSER(px) = 0; PX_NCOLS(px) = 0
	call malloc (PX_COLNAMES(px), PX_SZCOLNAME * (PX_MAXNCOLS + 1),
	    TY_CHAR)
	Memc[PX_COLNAMES(px)] = EOS

	# Decode the column strings. Check that the number of columns
	# does not exceed the maximum number permitted, by extracting
	# the column names one by one from the photometry and user column
	# strings.

	call pt_setnames (px, photcols, usercols)

	# Convert all the input column name specifications to upper case.
	call strupr (xcol)
	call strupr (ycol)
	call strupr (xpos)
	call strupr (ypos)
	call strupr (hcol)

	# Decode the x and y columns.
	if (strdic (xcol, PX_RXCOLNAME(px), PX_SZCOLNAME,
	    Memc[PX_RCOLNAMES(px)]) <= 0)
	    call strcpy (xcol, PX_RXCOLNAME(px), PX_SZCOLNAME)
	if (strdic (ycol, PX_RYCOLNAME(px), PX_SZCOLNAME,
	    Memc[PX_RCOLNAMES(px)]) <= 0)
	    call strcpy (ycol, PX_RYCOLNAME(px), PX_SZCOLNAME)

	# Decode the y and y coordinate column names.
	if (strdic (xpos, PX_RXPOSNAME(px), PX_SZCOLNAME,
	    Memc[PX_RCOLNAMES(px)]) <= 0)
	    call strcpy (xpos, PX_RXPOSNAME(px), PX_SZCOLNAME)
	if (strdic (ypos, PX_RYPOSNAME(px), PX_SZCOLNAME,
	    Memc[PX_RCOLNAMES(px)]) <= 0)
	    call strcpy (ypos, PX_RYPOSNAME(px), PX_SZCOLNAME)

	# Decode the histogram column name.
	if (strdic (hcol, PX_RHCOLNAME(px), PX_SZCOLNAME,
	    Memc[PX_RCOLNAMES(px)]) <= 0)
	    call strcpy (hcol, PX_RHCOLNAME(px), PX_SZCOLNAME)

	# Allocate space for the pointers and initialize them to NULL.
	call malloc (PX_COLPTRS(px), PX_MAXNCOLS, TY_POINTER)
	call amovki (NULL, Memi[PX_COLPTRS(px)], PX_MAXNCOLS) 

	return (px)
end


# PT_FREE -- Free memory used by the pexamine task.

procedure pt_free (px)

pointer	px

int	i

begin
	# Free the column lists.
	if (PX_RCOLNAMES(px) != NULL)
	    call mfree (PX_RCOLNAMES(px), TY_CHAR)
	if (PX_COLNAMES(px) != NULL)
	    call mfree (PX_COLNAMES(px), TY_CHAR)

	# Free the column pointers.
	do i = 1, PX_MAXNCOLS {
	    if (Memi[PX_COLPTRS(px)+i-1] != NULL)
		call mfree (Memi[PX_COLPTRS(px)+i-1], TY_REAL)
	}
	if (PX_COLPTRS(px) != NULL)
	    call mfree (PX_COLPTRS(px), TY_POINTER)

	# Free the pexamine structure.
	call mfree (px, TY_STRUCT)
end


# PT_SETNAMES -- Decode the photometry and user columns.

procedure pt_setnames (px, photcols, usercols)

pointer	px			# pointer to the pexamine strucuture
char	photcols[ARB]		# list of photometry columns
char	usercols[ARB]		# list of user columns

int	ip, nphot
pointer	sp, name
int	pt_getnames()

begin
	call smark (sp)
	call salloc (name, PX_SZCOLNAME, TY_CHAR)

	call strupr (photcols)
	call strupr (usercols)
	Memc[PX_RCOLNAMES(px)] = EOS

	ip = 1
	nphot = 0
	while (pt_getnames (photcols, ip, Memc[name], PX_SZCOLNAME) != EOF) {
	    if (nphot >= PX_MAXNCOLS)
		break
	    #if (Memc[name] == EOS)
		#next
	    call strcat (",", Memc[PX_RCOLNAMES(px)], PX_SZCOLNAME *
		(PX_MAXNCOLS + 1))
	    call strcat (Memc[name], Memc[PX_RCOLNAMES(px)], PX_SZCOLNAME *
		(PX_MAXNCOLS + 1))
	    nphot = nphot + 1
	}
	PX_RNPHOT(px) = nphot

	# Decode the user columns.
	ip = 1
	while (pt_getnames (usercols, ip, Memc[name], PX_SZCOLNAME) != EOF) {
	    if (nphot >= PX_MAXNCOLS)
		break
	    #if (Memc[name] == EOS)
		#next
	    call strcat (",", Memc[PX_RCOLNAMES(px)], PX_SZCOLNAME *
	        (PX_MAXNCOLS + 1))
	    call strcat (Memc[name], Memc[PX_RCOLNAMES(px)], PX_SZCOLNAME *
		(PX_MAXNCOLS + 1))
	    nphot = nphot + 1
	}
	PX_RNUSER(px) = nphot - PX_RNPHOT(px)

	PX_RNCOLS(px) = nphot

	call sfree (sp)
end


# PT_GPHOTCOLS -- Extract the requested and stored photometric columns
# from the pexamine structure.

procedure pt_gphotcols (px, rphotcols, rnphot, photcols, nphot)

pointer	px		# pointer to the pexamine structure
char	rphotcols[ARB]	# list of requested photometric columns
int	rnphot		# number of requested photometric columns
char	photcols[ARB]	# list of photometric columns
int	nphot		# number of photometric columns

int	ip, ncols
pointer	sp, name
int	pt_getnames()

begin
	call smark (sp)
	call salloc (name, PX_SZCOLNAME, TY_CHAR)

	ip = 1
	ncols = 0
	rphotcols[1] = EOS
	while (pt_getnames (Memc[PX_RCOLNAMES(px)], ip, Memc[name],
	    PX_SZCOLNAME) != EOF) {
	    #if (Memc[name] == EOS)
		#next
	    ncols = ncols + 1
	    if (ncols > PX_RNPHOT(px))
		break
	    call strcat (",", rphotcols, PX_SZCOLNAME * (PX_MAXNCOLS + 1))
	    call strcat (Memc[name], rphotcols, PX_SZCOLNAME *
	        (PX_MAXNCOLS + 1))
	}
	rnphot = PX_RNPHOT(px)

	ip = 1
	ncols = 0
	photcols[1] = EOS
	while (pt_getnames (Memc[PX_COLNAMES(px)], ip, Memc[name],
	    PX_SZCOLNAME) != EOF) {
	    #if (Memc[name] == EOS)
		#next
	    ncols = ncols + 1
	    if (ncols > PX_NPHOT(px))
		break
	    call strcat (",", photcols, PX_SZCOLNAME * (PX_MAXNCOLS + 1))
	    call strcat (Memc[name], photcols, PX_SZCOLNAME *
	        (PX_MAXNCOLS + 1))
	}
	nphot = PX_NPHOT(px)

	call sfree (sp)
end


# PT_GUSERCOLS -- Extract the requested and stored user columns
# from the pexamine structure.

procedure pt_gusercols (px, rusercols, rnuser, usercols, nuser)

pointer	px		# pointer to the pexamine structure
char	rusercols[ARB]	# list of requested user columns
int	rnuser		# number of requested user columns
char	usercols[ARB]	# list of user columns
int	nuser		# number of user columns

int	ip, ncols
pointer	sp, name
int	pt_getnames()

begin
	call smark (sp)
	call salloc (name, PX_SZCOLNAME, TY_CHAR)

	ip = 1
	ncols = 0
	rusercols[1] = EOS
	while (pt_getnames (Memc[PX_RCOLNAMES(px)], ip, Memc[name],
	    PX_SZCOLNAME) != EOF) {
	    #if (Memc[name] == EOS)
		#next
	    ncols = ncols + 1
	    if (ncols <= PX_RNPHOT(px))
		next
	    call strcat (",", rusercols, PX_SZCOLNAME * (PX_MAXNCOLS + 1))
	    call strcat (Memc[name], rusercols, PX_SZCOLNAME *
	        (PX_MAXNCOLS + 1))
	}
	rnuser = PX_RNUSER(px)

	ip = 1
	ncols = 0
	usercols[1] = EOS
	while (pt_getnames (Memc[PX_COLNAMES(px)], ip, Memc[name],
	    PX_SZCOLNAME) != EOF) {
	    #if (Memc[name] == EOS)
		#next
	    ncols = ncols + 1
	    if (ncols <= PX_NPHOT(px))
		next
	    call strcat (",", usercols, PX_SZCOLNAME * (PX_MAXNCOLS + 1))
	    call strcat (Memc[name], usercols, PX_SZCOLNAME *
	        (PX_MAXNCOLS + 1))
	}
	nuser = PX_NUSER(px)

	call sfree (sp)
end


# PT_LCOLS -- List the requested and stored columns with an optional
# title string.

procedure pt_lcols (title, rcols, rncols, cols, ncols)

char	title[ARB]		# title string for column listing
char	rcols[ARB]		# list of requested columns
int	rncols			# the number of requested columns
char	cols[ARB]		# list of stored columns
int	ncols			# the number of stored columns

int	ip1, ip2, i
pointer	sp, name1, name2
int	pt_getnames()

begin
	call smark (sp)
	call salloc (name1, PX_SZCOLNAME, TY_CHAR)
	call salloc (name2, PX_SZCOLNAME, TY_CHAR)

	call printf ("\n%s\n\n")
	    call pargstr (title)

	ip1 = 1
	ip2 = 1
	do i = 1, max (rncols, ncols) {
	    if (pt_getnames (rcols, ip1, Memc[name1], PX_SZCOLNAME) == EOF)
		Memc[name1] = EOS
	    if (pt_getnames (cols, ip2, Memc[name2], PX_SZCOLNAME) == EOF)
		Memc[name2] = EOS
	    call printf ("    requested: %*.*s    stored: %*.*s\n")
		call pargi (-PX_SZCOLNAME)
		call pargi (PX_SZCOLNAME)
		call pargstr (Memc[name1])
		call pargi (-PX_SZCOLNAME)
		call pargi (PX_SZCOLNAME)
		call pargstr (Memc[name2])
	}

	call sfree (sp)
end


# PT_GETNAMES -- Decode the list of column names into list of column names.

int procedure pt_getnames (colnames, ip, name, maxch)

char	colnames[ARB]		# list of column names	
int	ip			# pointer in to the list of names
char	name[ARB]		# the output column name
int	maxch			# maximum length of a column name

int	op, token
int	ctotok(), strlen()

begin
	# Decode the column labels.
	op = 1
	while (colnames[ip] != EOS) {

	    token = ctotok (colnames, ip, name[op], maxch)
	    if (name[op] == EOS)
		next
	    if ((token == TOK_UNKNOWN) || (token == TOK_CHARCON))
		break
	    if ((token == TOK_PUNCTUATION) && (name[op] == ',')) {
		if (op == 1)
		    next
		else
		    break
	    }

	    op = op + strlen (name[op])
	}

	name[op] = EOS
	if ((colnames[ip] == EOS) && (op == 1))
	    return (EOF)
	else
	    return (op - 1)
end
