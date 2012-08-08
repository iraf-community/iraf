include <ctype.h>
include <finfo.h>	# for file creation or modification time
include <time.h>
include <tbset.h>

# This file contains a set of routines for finding header keywords in a
# table.
#
# kw = tkw_open (tp)
# call tkw_close (kw)
# call tkw_find (tp, kw, keyword)
# nkw = tkw_len (kw)
# call tkw_reopen (tp, kw, keyword)
# call tkw_getkw (kw, k, keynum, keyword, maxch)
# call tkw_special (tp, keyword, value, maxch)
#
# Phil Hodge, 10-May-2000  Subroutines created.
# Phil Hodge, 31-May-2000  Add "keywords" i_nrows, etc.
# Phil Hodge, 19-Jul-2000  Add support for $I (equivalent to i_table).
# Phil Hodge, 15-Jul-2009  Remove ttype from calling sequence for tbparse.

define	NUM_SPECIAL	7		# number of keywords such as i_nrows
define	N_I_TABLE	1		# index number for i_table
define	N_I_FILE	2		# index number for i_file
define	N_I_CTIME	3
define	N_I_NROWS	4
define	N_I_NCOLS	5
define	N_I_NPAR	6
define	N_I_TYPE	7

define	SZ_KW_LIST	5
define	SZ_KW_SPACING	(SZ_KEYWORD+2)	# spacing of keywords in KW_NAME

define	NUM_KEYWORDS	Memi[$1]	# total number of keywords
define	NUM_MATCH	Memi[$1+1]	# number of keywords that match template
define	KW_NAME_PTR	Memi[$1+2]	# list of keyword names
define	KW_TYPE_PTR	Memi[$1+3]	# list of keyword data types
define	KW_MATCH_PTR	Memi[$1+4]	# indexes of keywords matching template
define	KW_NAME		Memc[KW_NAME_PTR($1) + ($2-1)*SZ_KW_SPACING]
define	KW_TYPE		Memi[KW_TYPE_PTR($1) + $2-1]
define	KW_MATCH	Memi[KW_MATCH_PTR($1) + $2-1]

# get list of all keywords in header

pointer procedure tkw_open (tp)

pointer tp		# i: pointer to table struct
#--
pointer kw		# o: pointer to keyword list struct
char	keyword[SZ_KEYWORD]	# current keyword
int	dtype			# data type of current keyword
char	value[SZ_PARREC]	# value of current keyword
int	i
int	npar			# number of keywords excluding i_nrows, etc
int	keynum			# index for keyword number
int	tbpsta()
errchk	tbhgnp

begin
	call malloc (kw, SZ_KW_LIST, TY_POINTER)

	npar = tbpsta (tp, TBL_NPAR)

	NUM_KEYWORDS(kw) = NUM_SPECIAL + npar
	NUM_MATCH(kw) = 0			# initial value

	call calloc (KW_NAME_PTR(kw),
		SZ_KW_SPACING * NUM_KEYWORDS(kw), TY_CHAR)

	call calloc (KW_TYPE_PTR(kw), NUM_KEYWORDS(kw), TY_INT)
	call calloc (KW_MATCH_PTR(kw), NUM_KEYWORDS(kw), TY_INT)

	# First assign names for the special keywords i_nrows, etc.
	# This list must agree with those in tkw_special, and the
	# number of such keywords must be no larger than NUM_SPECIAL.

	call strcpy ("i_table", KW_NAME(kw,N_I_TABLE), SZ_KEYWORD)
	KW_TYPE(kw,N_I_TABLE) = TY_CHAR

	call strcpy ("i_file", KW_NAME(kw,N_I_FILE), SZ_KEYWORD)
	KW_TYPE(kw,N_I_FILE) = TY_INT

	call strcpy ("i_ctime", KW_NAME(kw,N_I_CTIME), SZ_KEYWORD)
	KW_TYPE(kw,N_I_CTIME) = TY_INT

	call strcpy ("i_nrows", KW_NAME(kw,N_I_NROWS), SZ_KEYWORD)
	KW_TYPE(kw,N_I_NROWS) = TY_INT

	call strcpy ("i_ncols", KW_NAME(kw,N_I_NCOLS), SZ_KEYWORD)
	KW_TYPE(kw,N_I_NCOLS) = TY_INT

	call strcpy ("i_npar", KW_NAME(kw,N_I_NPAR), SZ_KEYWORD)
	KW_TYPE(kw,N_I_NPAR) = TY_INT

	call strcpy ("i_type", KW_NAME(kw,N_I_TYPE), SZ_KEYWORD)
	KW_TYPE(kw,N_I_TYPE) = TY_CHAR

	keynum = 1
	do i = NUM_SPECIAL+1, NUM_KEYWORDS(kw) {

	    call tbhgnp (tp, keynum, keyword, dtype, value)
	    call strcpy (keyword, KW_NAME(kw,i), SZ_KEYWORD)
	    KW_TYPE(kw,i) = dtype
	    keynum = keynum + 1
	}

	return (kw)
end

# free memory for keyword list

procedure tkw_close (kw)

pointer kw		# io: pointer to keyword list struct

begin
	if (kw != NULL) {
	    call mfree (KW_NAME_PTR (kw), TY_CHAR)
	    call mfree (KW_TYPE_PTR (kw), TY_INT)
	    call mfree (KW_MATCH_PTR (kw), TY_INT)
	    call mfree (kw, TY_POINTER)
	    kw = NULL
	}
end

# expand template for current keyword
# This can be called repeatedly after tkw_open.  Each time it is called,
# the previous list will be overwritten.

procedure tkw_find (tp, kw, keyword)

pointer tp		# i: pointer to table struct
pointer kw		# i: pointer to keyword list struct
char	keyword[ARB]	# i: keyword name template
#--
pointer sp
pointer template	# keyword converted to upper case, etc
char	pat[SZ_FNAME]	# encoded pattern
int	lenpat
int	k		# counter for keywords that match template
int	i, nmatch
int	strlen()
int	patmake(), pat_amatch()
errchk	uc_template, patmake, pat_amatch

begin
	call smark (sp)
	call salloc (template, SZ_FNAME, TY_CHAR)

	# Convert the keyword to upper case (except for special keywords).
	call uc_template (keyword, Memc[template], SZ_FNAME)

	lenpat = patmake (Memc[template], pat, SZ_FNAME)

	k = 0
	do i = 1, NUM_KEYWORDS(kw) {

	    if (strlen (KW_NAME(kw,i)) < 1)	# ignore blank keywords
		next

	    nmatch = pat_amatch (KW_NAME(kw,i), 1, pat)
	    if (nmatch == strlen (KW_NAME(kw,i))) {
		k = k + 1
		KW_MATCH(kw,k) = i
	    }
	}
	NUM_MATCH(kw) = k

	call sfree (sp)
end

# get all current keywords and expand template again

procedure tkw_reopen (tp, kw, keyword)

pointer tp		# i: pointer to table struct
pointer kw		# io: pointer to keyword list struct
char	keyword[ARB]	# i: keyword name template
#--
pointer tkw_open()

begin
	call tkw_close (kw)
	kw = tkw_open (tp)
	call tkw_find (tp, kw, keyword)
end

# This routine converts the keyword template to upper case (except for
# special keywords) and replaces "*" with "?*" for use with patmake.

procedure uc_template (keyword, template, maxch)

char	keyword[ARB]	# i: keyword template
char	template[ARB]	# o: template converted to upper case
int	maxch		# i: max length of template string
#--
char	ch
int	ip, op
int	strncmp()
bool	streq()

begin
	# Make "$I" equivalent to i_table.
	if (streq (keyword, "$I")) {
	    call strcpy ("i_table", template, maxch)
	    return
	}

	# Copy special keywords to output without change.
	if (strncmp (keyword, "i_", 2) == 0) {
	    call strcpy (keyword, template, maxch)
	    return
	}

	ip = 1
	op = 1
	ch = keyword[ip]

	while (ch != EOS) {

	    # Map "*" into "?*".
	    if (ch == '*' && ip > 1) {
		template[op] = '?'
		op = op + 1
	    }

	    if (op > maxch)
		call error (1, "keyword template string is too long")

	    if (IS_LOWER(ch))
		template[op] = TO_UPPER(ch)
	    else
		template[op] = ch

	    op = op + 1
	    ip = ip + 1
	    ch = keyword[ip]
	}
	template[op] = EOS
end

# This function returns the number of keywords that matched the template,
# i.e. after calling tkw_find.

int procedure tkw_len (kw)

pointer kw		# i: pointer to keyword list struct

begin
	return (NUM_MATCH(kw))
end

# This routine can be used to loop through the list of matched keywords,
# returning the keyword number and name.  k is the index in the list of
# keywords that match the template, and keynum is the index of the keyword
# in the header.  k runs from 1 to tkw_len.  keynum can be passed to
# tbhgnp to get a keyword or to tbhdel to delete a keyword.

procedure tkw_getkw (kw, k, keynum, keyword, maxch)

pointer kw		# i: pointer to keyword list struct
int	k		# i: index of keyword in list of match keywords
int	keynum		# o: keyword number in header
char	keyword[ARB]	# o: keyword name
int	maxch		# i: max length of keyword string
#--
int	knum

begin
	if (k < 1 || k > NUM_MATCH(kw))
	    call error (1, "tkw_getkw:  index is out of range")

	knum = KW_MATCH(kw,k)
	keynum = knum - NUM_SPECIAL

	call strcpy (KW_NAME(kw,knum), keyword, maxch)
end

# This routine returns the value of one of the special keywords.

procedure tkw_special (tp, keyword, value, maxch)

pointer tp			# i: pointer to table struct
char	keyword[ARB]		# i: current keyword
char	value[ARB]		# o: value of keyword
int	maxch			# i: size of value string
#--
pointer sp
pointer tablename		# name of table
pointer filename		# name of table without brackets
pointer hduname			# returned by tbparse and ignored
int	hdu			# ignored
int	junk, tbparse()
long	ostruct[LEN_FINFO]	# contains info about file
long	ctime			# creation or modification time
char	datestr[SZ_TIME]	# ctime converted to a string
int	finfo()
int	tbltype, tbl_subtype
int	tbpsta()
bool	streq()

begin
	call smark (sp)

	if (streq (keyword, "$I") || streq (keyword, "i_table")) {

	    # The table name.
	    call tbtnam (tp, value, maxch)

	} else if (streq (keyword, "i_file")) {

	    # The name of the file containing the table.
	    call salloc (tablename, SZ_FNAME, TY_CHAR)
	    call salloc (filename, SZ_FNAME, TY_CHAR)
	    call salloc (hduname, SZ_FNAME, TY_CHAR)

	    call tbtnam (tp, Memc[tablename], SZ_FNAME)
	    junk = tbparse (Memc[tablename], Memc[filename],
			Memc[hduname], SZ_FNAME, hdu)
	    call strcpy (Memc[filename], value, maxch)

	} else if (streq (keyword, "i_ctime")) {

	    # The time the file was created (or last modified).
	    call salloc (tablename, SZ_FNAME, TY_CHAR)
	    call salloc (filename, SZ_FNAME, TY_CHAR)
	    call salloc (hduname, SZ_FNAME, TY_CHAR)

	    # Get file name.
	    call tbtnam (tp, Memc[tablename], SZ_FNAME)
	    junk = tbparse (Memc[tablename], Memc[filename],
			Memc[hduname], SZ_FNAME, hdu)

	    if (finfo (Memc[filename], ostruct) == ERR)
		call error (1, "Can't get info about file")

	    ctime = FI_CTIME(ostruct)
	    call cnvtime (ctime, datestr, SZ_TIME)
	    call strcpy (datestr, value, maxch)

	} else if (streq (keyword, "i_nrows")) {

	    # The number of rows in the table.
	    call sprintf (value, maxch, "%d")
		call pargi (tbpsta (tp, TBL_NROWS))

	} else if (streq (keyword, "i_ncols")) {

	    # The number of columns in the table.
	    call sprintf (value, maxch, "%d")
		call pargi (tbpsta (tp, TBL_NCOLS))

	} else if (streq (keyword, "i_npar")) {

	    # The number of header keywords in the table.
	    call sprintf (value, maxch, "%d")
		call pargi (tbpsta (tp, TBL_NPAR))

	} else if (streq (keyword, "i_type")) {

	    # The type of the table.
	    tbltype = tbpsta (tp, TBL_WHTYPE)
	    tbl_subtype = tbpsta (tp, TBL_SUBTYPE)

	    if (tbltype == TBL_TYPE_TEXT) {

		if (tbl_subtype == TBL_SUBTYPE_SIMPLE)
		    call strcpy ("text", value, maxch)
		else if (tbl_subtype == TBL_SUBTYPE_EXPLICIT)
		    call strcpy ("text with explicit column definitions",
				value, maxch)
		else
		    call strcpy ("text", value, maxch)

	    } else if (tbltype == TBL_TYPE_FITS) {

		if (tbl_subtype == TBL_SUBTYPE_ASCII)
		    call strcpy ("fits ascii", value, maxch)
		else if (tbl_subtype == TBL_SUBTYPE_BINTABLE)
		    call strcpy ("fits binary", value, maxch)
		else if (tbl_subtype == TBL_SUBTYPE_IMAGE)
		    call strcpy ("fits primary header", value, maxch)
		else
		    call strcpy ("fits", value, maxch)

	    } else if (tbltype == TBL_TYPE_S_ROW) {

		call strcpy ("stsdas row ordered", value, maxch)

	    } else if (tbltype == TBL_TYPE_S_COL) {

		call strcpy ("stsdas column ordered", value, maxch)

	    } else {
		call strcpy ("unknown", value, maxch)
	    }

	} else {

	    call sfree (sp)
	    call error (1, "not a special keyword")
	}

	call sfree (sp)
end
