include	<ctype.h>

define	MAX_RANGES	100

define	ODR_LEN		5		# Length of ODR structure
define	ODR_LIST	Memi[$1]	# Image list
define	ODR_NUM		Memi[$1+1]	# Current record number
define	ODR_RGS		Memi[$1+2]	# Pointer to record ranges
define	ODR_NRGS	Memi[$1+3]	# Number of records
define	ODR_IMAGE	Memi[$1+4]	# Pointer to image string


#	 ONEDSPEC RECORD FORMAT IMAGE PACKAGE.
#
# This package provides:
#    1.  Image names stripped of the image extensions ".imh" or ".hhh"
#    2.  Provides a record extension template as introduced in ONEDSPEC.
#
# ODR_OPEN  -- Open the ONEDSPEC record format image package.
# ODR_OPEN1 -- Open a ODR list  given CL parameters.
# ODR_OPEN2 -- Open 2 ODR lists given CL parameters with common records.
# ODR_CLOSE -- Close the ODR structure.
# ODR_REW   -- Rewind image list.
# ODR_LEN   -- Length of ODR list.
# ODR_GETIM -- Get next ONEDSPEC record format image name.


# ODR_OPEN -- Open the ONEDSPEC record image package.
# This package is based on the IMT and XTOOLS RANGES packages.

procedure odr_open (list, recs, odr)

char	list[ARB]		# Image list
char	recs[ARB]		# Record string
pointer	odr			# ODR pointer (returned)

int	i, imtopen(), decode_ranges()
pointer	rgs

begin
	call malloc (odr, ODR_LEN, TY_STRUCT)
	ODR_LIST(odr) = imtopen (list)
	ODR_NUM(odr) = -1
	ODR_RGS(odr) = NULL
	ODR_NRGS(odr) = 1
	ODR_IMAGE(odr) = NULL

	for (i=1; IS_WHITE(recs[i]); i=i+1)
	    ;
	if (recs[i] == EOS)
	    return

	call malloc (rgs, 3*MAX_RANGES, TY_INT)
	if (decode_ranges (recs, Memi[rgs], MAX_RANGES, i) == ERR) {
	    call mfree (rgs, TY_INT)
	    call mfree (odr, TY_STRUCT)
	    call error (0, "Bad range specification")
	}

	ODR_RGS(odr) = rgs
	ODR_NRGS(odr) = i
	call malloc (ODR_IMAGE(odr), SZ_FNAME, TY_CHAR)
end


# ODR_OPEN1 -- Open a single ODR list given CL parameters.
# If the record parameter is null then no record extensions are used.

procedure odr_open1 (param, record, odr)

char	param[ARB]		# Image list parameter
char	record[ARB]		# Record format parameter
pointer	odr			# ODR pointer (returned)

pointer	sp, list, records

begin
	call smark (sp)
	call salloc (list, SZ_LINE, TY_CHAR)
	call salloc (records, SZ_LINE, TY_CHAR)
	Memc[records] = EOS

	call clgstr (param, Memc[list], SZ_LINE)
	if (record[1] != EOS)
	    call clgstr (record, Memc[records], SZ_LINE)

	call odr_open (Memc[list], Memc[records], odr)

	call sfree (sp)
end


# ODR_OPEN2 -- Open two ODR lists given CL parameters  with common records.
# If the record parameter is null then no record extensions are used.

procedure odr_open2 (param1, param2, record, odr1, odr2)

char	param1[ARB]		# First image list parameter
char	param2[ARB]		# Second image list parameter
char	record[ARB]		# Record parameter
pointer	odr1			# First ODR pointer (returned)
pointer	odr2			# Second ODR pointer (returned)

pointer	sp, list1, list2, records

begin
	call smark (sp)
	call salloc (list1, SZ_LINE, TY_CHAR)
	call salloc (list2, SZ_LINE, TY_CHAR)
	call salloc (records, SZ_LINE, TY_CHAR)
	Memc[records] = EOS

	call clgstr (param1, Memc[list1], SZ_LINE)
	call clgstr (param2, Memc[list2], SZ_LINE)
	if (record[1] != EOS)
	    call clgstr (record, Memc[records], SZ_LINE)

	call odr_open (Memc[list1], Memc[records], odr1)
	call odr_open (Memc[list2], Memc[records], odr2)

	call sfree (sp)
end


# ODR_CLOSE -- Close the ODR structure.

procedure odr_close (odr)

pointer	odr			# ODR pointer

begin
	call imtclose (ODR_LIST(odr))
	call mfree (ODR_RGS(odr), TY_INT)
	call mfree (ODR_IMAGE(odr), TY_CHAR)
	call mfree (odr, TY_STRUCT)
end


# ODR_REW -- Rewind ODR list.

procedure odr_rew (odr)

pointer	odr			# ODR pointer

begin
	call imtrew (ODR_LIST(odr))
	ODR_NUM(odr) = -1
end


# ODR_LEN -- Length of ODR list.

int procedure odr_len (odr)

pointer	odr			# ODR pointer
int	imtlen()

begin
	return (imtlen (ODR_LIST(odr)) * ODR_NRGS(odr))
end


# ODR_GETIM -- Get next ONEDSPEC record image name.
# Return EOF when done.

int procedure odr_getim (odr, image, maxchar)

pointer	odr			# ODR pointer
char	image[maxchar]		# Next image name
int	maxchar			# Maximum number of chars in image name

int	i, stat, imtgetim(), strmatch(),  get_next_number()

begin
	repeat {
	    if (ODR_NUM(odr) < 0) {
	        stat = imtgetim (ODR_LIST(odr), image, maxchar)
	        if (stat == EOF)
		    break

	        i = strmatch (image, ".imh")
	        if (i > 0)
	            call strcpy (image[i], image[i-4], maxchar)
	        i = strmatch (image, ".hhh")
	        if (i > 0)
	            call strcpy (image[i], image[i-4], maxchar)

	        if (ODR_RGS(odr) == NULL)
		    break
	    
	        call strcpy (image, Memc[ODR_IMAGE(odr)], SZ_FNAME)
	    }

	    stat = get_next_number (Memi[ODR_RGS(odr)], ODR_NUM(odr))
	    if (stat != EOF) {
	        call sprintf (image, maxchar, "%s.%04d")
		    call pargstr (Memc[ODR_IMAGE(odr)])
		    call pargi (ODR_NUM(odr))
	        break
	    }
	    ODR_NUM(odr) = -1
	}

	return (stat)
end
