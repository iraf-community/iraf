include		<ctype.h>
include		"cqdef.h"
include		"cq.h"

# These are the catalog configuration file access routines used by the
# catalog access interface. These routines should not normally be called
# directly from the applications program.


# CQ_DGETI -- Get an integer field from the database record.

int procedure cq_dgeti (cq, record, field)

pointer	cq				#I The catalog database descriptor
int	record				#I The catalog record index
char	field[ARB]			#I The record field

int	ival				#O Field value
char	name[SZ_LINE]

int	fscan(), nscan()
bool	streq()

begin
	if ((record < 1) || (record > CQ_NRECS(cq)))
	    call error (0, "The catalog record is out of bounds")

	call seek (CQ_FD(cq), CQ_OFFSET(cq, record))

	while (fscan (CQ_FD(cq)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargi (ival)
		if (nscan() == 2)
		   return (ival)
		else
		   call error (0, "Error reading catalog integer field value")
	    }
	}

	call error (0, "Catalog record field not found")
end


# CQ_DGETR -- Get a real field from the catalog database record.

real procedure cq_dgetr (cq, record, field)

pointer	cq				#I The catalog database descriptor
int	record				#I The catalog database record index
char	field[ARB]			#I The catalog record field

real	rval
char	name[SZ_LINE]

int	fscan(), nscan()
bool	streq()

begin
	if ((record < 1) || (record > CQ_NRECS(cq)))
	    call error (0, "The catalog record is out of bounds")

	call seek (CQ_FD(cq), CQ_OFFSET(cq, record))

	while (fscan (CQ_FD(cq)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargr (rval)
		if (nscan() == 2)
		   return (rval)
		else
		   call error (0, "Error reading real catalog field value")
	    }
	}

	call error (0, "Catalog record field not found")
end


# CQ_DGETD -- Get a double precision field from a record.

double procedure cq_dgetd (cq, record, field)

pointer cq                              #I The catalog database descriptor
int     record                          #I The catalog database index
char    field[ARB]                      #I The catalog database field

double  dval
char    name[SZ_LINE]

int     fscan(), nscan()
bool    streq()

begin
        if ((record < 1) || (record > CQ_NRECS(cq)))
            call error (0, "The catalog record is out of bounds")

        call seek (CQ_FD(cq), CQ_OFFSET(cq, record))

        while (fscan (CQ_FD(cq)) != EOF) {
            call gargwrd (name, SZ_LINE)

            if (streq (name, "begin"))
                break
            else if (streq (name, field)) {
                call gargd (dval)
                if (nscan() == 2)
                   return (dval)
                else
                   call error (0, "Error reading double catalog field value")
            }
        }

        call error (0, "Catalog record field not found")
end


# CQ_DGWRD -- Get a string field from the database file.

procedure cq_dgwrd (cq, record, field, str, maxchar)

pointer	cq				#I The catalog access descriptor
int	record				#I The catalog record index
char	field[ARB]			#I The field name
char	str[maxchar]			#O The output string value
int	maxchar				#I The maximum characters for string

char	name[SZ_LINE]
int	i, fscan()
bool	streq()

begin
	if ((record < 1) || (record > CQ_NRECS(cq)))
	    call error (0, "Catalog record is out of bounds")

	call seek (CQ_FD(cq), CQ_OFFSET(cq, record))

	while (fscan (CQ_FD(cq)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargwrd (str, maxchar)
		for (i=1; IS_WHITE(str[i]); i=i+1)
		    ;
		if (i > 1)
		    call strcpy (str[i], str, maxchar)
		return
	    }
	}

	call error (0, "Catalog record field not found")
end


# CQ_DGSTR -- Get a string field from the database file.

procedure cq_dgstr (cq, record, field, str, maxchar)

pointer	cq				#I The catalog access descriptor
int	record				#I The catalog record index
char	field[ARB]			#I The field name
char	str[maxchar]			#O The output string value
int	maxchar				#I The maximum characters for string

char	name[SZ_LINE]
int	i, fscan()
bool	streq()

begin
	if ((record < 1) || (record > CQ_NRECS(cq)))
	    call error (0, "Catalog record is out of bounds")

	call seek (CQ_FD(cq), CQ_OFFSET(cq, record))

	while (fscan (CQ_FD(cq)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargstr (str, maxchar)
		for (i=1; IS_WHITE(str[i]); i=i+1)
		    ;
		if (i > 1)
		    call strcpy (str[i], str, maxchar)
		return
	    }
	}

	call error (0, "Catalog record field not found")
end


# CQ_DGAI -- Get an integer array field from a record.

procedure cq_dgai (cq, record, field, array, len_array, npts)

pointer	cq				#I The database catalog record
int	record				#I The database record index
char	field[ARB]			#I The database field
int	array[len_array]		#O The output array values
int	len_array			#I The length of array
int	npts				#O The number of points in the array

char	name[SZ_LINE]
int	i

int	fscan(), nscan()
bool	streq()

begin
	if ((record < 1) || (record > CQ_NRECS(cq)))
	    call error (0, "The catalog record is out of bounds")

	call seek (CQ_FD(cq), CQ_OFFSET(cq, record))

	while (fscan (CQ_FD(cq)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargi (npts)
		if (nscan() != 2)
		    call error (0, "Error reading size of integer array")

		npts = min (npts, len_array)
		for (i = 1; i <= npts; i = i + 1) {
		    if (fscan (CQ_FD(cq)) == EOF)
		        call error (0, "The integer array is truncated")

		    call gargi (array[i])
		    if (nscan() != 1)
		        call error (0, "Error decoding integer array")
		}
		return
	    }
	}

	call error (0, "The catalog record field not found")
end


# CQ_DGAR -- Get a real array field from a record.

procedure cq_dgar (cq, record, field, array, len_array, npts)

pointer	cq				#I The database catalog record
int	record				#I The database record index
char	field[ARB]			#I The database field
real	array[len_array]		#O The output array values
int	len_array			#I The length of array
int	npts				#O The number of points in the array

char	name[SZ_LINE]
int	i

int	fscan(), nscan()
bool	streq()

begin
	if ((record < 1) || (record > CQ_NRECS(cq)))
	    call error (0, "The catalog record is out of bounds")

	call seek (CQ_FD(cq), CQ_OFFSET(cq, record))

	while (fscan (CQ_FD(cq)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargi (npts)
		if (nscan() != 2)
		    call error (0, "Error reading real array size value")

		npts = min (npts, len_array)
		for (i = 1; i <= npts; i = i + 1) {
		    if (fscan (CQ_FD(cq)) == EOF)
		        call error (0, "The real array is truncated")

		    call gargr (array[i])
		    if (nscan() != 1)
		        call error (0, "Error reading real array")
		}
		return
	    }
	}

	call error (0, "The catalog record field not found")
end


# CQ_DGAD -- Get a double array field from a catalog.

procedure cq_dgad (cq, record, field, array, len_array, npts)

pointer	cq				#I The catalog database descriptor
int	record				#I The catalog record index
char	field[ARB]			#I The database field
double	array[len_array]		#O The array values
int	len_array			#I The length of array
int	npts				#O The number of points in the array

char	name[SZ_LINE]
int	i

int	fscan(), nscan()
bool	streq()

begin
	if ((record < 1) || (record > CQ_NRECS(cq)))
	    call error (0, "The catalog record is out of bounds")

	call seek (CQ_FD(cq), CQ_OFFSET(cq, record))

	while (fscan (CQ_FD(cq)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargi (npts)
		if (nscan() != 2)
		    call error (0, "Error the double array size")

		npts = min (npts, len_array)
		for (i = 1; i <= npts; i = i + 1) {
		    if (fscan (CQ_FD(cq)) == EOF)
		        call error (0, "The double array is truncated")

		    call gargd (array[i])
		    if (nscan() != 1)
		        call error (0, "Error reading the double array")
		}
		return
	    }
	}

	call error (0, "Catalog record field not found")
end


# CQ_DGATXT -- Get newline delimited text from a database file.

procedure cq_dgatxt (cq, record, field, str, maxchar, nlines)

pointer	cq				#I The catalog access descriptor
int	record				#I The catalog record index
char	field[ARB]			#I The field name
char	str[maxchar]			#O The output string value
int	maxchar				#I The maximum characters for string
int	nlines				#I the number of text lines

char	name[SZ_LINE]
int	i, op
int	fscan(), nscan(), gstrcpy()
bool	streq()

begin
	if ((record < 1) || (record > CQ_NRECS(cq)))
	    call error (0, "Catalog record is out of bounds")

	call seek (CQ_FD(cq), CQ_OFFSET(cq, record))

	while (fscan (CQ_FD(cq)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargi (nlines)
		if (nscan() != 2)
		    call error (0, "Error text array length")
		op = 1
		do i = 1, nlines {
		    if (fscan (CQ_FD(cq)) == EOF)
		        call error (0, "The text array is truncated")
		    call gargstr (name, SZ_LINE)
		    op = op + gstrcpy (name, str[op], maxchar - op +1)
		    if (op > maxchar)
			break
		    str[op] = '\n'
		    op = op  + 1
		    str[op] = EOS
		}

		return
	    }
	}

	call error (0, "Catalog record field not found")
end


## DTPTIME -- Put a time string with a comment
#
#procedure dtptime (dt)
#
#pointer	dt				# DTTEXT pointer
#
#char	timestr[SZ_TIME]
#long	time, clktime()
#
#begin
#	time = clktime (0)
#	call cnvtime (time, timestr, SZ_TIME)
#	call fprintf (DT(dt), "# %s\n")
#	    call pargstr (timestr)
#end
#
#
## DTPUT -- Print to database.
#
#procedure dtput (dt, format)
#
#pointer	dt				# DTTEXT pointer
#char	format[ARB]			# String format
#
#begin
#	call fprintf (DT(dt), format)
#end

# CQ_DSCAN -- Scan database.

int procedure cq_dscan (cq)

pointer	cq				# The catalog database descriptor.

int	fscan()

begin
	return (fscan (CQ_FD(cq)))
end
