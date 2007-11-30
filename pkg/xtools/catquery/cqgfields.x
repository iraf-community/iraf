include <ctype.h>
include "cqdef.h"
include "cq.h"

# CQ_SETRECORD -- Set the the current record. What action this procedure takes
# depends on the input data type. In the case of text files this task
# sets the current record pointer and figures where in the record each
# column begins. For blocked text files the foffsets determine where each
# record begins.

int procedure cq_setrecord (res, recptr)

pointer	res			#I the results descriptor
int	recptr			#U the current record pointer

pointer	buf

begin
	# The record is outside the record data range.
	if (recptr <= 0) {
	    CQ_RECPTR(res) = 0
	    CQ_FNFIELDS(res) = 0
	    call aclri (Memi[CQ_FINDICES(res)], CQ_MAX_NFIELDS + 1)
	    return (BOF)
	}
	if (recptr > CQ_RNRECS(res))
	    return (EOF)

	CQ_RECPTR(res) = recptr
	switch (CQ_RTYPE(res)) {
	case CQ_STEXT:
	    buf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    call cq_find_fields (Memc[buf], Memi[CQ_FINDICES(res)],
	        CQ_MAX_NFIELDS, CQ_FNFIELDS(res))
	case CQ_BTEXT:
	    ;
	default:
	}

	return (recptr)
end


# CQ_GVALC -- Fetch a record field as a string value.

int procedure cq_gvalc (res, recptr, field, str, maxch)

pointer	res			#I the results descriptor
int	recptr			#I the current record pointer
char	field[ARB]		#I the record field name.
char	str[ARB]		#O the output string parameter
int	maxch			#I the maximum number of characters

pointer	fbuf
int	fnum, fip, fsize
int	cq_fnumber(), cq_setrecord()

begin
	# The record is outside the record data range.
	str[1] = EOS
	if (recptr <= 0 || recptr > CQ_RNRECS(res))
	    return (0)

	# Find the field number.
	fnum = cq_fnumber (res, field)
	if (fnum <= 0)
	    return (0)

	# Set the current record if necessary.
	if (recptr != CQ_RECPTR(res)) {
	    if (cq_setrecord (res, recptr) != recptr)
		return (0)
	}

	# Extract the requested field as a string. If the data is in binary
	# internally this will require formatting a string. If the data is
	# text this requires extracting the appropriate piece of text.

	switch (CQ_RTYPE(res)) {

	case CQ_STEXT:
	    fbuf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    fnum = Memi[CQ_FOFFSETS(res)+fnum-1]
	    fip = Memi[CQ_FINDICES(res)+fnum-1]
	    fsize = min (maxch, Memi[CQ_FINDICES(res)+fnum] -
	        Memi[CQ_FINDICES(res)+fnum-1])
	    call strcpy (Memc[fbuf+fip-1], str, fsize)

	case CQ_BTEXT:
	    fbuf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    fip = Memi[CQ_FOFFSETS(res)+fnum-1]
	    fsize = min (maxch, Memi[CQ_FSIZES(res)+fnum-1])
	    call strcpy (Memc[fbuf+fip-1], str, fsize)

	default:
	    fsize = 0

	}

	return (fsize)
end


# CQ_GVALD -- Return a double precision field value 

int procedure cq_gvald (res, recptr, field, dval)

pointer	res			#I the results descriptor
int	recptr			#I the current record pointer
char	field[ARB]		#I the record field name.
double	dval			#O the output double value

pointer	fbuf, sp, line
int	fnum, fip, fsize, nchars
int	cq_fnumber(), ctod(), cq_setrecord()

begin
	dval = INDEFD

	# The record is outside the record data range.
	if (recptr <= 0 || recptr > CQ_RNRECS(res))
	    return (0)

	# Find the field number.
	fnum = cq_fnumber (res, field)
	if (fnum <= 0)
	    return (0)

	# Set the current record if necessary.
	if (recptr != CQ_RECPTR(res)) {
	    if (cq_setrecord (res, recptr) != recptr)
		return (0)
	}

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Extract the requested field as a double precision value. If the data
	# is in binary internally this may imply a type conversion. If the data
	# is text this requires decoding the string value.

	switch (CQ_RTYPE(res)) {

	case CQ_STEXT:
	    fbuf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    fnum = Memi[CQ_FOFFSETS(res)+fnum-1]
	    fip = Memi[CQ_FINDICES(res)+fnum-1]
	    fsize = min (SZ_LINE, Memi[CQ_FINDICES(res)+fnum] -
	        Memi[CQ_FINDICES(res)+fnum-1])
	    call strcpy (Memc[fbuf+fip-1], Memc[line], fsize)
	    fip = 1
	    nchars = ctod (Memc[line], fip, dval) 

	case CQ_BTEXT:
	    fbuf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    fip = Memi[CQ_FOFFSETS(res)+fnum-1]
	    fsize = min (SZ_LINE, Memi[CQ_FSIZES(res)+fnum-1])
	    call strcpy (Memc[fbuf+fip-1], Memc[line], fsize)
	    fip = 1
	    nchars = ctod (Memc[line], fip, dval)

	default:
	    nchars = 0

	}

	call sfree (sp)

	return (nchars)
end


# CQ_GVALR -- Return a real precision field value.

int procedure cq_gvalr (res, recptr, field, rval)

pointer	res			#I the results descriptor
int	recptr			#I the current record pointer
char	field[ARB]		#I the record field name.
real	rval			#O the output real value

pointer	fbuf, sp, line
int	fnum, fip, fsize, nchars
int	cq_fnumber(), ctor(), cq_setrecord()

begin
	rval = INDEFR

	# The record is outside the record data range.
	if (recptr <= 0 || recptr > CQ_RNRECS(res))
	    return (0)

	# Find the field number.
	fnum = cq_fnumber (res, field)
	if (fnum <= 0)
	    return (0)

	# Set the current record if necessary.
	if (recptr != CQ_RECPTR(res)) {
	    if (cq_setrecord (res, recptr) != recptr)
		return (0)
	}

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Extract the requested field as a double precision value. If the data
	# is in binary internally this may imply a type conversion. If the data
	# is text this requires decoding the string value.

	switch (CQ_RTYPE(res)) {

	case CQ_STEXT:
	    fbuf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    fnum = Memi[CQ_FOFFSETS(res)+fnum-1]
	    fip = Memi[CQ_FINDICES(res)+fnum-1]
	    fsize = min (SZ_LINE, Memi[CQ_FINDICES(res)+fnum] -
	        Memi[CQ_FINDICES(res)+fnum-1])
	    call strcpy (Memc[fbuf+fip-1], Memc[line], fsize)
	    fip = 1
	    nchars = ctor (Memc[line], fip, rval)

	case CQ_BTEXT:
	    fbuf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    fip = Memi[CQ_FOFFSETS(res)+fnum-1]
	    fsize = min (SZ_LINE, Memi[CQ_FSIZES(res)+fnum-1])
	    call strcpy (Memc[fbuf+fip-1], Memc[line], fsize)
	    fip = 1
	    nchars = ctor (Memc[line], fip, rval)

	default:
	    nchars = 0

	}

	call sfree (sp)

	return (nchars)
end


# CQ_GVALL -- Return a long integer field value.

int procedure cq_gvall (res, recptr, field, lval)

pointer	res			#I the results descriptor
int	recptr			#I the current record pointer
char	field[ARB]		#I the record field name.
long	lval			#I the output long value

pointer	fbuf, sp, line
int	fnum, fip, fsize, nchars
int	cq_fnumber(), ctol(), cq_setrecord()

begin
	lval = INDEFL

	# The record is outside the record data range.
	if (recptr <= 0 || recptr > CQ_RNRECS(res))
	    return (0)

	# Find the field number.
	fnum = cq_fnumber (res, field)
	if (fnum <= 0)
	    return (0)

	# Set the current record if necessary.
	if (recptr != CQ_RECPTR(res)) {
	    if (cq_setrecord (res, recptr) != recptr)
		return(0)
	}

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Extract the requested field as a double precision value. If the data
	# is in binary internally this may imply a type conversion. If the data
	# is text this requires decoding the string value.

	switch (CQ_RTYPE(res)) {

	case CQ_STEXT:
	    fbuf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    fnum = Memi[CQ_FOFFSETS(res)+fnum-1]
	    fip = Memi[CQ_FINDICES(res)+fnum-1]
	    fsize = min (SZ_LINE, Memi[CQ_FINDICES(res)+fnum] -
	        Memi[CQ_FINDICES(res)+fnum-1])
	    call strcpy (Memc[fbuf+fip-1], Memc[line], fsize)
	    fip = 1
	    nchars = ctol (Memc[line], fip, lval)

	case CQ_BTEXT:
	    fbuf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    fip = Memi[CQ_FOFFSETS(res)+fnum-1]
	    fsize = min (SZ_LINE, Memi[CQ_FSIZES(res)+fnum-1])
	    call strcpy (Memc[fbuf+fip-1], Memc[line], fsize)
	    fip = 1
	    nchars = ctol (Memc[line], fip, lval) 

	default:
	    nchars = 0

	}

	call sfree (sp)

	return (nchars)
end


# CQ_GVALI -- Return an integer field value 

int procedure cq_gvali (res, recptr, field, ival)

pointer	res			#I the results descriptor
int	recptr			#I the current record pointer
char	field[ARB]		#I the record field name.
int	ival			#I the output int value

pointer	fbuf, sp, line
int	fnum, fip, fsize, nchars
int	cq_fnumber(), ctoi(), cq_setrecord()

begin
	ival = INDEFI

	# The record is outside the record data range.
	if (recptr <= 0 || recptr > CQ_RNRECS(res))
	    return (0)

	# Find the field number.
	fnum = cq_fnumber (res, field)
	if (fnum <= 0)
	    return (0)

	# Set the current record if necessary.
	if (recptr != CQ_RECPTR(res)) {
	    if (cq_setrecord (res, recptr) != recptr)
		return (0)
	}

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Extract the requested field as a double precision value. If the data
	# is in binary internally this may imply a type conversion. If the data
	# is text this requires decoding the string value.

	switch (CQ_RTYPE(res)) {

	case CQ_STEXT:
	    fbuf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    fnum = Memi[CQ_FOFFSETS(res)+fnum-1]
	    fip = Memi[CQ_FINDICES(res)+fnum-1]
	    fsize = min (SZ_LINE, Memi[CQ_FINDICES(res)+fnum] -
	        Memi[CQ_FINDICES(res)+fnum-1])
	    call strcpy (Memc[fbuf+fip-1], Memc[line], fsize)
	    fip = 1
	    nchars = ctoi (Memc[line], fip, ival)

	case CQ_BTEXT:
	    fbuf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    fip = Memi[CQ_FOFFSETS(res)+fnum-1]
	    fsize = min (SZ_LINE, Memi[CQ_FSIZES(res)+fnum-1])
	    call strcpy (Memc[fbuf+fip-1], Memc[line], fsize)
	    fip = 1
	    nchars = ctoi (Memc[line], fip, ival) 

	default:
	    nchars = 0
	}

	call sfree (sp)

	return (nchars)
end


# CQ_GVALS -- Return a short integer field value 

int procedure cq_gvals (res, recptr, field, sval)

pointer	res			#I the results descriptor
int	recptr			#I the current record pointer
char	field[ARB]		#I the record field name.
short	sval			#O the output short value

pointer	fbuf, sp, line
int	fnum, fip, fsize, nchars, ival
int	cq_fnumber(), ctoi(), cq_setrecord()

begin
	sval = INDEFS

	# The record is outside the record data range.
	if (recptr <= 0 || recptr > CQ_RNRECS(res))
	    return (0)

	# Find the field number.
	fnum = cq_fnumber (res, field)
	if (fnum <= 0)
	    return (0)

	# Set the current record if necessary.
	if (recptr != CQ_RECPTR(res)) {
	    if (cq_setrecord (res, recptr) != recptr)
		return (0)
	}

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Extract the requested field as a double precision value. If the data
	# is in binary internally this may imply a type conversion. If the data
	# is text this requires decoding the string value.

	switch (CQ_RTYPE(res)) {

	case CQ_STEXT:
	    fbuf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    fnum = Memi[CQ_FOFFSETS(res)+fnum-1]
	    fip = Memi[CQ_FINDICES(res)+fnum-1]
	    fsize = min (SZ_LINE, Memi[CQ_FINDICES(res)+fnum] -
	        Memi[CQ_FINDICES(res)+fnum-1])
	    call strcpy (Memc[fbuf+fip-1], Memc[line], fsize)
	    fip = 1
	    nchars = ctoi (Memc[line], fip, ival)
	    if (nchars > 0)
	        sval = ival

	case CQ_BTEXT:
	    fbuf = CQ_RBUF(res) + Memi[CQ_RINDEX(res)+recptr-1] - 1
	    fip = Memi[CQ_FOFFSETS(res)+fnum-1]
	    fsize = min (SZ_LINE, Memi[CQ_FSIZES(res)+fnum-1])
	    call strcpy (Memc[fbuf+fip-1], Memc[line], fsize)
	    fip = 1
	    nchars = ctoi (Memc[line], fip, ival)
	    if (nchars > 0)
		sval = ival

	default:
	    nchars = 0

	}

	call sfree (sp)

	return (nchars)
end


# CQ_FIND_FIELDS -- This procedure finds the starting column for each field
# in the input line.  These column numbers are returned in the array
# field_pos; the number of fields is also returned.

procedure cq_find_fields (linebuf, field_pos, max_fields, nfields)

char    linebuf[ARB]                    #I the input buffer
int     field_pos[max_fields]           #O the output field positions
int     max_fields                      #I the maximum number of fields
int     nfields                         #O the computed number of fields

bool    in_field
int     ip, field_num

begin
        field_num = 1
        field_pos[1] = 1
        in_field = false

        for (ip=1; linebuf[ip] != '\n' && linebuf[ip] != EOS; ip=ip+1) {
            if (! IS_WHITE(linebuf[ip]))
                in_field = true
            else if (in_field) {
                in_field = false
                field_num = field_num + 1
                field_pos[field_num] = ip
            }
        }

        field_pos[field_num+1] = ip
        nfields = field_num
end
