include "cqdef.h"
include "cq.h"


# CQ_HINFO -- Get the header keyword value by keyword name.

int procedure cq_hinfo (res, hkname, hkvalue, sz_hkvalue)

pointer	res			#I the results descriptor
char	hkname[ARB]		#I the header keyword name
char	hkvalue[ARB]		#O the header keyword value
int	sz_hkvalue		#I the maximum size of the keyword value

pointer	sp, kname
int	kwno
int	strdic(), cq_wrdstr()

begin
	# Return if there are no fields. 
	if (CQ_NHEADER(res) <= 0)
	    return (0)

	call smark (sp)
	call salloc (kname, CQ_SZ_FNAME, TY_CHAR)

	# Find the requested field.
	kwno = strdic (hkname, Memc[kname], CQ_SZ_FNAME, Memc[CQ_HKNAMES(res)])
	if (kwno <= 0) {
	    call sfree (sp)
	    return (0)
	}

	# Retrieve the keyword value.
	if (cq_wrdstr (kwno, hkvalue, sz_hkvalue, Memc[CQ_HKVALUES(res)]) <= 0)
	    hkvalue[1] = EOS

	call sfree (sp)

	return (kwno)
end


# CQ_HINFON -- Get the header keyword name and value using the keyword number.

int procedure cq_hinfon (res, kwno, hkname, sz_hkname, hkvalue, sz_hkvalue)

pointer	res			#I the results descriptor
int	kwno			#I the keyword number
char	hkname[ARB]		#O the header keyword name
int	sz_hkname		#I the maximum size of the keyword name
char	hkvalue[ARB]		#O the header keyword value
int	sz_hkvalue		#I the maximum size of the keyword value

int	cq_wrdstr()

begin
	# Return if there are no keywords.
	if (CQ_NHEADER(res) <= 0)
	    return (0)

	# Return if the keyword is out of bounds.
	if (kwno < 1 || kwno > CQ_NHEADER(res))
	    return (0)

	# Retrieve the keyword value.
	if (cq_wrdstr (kwno, hkname, sz_hkname, Memc[CQ_HKNAMES(res)]) <= 0)
	    hkname[1] = EOS

	# Retrieve the keyword value.
	if (cq_wrdstr (kwno, hkvalue, sz_hkvalue, Memc[CQ_HKVALUES(res)]) <= 0)
	    hkvalue[1] = EOS

	return (kwno)
end


# CQ_FINFO -- Get the field description by field name.

int procedure cq_finfo (res, field, foffset, fsize, ftype, units, sz_units,
	fmts, sz_fmts)

pointer	res			#I the results descriptor
char	field[ARB]		#I the field name
int	foffset			#O the output field offset 
int	fsize			#O the output field size
int	ftype			#O the output field datatype 
char	units[ARB]		#O the outpit field units string
int	sz_units		#I the maximum size of the units string
char	fmts[ARB]		#O the outpit field formats string
int	sz_fmts			#I the maximum size of the formats string

pointer	sp, fname
int	fieldno
int	strdic(), cq_wrdstr()

begin
	# Return if there are no fields. 
	if (CQ_NFIELDS(res) <= 0)
	    return (0)

	call smark (sp)
	call salloc (fname, CQ_SZ_FNAME, TY_CHAR)

	# Find the requested field.
	fieldno = strdic (field, Memc[fname], CQ_SZ_FNAME, Memc[CQ_FNAMES(res)])
	if (fieldno <= 0) {
	    call sfree (sp)
	    return (0)
	}

	# Get the field offset, size, and type.
	foffset = Memi[CQ_FOFFSETS(res)+fieldno-1]
	fsize = Memi[CQ_FSIZES(res)+fieldno-1]
	ftype = Memi[CQ_FTYPES(res)+fieldno-1]

	# Get the field units and format.
	if (cq_wrdstr (fieldno, units, sz_units, Memc[CQ_FUNITS(res)]) <= 0)
	    units[1] = EOS
	if (cq_wrdstr (fieldno, fmts, sz_fmts, Memc[CQ_FFMTS(res)]) <= 0)
	    fmts[1] = EOS

	call sfree (sp)

	return (fieldno)
end


# CQ_FNUMBER -- Get the field number given the field name.

int procedure cq_fnumber (res, field)

pointer	res			#I the results descriptor
char	field[ARB]		#I the field name

pointer	sp, fname
int	fieldno
int	strdic()

begin
	# Return if there are no fields. 
	if (CQ_NFIELDS(res) <= 0)
	    return (0)

	call smark (sp)
	call salloc (fname, CQ_SZ_FNAME, TY_CHAR)

	# Find the requested field.
	fieldno = strdic (field, Memc[fname], CQ_SZ_FNAME, Memc[CQ_FNAMES(res)])

	call sfree (sp)
	return (fieldno)
end


# CQ_FOFFSET -- Get the field offset given the field name.

int procedure cq_foffset (res, field)

pointer	res			#I the results descriptor
char	field[ARB]		#I the field name

pointer	sp, fname
int	fieldno
int	strdic()

begin
	# Return if there are no fields. 
	if (CQ_NFIELDS(res) <= 0)
	    return (0)

	call smark (sp)
	call salloc (fname, CQ_SZ_FNAME, TY_CHAR)

	# Find the requested field.
	fieldno = strdic (field, Memc[fname], CQ_SZ_FNAME, Memc[CQ_FNAMES(res)])

	call sfree (sp)

	if (fieldno <= 0)
	    return (0)
	else
	    return (Memi[CQ_FOFFSETS(res)+fieldno-1])
end


# CQ_FSIZE -- Get the field offset given the field name.

int procedure cq_fsize (res, field)

pointer	res			#I the results descriptor
char	field[ARB]		#I the field name

pointer	sp, fname
int	fieldno
int	strdic()

begin
	# Return if there are no fields. 
	if (CQ_NFIELDS(res) <= 0)
	    return (0)

	call smark (sp)
	call salloc (fname, CQ_SZ_FNAME, TY_CHAR)

	# Find the requested field.
	fieldno = strdic (field, Memc[fname], CQ_SZ_FNAME, Memc[CQ_FNAMES(res)])

	call sfree (sp)

	if (fieldno <= 0)
	    return (0)
	else
	    return (Memi[CQ_FSIZES(res)+fieldno-1])
end


# CQ_FTYPE -- Get the field type given the field name.

int procedure cq_ftype (res, field)

pointer	res			#I the results descriptor
char	field[ARB]		#I the field name

pointer	sp, fname
int	fieldno
int	strdic()

begin
	# Return if there are no fields. 
	if (CQ_NFIELDS(res) <= 0)
	    return (0)

	call smark (sp)
	call salloc (fname, CQ_SZ_FNAME, TY_CHAR)

	# Find the requested field.
	fieldno = strdic (field, Memc[fname], CQ_SZ_FNAME, Memc[CQ_FNAMES(res)])

	call sfree (sp)

	if (fieldno <= 0)
	    return (0)
	else
	    return (Memi[CQ_FTYPES(res)+fieldno-1])
end


# CQ_FUNITS -- Get the field units given the field name.

procedure cq_funits (res, field, units, sz_units)

pointer	res			#I the results descriptor
char	field[ARB]		#I the field name
char	units[ARB]		#O the output units string
int	sz_units		#I the maximum size of the units string

pointer	sp, fname
int	fieldno
int	strdic(), cq_wrdstr()

begin
	# Return if there are no fields. 
	if (CQ_NFIELDS(res) <= 0) {
	    units[1] = EOS
	    return
	}

	call smark (sp)
	call salloc (fname, CQ_SZ_FNAME, TY_CHAR)

	# Find the requested field.
	fieldno = strdic (field, Memc[fname], CQ_SZ_FNAME, Memc[CQ_FNAMES(res)])

	# Get the units string.
	if (fieldno > 0) {
	    if (cq_wrdstr (fieldno, units, sz_units, Memc[CQ_FUNITS(res)]) <= 0)
	        units[1] = EOS
	} else
	    units[1] = EOS

	call sfree (sp)
end


# CQ_FFMTS -- Get the field format given the field name.

procedure cq_ffmts (res, field, format, sz_format)

pointer	res			#I the results descriptor
char	field[ARB]		#I the field name
char	format[ARB]		#O the output format string
int	sz_format		#I the maximum size of the format string

pointer	sp, fname
int	fieldno
int	strdic(), cq_wrdstr()

begin
	# Return if there are no fields. 
	if (CQ_NFIELDS(res) <= 0) {
	    format[1] = EOS
	    return
	}

	call smark (sp)
	call salloc (fname, CQ_SZ_FNAME, TY_CHAR)

	# Find the requested field.
	fieldno = strdic (field, Memc[fname], CQ_SZ_FNAME, Memc[CQ_FNAMES(res)])

	# Get the units string.
	if (fieldno > 0) {
	    if (cq_wrdstr (fieldno, format, sz_format,
	        Memc[CQ_FFMTS(res)]) <= 0)
	        format[1] = EOS
	} else
	    format[1] = EOS

	call sfree (sp)
end


# CQ_FINFON -- Get the field description by field number.

int procedure cq_finfon (res, fieldno, fname, sz_fname, foffset, fsize, ftype,
	units, sz_units, fmts, sz_fmts)

pointer	res			#I the results descriptor
int	fieldno			#I the input field number
char	fname[ARB]		#O the field name
int	sz_fname		#I the maximum field name size
int	foffset			#O the output field offset 
int	fsize			#O the output field size
int	ftype			#O the output field datatype 
char	units[ARB]		#O the outpit field units string
int	sz_units		#I the maximum size of the units string
char	fmts[ARB]		#O the outpit field formats string
int	sz_fmts			#I the maximum size of the formats string

int	cq_wrdstr()

begin
	# Return if there are no fields. 
	if (CQ_NFIELDS(res) <= 0)
	    return (0)
	if (fieldno <= 0 || fieldno > CQ_NFIELDS(res))
	    return (0)

	# Get the field name.
	if (cq_wrdstr (fieldno, fname, sz_fname, Memc[CQ_FNAMES(res)]) <= 0)
	    return (0)

	# Set the field offset, size, and type.
	foffset = Memi[CQ_FOFFSETS(res)+fieldno-1]
	fsize = Memi[CQ_FSIZES(res)+fieldno-1]
	ftype = Memi[CQ_FTYPES(res)+fieldno-1]

	if (cq_wrdstr (fieldno, units, sz_units, Memc[CQ_FUNITS(res)]) <= 0)
	    units[1] = EOS
	if (cq_wrdstr (fieldno, fmts, sz_fmts, Memc[CQ_FFMTS(res)]) <= 0)
	    fmts[1] = EOS

	return (fieldno)
end


# CQ_FNAME -- Get the field name given the field number.

int procedure cq_fname (res, fieldno, fname, sz_fname)

pointer	res			#I the results descriptor
int	fieldno			#I the input field number
char	fname[ARB]		#O the field name
int	sz_fname		#I the maximum field name size

int	cq_wrdstr()

begin
	# Return if there are no fields. 
	if (CQ_NFIELDS(res) <= 0)
	    return (0)
	if (fieldno <= 0 || fieldno > CQ_NFIELDS(res))
	    return (0)

	# Get the field name.
	if (cq_wrdstr (fieldno, fname, sz_fname, Memc[CQ_FNAMES(res)]) <= 0)
	    return (0)

	return (fieldno)
end
