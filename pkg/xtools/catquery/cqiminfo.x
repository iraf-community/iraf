include "cqdef.h"
include "cq.h"


# CQ_WINFO -- Get the WCS field description by field name.

int procedure cq_winfo (res, wfield, wkname, sz_wkname, wkvalue, sz_wkvalue,
	wktype, wkunits, sz_wkunits)

pointer	res			#I the results descriptor
char	wfield[ARB]		#I the field name
char	wkname[ARB]		#O the output keyword name
int	sz_wkname		#I the maximum size of the keyword name string
char	wkvalue[ARB]		#O the current value string
int	sz_wkvalue		#I the maximum size of the current value string
int	wktype			#O the output field datatype 
char	wkunits[ARB]		#O the outpit field units string
int	sz_wkunits		#I the maximum size of the units string

pointer	sp, fname
int	fieldno
int	strdic(), cq_wrdstr()

begin
	# Return if there are no fields. 
	if (CQ_NWCS(res) <= 0)
	    return (0)

	call smark (sp)
	call salloc (fname, CQ_SZ_FNAME, TY_CHAR)

	# Find the requested field.
	fieldno = strdic (wfield, Memc[fname], CQ_SZ_FNAME,
	    Memc[CQ_WPNAMES(res)])
	if (fieldno <= 0) {
	    call sfree (sp)
	    return (0)
	}

	# Get the field keyword name.
	if (cq_wrdstr (fieldno, wkname, sz_wkname, Memc[CQ_WKNAMES(res)]) <= 0)
	    wkname[1] = EOS

	# Get the field keyword value.
	if (cq_wrdstr (fieldno, wkvalue, sz_wkvalue,
	    Memc[CQ_WKVALUES(res)]) <= 0)
	    wkvalue[1] = EOS

	# Get the field type.
	wktype = Memi[CQ_WKTYPES(res)+fieldno-1]

	# Get the field units.
	if (cq_wrdstr (fieldno, wkunits, sz_wkunits,
	    Memc[CQ_WKUNITS(res)]) <= 0)
	    wkunits[1] = EOS

	call sfree (sp)

	return (fieldno)
end


# CQ_WINFON -- Get the WCS field description by field number.

int procedure cq_winfon (res, fieldno, wfield, sz_wfield, wkname, sz_wkname,
	wkvalue, sz_wkvalue, wktype, wkunits, sz_wkunits)

pointer	res			#I the results descriptor
int	fieldno			#I the input field number
char	wfield[ARB]		#O the field name
int	sz_wfield		#I the maximum size of the field string
char	wkname[ARB]		#O the output keyword name
int	sz_wkname		#I the maximum size of the keyword name string
char	wkvalue[ARB]		#O the current value string
int	sz_wkvalue		#I the maximum size of the current value string
int	wktype			#O the output field datatype 
char	wkunits[ARB]		#O the outpit field units string
int	sz_wkunits		#I the maximum size of the units string

int	cq_wrdstr()

begin
	# Return if there are no fields. 
	if (CQ_NWCS(res) <= 0)
	    return (0)
	if (fieldno <= 0 || fieldno > CQ_NWCS(res))
	    return (0)

	# Get the field name.
	if (cq_wrdstr (fieldno, wfield, sz_wfield, Memc[CQ_WPNAMES(res)]) <= 0)
	    return (0)

	# Get the field keyword name.
	if (cq_wrdstr (fieldno, wkname, sz_wkname, Memc[CQ_WKNAMES(res)]) <= 0)
	    wkname[1] = EOS

	# Get the field keyword value.
	if (cq_wrdstr (fieldno, wkvalue, sz_wkvalue,
	    Memc[CQ_WKVALUES(res)]) <= 0)
	    wkvalue[1] = EOS

	# Get the field type.
	wktype = Memi[CQ_WKTYPES(res)+fieldno-1]

	# Get the field units.
	if (cq_wrdstr (fieldno, wkunits, sz_wkunits,
	    Memc[CQ_WKUNITS(res)]) <= 0)
	    wkunits[1] = EOS

	return (fieldno)
end


# CQ_KINFO -- Get the keyword field description by field name.

int procedure cq_kinfo (res, kfield, ikname, sz_ikname, ikvalue, sz_ikvalue,
	iktype, ikunits, sz_ikunits)

pointer	res			#I the results descriptor
char	kfield[ARB]		#I the field name
char	ikname[ARB]		#O the output keyword name
int	sz_ikname		#I the maximum size of the keyword name string
char	ikvalue[ARB]		#O the current value string
int	sz_ikvalue		#I the maximum size of the current value string
int	iktype			#O the output field datatype 
char	ikunits[ARB]		#O the outpit field units string
int	sz_ikunits		#I the maximum size of the units string

pointer	sp, fname
int	fieldno
int	strdic(), cq_wrdstr()

begin
	# Return if there are no fields. 
	if (CQ_NIMPARS(res) <= 0)
	    return (0)

	call smark (sp)
	call salloc (fname, CQ_SZ_FNAME, TY_CHAR)

	# Find the requested field.
	fieldno = strdic (kfield, Memc[fname], CQ_SZ_FNAME,
	    Memc[CQ_IPNAMES(res)])
	if (fieldno <= 0) {
	    call sfree (sp)
	    return (0)
	}

	# Get the field keyword name.
	if (cq_wrdstr (fieldno, ikname, sz_ikname, Memc[CQ_IKNAMES(res)]) <= 0)
	    ikname[1] = EOS

	# Get the field keyword value.
	if (cq_wrdstr (fieldno, ikvalue, sz_ikvalue,
	    Memc[CQ_IKVALUES(res)]) <= 0)
	    ikvalue[1] = EOS

	# Get the field type.
	iktype = Memi[CQ_IKTYPES(res)+fieldno-1]

	# Get the field units.
	if (cq_wrdstr (fieldno, ikunits, sz_ikunits,
	    Memc[CQ_IKUNITS(res)]) <= 0)
	    ikunits[1] = EOS

	call sfree (sp)

	return (fieldno)
end


# CQ_KINFON -- Get the image keyword field description by field number.

int procedure cq_kinfon (res, fieldno, kfield, sz_kfield, ikname, sz_ikname,
	ikvalue, sz_ikvalue, iktype, ikunits, sz_ikunits)

pointer	res			#I the results descriptor
int	fieldno			#I the input field number
char	kfield[ARB]		#O the field name
int	sz_kfield		#I the maximum size of the field string
char	ikname[ARB]		#O the output keyword name
int	sz_ikname		#I the maximum size of the keyword name string
char	ikvalue[ARB]		#O the current value string
int	sz_ikvalue		#I the maximum size of the current value string
int	iktype			#O the output field datatype 
char	ikunits[ARB]		#O the outpit field units string
int	sz_ikunits		#I the maximum size of the units string

int	cq_wrdstr()

begin
	# Return if there are no fields. 
	if (CQ_NIMPARS(res) <= 0)
	    return (0)
	if (fieldno <= 0 || fieldno > CQ_NIMPARS(res))
	    return (0)

	# Get the field name.
	if (cq_wrdstr (fieldno, kfield, sz_kfield, Memc[CQ_IPNAMES(res)]) <= 0)
	    return (0)

	# Get the field keyword name.
	if (cq_wrdstr (fieldno, ikname, sz_ikname, Memc[CQ_IKNAMES(res)]) <= 0)
	    ikname[1] = EOS

	# Get the field keyword value.
	if (cq_wrdstr (fieldno, ikvalue, sz_ikvalue, Memc[CQ_IKVALUES(res)]) <=
	    0)
	    ikvalue[1] = EOS

	# Get the field type.
	iktype = Memi[CQ_IKTYPES(res)+fieldno-1]

	# Get the field units.
	if (cq_wrdstr (fieldno, ikunits, sz_ikunits, Memc[CQ_IKUNITS(res)]) <=
	    0)
	    ikunits[1] = EOS

	return (fieldno)
end
