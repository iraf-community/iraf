# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<qpioset.h>
include	"qpf.h"

# QPF_WATTR -- Record information about the attributes of the filter
# expression used to generate an image.  Currently the only value which can be
# computed and recorded is total range (integral of the in-range intervals) of
# the range list defining an attribute, for example, the total exposure time
# defined by the time range list used to filter the data.
#
# This routine is driven by a set of optional QPOE header keywords of the
# form
#
#	Keyword		String Value
#
#	defattrN	<param-name> = "integral" <attribute-name>[:type]
# e.g.
#	defattr1	"exptime = integral time:d"
#
# where param-name is the parameter name to be written to the output image
# header, "integral" is the value to be computed, and attribute-name is the
# QPEX attribute (e.g., "time") to be used for the computation.  A finite
# value is returned for the integral if a range list is given for the named
# attribute and the range is closed.  If the range is open on either or both
# ends, or no range expression is defined for the attribute, then INDEF is
# returned for the value of the integral.

procedure qpf_wattr (qpf, im)

pointer	qpf				#I QPF descriptor
pointer	im				#I image descriptor

real	r1, r2, rsum
double	d1, d2, dsum
int	dtype, i, j, xlen, nranges, i1, i2, isum
pointer	sp, io, qp, ex, kwname, kwval, pname, funame, atname, ip, xs, xe

bool	strne()
pointer	qpio_stati()
int	qp_gstr(), ctowrd(), qp_accessf()
int	qpex_attrli(), qpex_attrlr(), qpex_attrld()
errchk	qpex_attrli, qpex_attrlr, qpex_attrld, imaddi, imaddr, imaddd

begin
	io = QPF_IO(qpf)
	if (io == NULL)
	    return

	qp = QPF_QP(qpf)
	ex = qpio_stati (io, QPIO_EX)

	call smark (sp)
	call salloc (kwname, SZ_FNAME, TY_CHAR)
	call salloc (kwval, SZ_LINE, TY_CHAR)
	call salloc (pname, SZ_FNAME, TY_CHAR)
	call salloc (funame, SZ_FNAME, TY_CHAR)
	call salloc (atname, SZ_FNAME, TY_CHAR)

	# Process a sequence of "defattrN" header parameter definitions.
	# Each defines a parameter to be computed and added to the output
	# image header.

	do i = 1, ARB {
	    # Check for a parameter named "defattrN", get string value.
	    call sprintf (Memc[kwname], SZ_FNAME, "defattr%d")
		call pargi (i)

	    if (qp_accessf (qp, Memc[kwname]) == NO)
		break
	    if (qp_gstr (qp, Memc[kwname], Memc[kwval], SZ_LINE) <= 0)
		break

	    # Parse string value into parameter name, function name,
	    # expression attribute name, and datatype.

	    ip = kwval
	    if (ctowrd (Memc, ip, Memc[pname], SZ_FNAME) <= 0)
		break
	    while (IS_WHITE(Memc[ip]) || Memc[ip] == '=')
		ip = ip + 1
	    if (ctowrd (Memc, ip, Memc[funame], SZ_FNAME) <= 0)
		break
	    if (ctowrd (Memc, ip, Memc[atname], SZ_FNAME) <= 0)
		break

	    dtype = TY_INT
	    for (ip=atname;  Memc[ip] != EOS;  ip=ip+1)
		if (Memc[ip] == ':') {
		    Memc[ip] = EOS
		    if (Memc[ip+1] == 'd')
			dtype = TY_DOUBLE
		    else if (Memc[ip+1] == 'r')
			dtype = TY_REAL
		    else
			call eprintf ("QPF.defattr: datatype not recognized\n")
		}

	    # Verify known function type.
	    if (strne (Memc[funame], "integral")) {
		call eprintf ("QPF.defattr: function `%s' not recognized\n")
		    call pargstr (Memc[funame])
		break
	    }

	    # Compute the integral of the range list for the named attribute.
	    xlen = 0
	    xs = NULL  
	    xe = NULL

	    switch (dtype) {
	    case TY_REAL:
		if (ex == NULL)
		    nranges = 0
		else
		    nranges = qpex_attrlr (ex, Memc[atname], xs, xe, xlen)

		if (nranges <= 0)
		    rsum = INDEFR
		else {
		    rsum = 0
		    do j = 1, nranges {
			r1 = Memr[xs+j-1]
			r2 = Memr[xe+j-1]
			if (IS_INDEFR(r1) || IS_INDEFR(r2)) {
			    rsum = INDEFR
			    break
			} else
			    rsum = rsum + (r2 - r1)
		    }
		}

		call mfree (xs, TY_REAL)
		call mfree (xe, TY_REAL)
		call imaddr (im, Memc[pname], rsum)

	    case TY_DOUBLE:
		if (ex == NULL)
		    nranges = 0
		else
		    nranges = qpex_attrld (ex, Memc[atname], xs, xe, xlen)

		if (nranges <= 0)
		    dsum = INDEFD
		else {
		    dsum = 0
		    do j = 1, nranges {
			d1 = Memd[xs+j-1]
			d2 = Memd[xe+j-1]
			if (IS_INDEFD(d1) || IS_INDEFD(d2)) {
			    dsum = INDEFD
			    break
			} else
			    dsum = dsum + (d2 - d1)
		    }
		}

		call mfree (xs, TY_DOUBLE)
		call mfree (xe, TY_DOUBLE)
		call imaddd (im, Memc[pname], dsum)

	    default:
		if (ex == NULL)
		    nranges = 0
		else
		    nranges = qpex_attrli (ex, Memc[atname], xs, xe, xlen)

		if (nranges <= 0)
		    isum = INDEFI
		else {
		    isum = 0
		    do j = 1, nranges {
			i1 = Memi[xs+j-1]
			i2 = Memi[xe+j-1]
			if (IS_INDEFI(i1) || IS_INDEFI(i2)) {
			    isum = INDEFI
			    break
			} else
			    isum = isum + (i2 - i1)
		    }
		}

		call mfree (xs, TY_INT)
		call mfree (xe, TY_INT)
		call imaddi (im, Memc[pname], isum)
	    }
	}

	call sfree (sp)
end
