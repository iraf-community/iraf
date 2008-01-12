#* HISTORY *
#* B.Simon	30-Sep-98	Rewriten to support all image types

# GF_ADDPAR -- Add a parameter to the group parameter block

procedure gf_addpar (im, pname, dtype, plen, pval, pcomm)

pointer	im			# i: image descriptor
char	pname[ARB]		# i: parameter name
int	dtype			# i: SPP datatype of parameter
int	plen			# i: length (> 1 if array)
char	pval[ARB]		# i: string encoded initial parameter value
char	pcomm[ARB]		# i: string comment to the new parameter
#--
bool	bval
real	rval
double	dval
short	sval
long	lval
int	ival, nc, ic

string	wronglen  "gf_addpar: cannot add array parameter to header"

bool	gf_geis()
int	ctoi(), ctol(), ctor(), ctod()

errchk	imadds, imaddl, imaddr, imaddd, imastr

begin
	# Call geis procedure to update stf kernel if this is a geis file

	if (gf_geis (im)) {
	    call gi_addpar (im, pname, dtype, plen, pval, pcomm)
	    return
	}

	# Otherwise, treat it as an ordinary header parameter

	if (plen != 1 && dtype != TY_CHAR)
	    call error (1, wronglen)

	ic = 1
	switch (dtype) {
	case TY_BOOL:
	    bval = (pval[1] == 'T')
	    call gf_iaddb (im, pname, bval)
	case TY_SHORT:
	    nc = ctoi (pval, ic, ival)
	    sval = ival
	    call gf_iadds (im, pname, sval)
	case TY_LONG, TY_INT:
	    nc = ctol (pval, ic, lval)
	    call gf_iaddl (im, pname, lval)
	case TY_REAL:
	    nc = ctor (pval, ic, rval)
	    call gf_iaddr (im, pname, rval)
	case TY_DOUBLE:
	    nc = ctod (pval, ic, dval)
	    call gf_iaddd (im, pname, dval)
	default:
	    call gf_iastr (im, pname, pval)
	}
	
end

