include	"autoid.h"

define	AIDSET "|reflist|refspec|crval|cdelt|crpix|crsearch|cdsearch|cddir\
		|ntarget|nreference|aidord|nbins|nneighbors|npattern|sigma\
		|nfound|rms|fmatch|ftmatch|minratio|ndmax|debug|nbest|wrms\
		|wfmatch|wftmatch|"


# AID_SETS -- Set AID parameters by name.
# If the first word of the value field is "CL" or "ENV" then the second
# word is the CL parameter name or environment variable name to use
# for the value.

procedure aid_sets (aid, param, value)

pointer	aid		#I AID object
char	param[ARB]	#I Parameter name
char	value[ARB]	#I Value

int	i, j, strdic(), strncmp(), envfind(), nowhite(), ctoi(), ctor()
pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	i = strdic (param, Memc[str], SZ_LINE, AIDSET)

	if (strncmp ("CL ", value, 3) == 0)
	    call clgstr (value[4], Memc[str], SZ_LINE)
	else if (strncmp ("ENV ", value, 4) == 0) {
	    if (envfind (value[5], Memc[str], SZ_LINE) <= 0)
		Memc[str] = EOS
	} else
	    call strcpy (value, Memc[str], SZ_LINE)
	j = nowhite (Memc[str], Memc[str], SZ_LINE)

	j = 1
	switch (i) {
	case 1:
	    call strcpy (Memc[str], AID_REFLIST(aid), AID_SZLINE)
	case 2:
	    call strcpy (Memc[str], AID_REFSPEC(aid), AID_SZLINE)
	case 3:
	    call strcpy (Memc[str], AID_CR(aid), AID_SZLINE)
	case 4:
	    call strcpy (Memc[str], AID_CD(aid), AID_SZLINE)
	case 5:
	    call strcpy (Memc[str], AID_CP(aid), AID_SZLINE)
	case 6:
	    call strcpy (Memc[str], AID_CRS(aid), AID_SZLINE)
	case 7:
	    call strcpy (Memc[str], AID_CDS(aid), AID_SZLINE)
	case 8:
	    AID_CDDIR(aid) = strdic (Memc[str], Memc[str], SZ_LINE, CDDIR)
	    if (AID_CDDIR(aid) == 0)
		AID_CDDIR(aid) = CDUNKNOWN
	case 9:
	    i = ctoi (Memc[str], j, AID_NTMAX(aid))
	case 10:
	    i = ctoi (Memc[str], j, AID_NRMAX(aid))
	case 11:
	    i = ctoi (Memc[str], j, AID_ORD(aid))
	    call ic_puti (AID_IC1(aid), "order", AID_ORD(aid))
	case 12:
	    i = ctoi (Memc[str], j, AID_NB(aid))
	case 13:
	    i = ctoi (Memc[str], j, AID_NN(aid))
	case 14:
	    i = ctoi (Memc[str], j, AID_NP(aid))
	case 15:
	    i = ctor (Memc[str], j, AID_SIG(aid))
	case 16:
	    i = ctoi (Memc[str], j, AID_NFOUND(aid))
	case 17:
	    i = ctor (Memc[str], j, AID_RMSG(aid))
	case 18:
	    i = ctor (Memc[str], j, AID_FMATCHG(aid))
	case 19:
	    i = ctor (Memc[str], j, AID_FTMATCHG(aid))
	case 20:
	    i = ctor (Memc[str], j, AID_MINRATIO(aid))
	case 21:
	    i = ctoi (Memc[str], j, AID_NDMAX(aid))
	case 22:
	    call strcpy (Memc[str], AID_DEBUG(aid,1), AID_SZLINE)
	case 23:
	    i = ctoi (Memc[str], j, AID_NBEST(aid))
	case 24:
	    i = ctor (Memc[str], j, AID_WRMS(aid))
	case 25:
	    i = ctor (Memc[str], j, AID_WFMATCH(aid))
	case 26:
	    i = ctor (Memc[str], j, AID_WFTMATCH(aid))
	}

	call sfree (sp)
end


# AID_SETI -- Set integer AID parameters.

procedure aid_seti (aid, param, ival)

pointer	aid		#I AID object
char	param[ARB]	#I Parameter name
int	ival		#I Value

pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call sprintf (Memc[str], SZ_FNAME, "%d")
	    call pargi (ival)
	call aid_sets (aid, param, Memc[str])
	call sfree (sp)
end


# AID_SETR -- Set real AID parameters.

procedure aid_setr (aid, param, rval)

pointer	aid		#I AID object
char	param[ARB]	#I Parameter name
real	rval		#I Value

pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call sprintf (Memc[str], SZ_FNAME, "%g")
	    call pargr (rval)
	call aid_sets (aid, param, Memc[str])
	call sfree (sp)
end


# AID_SETD -- Set double AID parameters.

procedure aid_setd (aid, param, dval)

pointer	aid		#I AID object
char	param[ARB]	#I Parameter name
double	dval		#I Value

pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call sprintf (Memc[str], SZ_FNAME, "%g")
	    call pargd (dval)
	call aid_sets (aid, param, Memc[str])
	call sfree (sp)
end
