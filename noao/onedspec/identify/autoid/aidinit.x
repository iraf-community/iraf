include	<smw.h>
include	"../identify.h"
include	"autoid.h"


# AID_INIT -- Create AID object and initialize algorithm parameters.

procedure aid_init (aid, pset)

pointer	aid		#O AID object
char	pset[ARB]	#I Pset for parameters

pointer	pp, clopset()
int	clgpseti(), strdic()
double	clgpsetd()

begin
	call calloc (aid, AID_LEN, TY_STRUCT)

	# Set default parameters.  This can be overridden later by
	# the application.

	pp = clopset (pset)

	#call clgpseta (pp, "crval", AID_CR(aid), AID_SZLINE) 
	#call clgpseta (pp, "cdelt", AID_CD(aid), AID_SZLINE) 
	call strcpy ("INDEF", AID_CR(aid), AID_SZLINE)
	call strcpy ("INDEF", AID_CD(aid), AID_SZLINE)

	call clgpseta (pp, "reflist", AID_REFLIST(aid), AID_SZLINE) 
	call clgpseta (pp, "refspec", AID_REFSPEC(aid), AID_SZLINE) 
	call clgpseta (pp, "crpix", AID_CP(aid), AID_SZLINE) 
	call clgpseta (pp, "crquad", AID_CQ(aid), AID_SZLINE) 
	call clgpseta (pp, "cddir", AID_DEBUG(aid,1), AID_SZLINE) 
	AID_CDDIR(aid) = strdic (AID_DEBUG(aid,1), AID_DEBUG(aid,1),
	    AID_SZLINE, CDDIR)
	call clgpseta (pp, "crsearch", AID_CRS(aid), AID_SZLINE) 
	call clgpseta (pp, "cdsearch", AID_CDS(aid), AID_SZLINE) 
	AID_NTMAX(aid) = clgpseti (pp, "ntarget")
	#AID_NRMAX(aid) = clgpseti (pp, "nreference")
	AID_NRMAX(aid) = 2 * AID_NTMAX(aid)
	AID_ORD(aid) = clgpseti (pp, "aidord")
	AID_MAXNL(aid) = clgpsetd (pp, "maxnl")
	AID_NB(aid) = clgpseti (pp, "nbins")
	AID_NN(aid) = clgpseti (pp, "nneighbors")
	AID_NP(aid) = clgpseti (pp, "npattern")
	AID_SIG(aid) = clgpsetd (pp, "sigma")
	AID_NFOUND(aid) = clgpseti (pp, "nfound")
	AID_RMSG(aid) = clgpsetd (pp, "rms")
	AID_FMATCHG(aid) = clgpsetd (pp, "fmatch")
	AID_FTMATCHG(aid) = clgpsetd (pp, "fmatch")
	AID_MINRATIO(aid) = clgpsetd (pp, "minratio")
	AID_NDMAX(aid) = clgpseti (pp, "ndmax")
	call clgpseta (pp, "debug", AID_DEBUG(aid,1), AID_SZLINE) 
	AID_NBEST(aid) = 3
	AID_WRMS(aid) = 0.34
	AID_WFMATCH(aid) = 0.33
	AID_WFTMATCH(aid) = 0.33
	call clcpset (pp)

	call ic_open (AID_IC1(aid))
	call ic_pstr (AID_IC1(aid), "function", "chebyshev")
	call ic_puti (AID_IC1(aid), "order", AID_ORD(aid))
	call ic_puti (AID_IC1(aid), "niterate", 5)
	call ic_putr (AID_IC1(aid), "low", 2.)
	call ic_putr (AID_IC1(aid), "high", 2.)
end


# AID_FREE -- Free memory associated with the AID algorithms.

procedure aid_free (aid)

pointer	aid		#U AID object

begin
	if (AID_IDR(aid) != NULL) {
	    if (ID_SH(AID_IDR(aid)) != NULL) {
		call smw_close (MW(ID_SH(AID_IDR(aid))))
		call imunmap (IM(ID_SH(AID_IDR(aid))))
		call shdr_close (ID_SH(AID_IDR(aid)))
	    }
	}

	call ic_closed (AID_IC1(aid))
	call mfree (AID_SPECR(aid), TY_REAL)
	call mfree (AID_XR(aid), TY_DOUBLE)
	call mfree (AID_XT(aid), TY_DOUBLE)
	call mfree (AID_XTF(aid), TY_DOUBLE)
	call id_free (AID_IDR(aid))
	call mfree (AID_EVS(aid), TY_POINTER)
	call mfree (aid, TY_STRUCT)
end
