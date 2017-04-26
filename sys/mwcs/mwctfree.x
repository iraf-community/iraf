# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mwcs.h"

# MW_CTFREE -- Free a CTRAN (coordinate transformation) descriptor.  We keep
# track of all allocated CTRAN descriptors in the parent MWCS descriptor, and
# NULL the saved entry for a descriptor when it is freed, thus guaranteeing
# that a descriptor will be freed only once.

procedure mw_ctfree (ct)

pointer	ct		#U pointer to CTRAN descriptor

int	fn, i, j
pointer mw, fc
include	"mwcs.com"

begin
	if (ct != NULL) {
	    mw = CT_MW(ct)
	    if (mw != NULL)
		do i = 1, MAX_CTRAN
		    if (MI_CTRAN(mw,i) == ct) {
			# Free private storage for any input WCS functions.
			do j = 1, CT_NCALLI(ct) {
			    fc = CT_FCI(ct,j)
			    fn = WF_FN(FC_WF(fc))
			    if (FN_DESTROY(fn) != NULL)
				call zcall1 (FN_DESTROY(fn), fc)
			}
			# Free private storage for any output WCS functions.
			do j = 1, CT_NCALLO(ct) {
			    fc = CT_FCO(ct,j)
			    fn = WF_FN(FC_WF(fc))
			    if (FN_DESTROY(fn) != NULL)
				call zcall1 (FN_DESTROY(fn), fc)
			}
			# Free the main CTRAN descriptor.
			call mfree (ct, TY_STRUCT)
			MI_CTRAN(mw,i) = NULL
			break
		    }
	}
end
