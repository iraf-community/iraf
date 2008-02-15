# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mwset.h>
include	"mwcs.h"

# MW_SDEFWCS -- Set the default WCS.  This is the WCS indicated by the user
# environment variable "setwcs", if defined and the named WCS exists, else
# the first world system is used, else the physical system is used.

procedure mw_sdefwcs (mw)

pointer	mw			#I pointer to MWCS descriptor

pointer	sp, defwcs
int	envfind()

begin
	call smark (sp)
	call salloc (defwcs, SZ_FNAME, TY_CHAR)

	MI_WCS(mw) = NULL

	# Set the default WCS defined in the user environment, if defined
	# and the named WCS exists in this MWCS.

	if (envfind ("defwcs", Memc[defwcs], SZ_FNAME) > 0)
	    iferr (call mw_ssystem (mw, Memc[defwcs]))
		;

	# Otherwise, the default WCS is the first world system, if any,
	# else it is the physical system.  The first world system is WCS 3
	# as the physical and logical systems are systems 1 and 2 and are
	# always defined in any MWCS.

	if (MI_WCS(mw) == NULL) {
	    if (MI_NWCS(mw) >= 3)
		MI_WCS(mw) = MI_WCSP(mw,3)
	    else
		call mw_ssystem (mw, "physical")
	}

	call sfree (sp)
end
