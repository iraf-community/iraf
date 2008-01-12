include "igi.h"

procedure igclose (igs)

pointer	igs		# igi parameters structure

pointer	igps		# Parameters structure

begin
	igps = PLOT_PARMS(igs)

	# String buffers
	call mfree (MG_DATAFN_P(igps),  TY_CHAR)
	call mfree (MG_COLNAME_P(igps), TY_CHAR)
	call mfree (MG_TITLE_P(igps),   TY_CHAR)
	call mfree (MG_XLABEL_P(igps),  TY_CHAR)
	call mfree (MG_YLABEL_P(igps),  TY_CHAR)
	call mfree (MG_LTYPE_P(igps),   TY_CHAR)
	call mfree (MG_PTYPE_P(igps),   TY_CHAR)
	call mfree (MG_TICKFMT_P(igps), TY_CHAR)

	# Free the data vectors
	if (MG_SDATAP(igps) != NULL)
	    call mfree (MG_SDATAP(igps), TY_REAL)
	if (MG_XDATAP(igps) != NULL)
	    call mfree (MG_XDATAP(igps), TY_REAL)
	if (MG_YDATAP(igps) != NULL)
	    call mfree (MG_YDATAP(igps), TY_REAL)
	if (MG_EDATAP(igps) != NULL)
	    call mfree (MG_EDATAP(igps), TY_REAL)
	if (MG_PDATAP(igps) != NULL)
	    call mfree (MG_PDATAP(igps), TY_REAL)
	if (MG_LDATAP(igps) != NULL)
	    call mfree (MG_LDATAP(igps), TY_REAL)
	if (MG_ZDATAP(igps) != NULL)
	    call mfree (MG_ZDATAP(igps), TY_REAL)
	
	# Plot parameters
	call mfree (igps, TY_STRUCT)

	# igi parameters
	call mfree (igs, TY_STRUCT)
end
