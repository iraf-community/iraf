# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# RG_FREE -- Free ranges memory.

procedure rg_free (rg)

pointer	rg				# Ranges

begin
	if (rg != NULL) {
	    call mfree (rg, TY_STRUCT)
	    rg = NULL
	}
end
