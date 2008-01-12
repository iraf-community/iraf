# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLCPSET -- Close a pset.

procedure clcpset (pp)

pointer	pp		# pset descriptor

begin
	call mfree (pp, TY_STRUCT)
end
