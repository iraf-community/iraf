# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# NETSTATUS -- Print the network status.

procedure t_netstatus()

begin
	call ki_shownet (STDOUT)
end
