# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# PERROR -- Fatal error in NSPP.

procedure perror()

begin
	call fatal (0, "Fatal error in Ncar system plot package")
end
