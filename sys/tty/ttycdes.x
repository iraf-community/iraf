# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# TTYCDES -- Close a terminal opened with TTYODES or TTYGDES.

procedure ttycdes (tty)

pointer	tty

begin
	call ttyclose (tty)
end
