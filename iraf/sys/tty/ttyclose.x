# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# TTYCLOSE -- Close the tty terminal descriptor opened with TTYOPEN.

procedure ttyclose (tty)

pointer	tty

begin
	call mfree (tty, TY_STRUCT)
end
