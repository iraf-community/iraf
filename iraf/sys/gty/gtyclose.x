# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GTYCLOSE -- Close the tty terminal descriptor opened with TTYOPEN.

procedure gtyclose (tty)

pointer	tty

begin
	call mfree (tty, TY_STRUCT)
end
