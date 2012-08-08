# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# XWHEN -- Post an exception handler.

procedure xwhen (signal, handler, old_handler)

int	signal				# signal to be caught
int	handler				# epa of user supplied exception handler
int	old_handler			# epa of old handler, if any

begin
	call zxwhen (signal, handler, old_handler)
end
