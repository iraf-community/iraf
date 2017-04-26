# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GETPID -- Get the process id.

int procedure getpid()

int	pid

begin
	call zgtpid (pid)
	return (pid)
end
