# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FILERR -- Take an error action, including the name of the file in the
# error message.  Note that the order of the arguments is reversed in
# filerr and syserr; this is unfortunate, but too hard to change at this
# point.  The logic behind this (if there is any) is that the main operand
# of filerr is the file name, that of syserr the error number.

procedure filerr (fname, errcode)

char	fname[ARB]
int	errcode

begin
	call syserrs (errcode, fname)
end
