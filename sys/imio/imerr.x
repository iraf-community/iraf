# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMERR -- Format an error message for the named image and call error.
# format of error message:  ERROR (nnn, "message ('imname')").

procedure imerr (image_name, errcode)

char	image_name[ARB]
int	errcode

begin
	call syserrs (errcode, image_name)
end
