# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMDELETE -- Delete an image.

procedure imdelete (image)

char	image[ARB]

begin
	call iki_init()
	call iki_delete (image)
end
