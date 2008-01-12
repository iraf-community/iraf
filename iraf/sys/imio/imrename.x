# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMRENAME -- Rename an image.

procedure imrename (old, new)

char	old[ARB]		# old image name
char	new[ARB]		# new image name

begin
	call iki_init()
	call iki_rename (old, new)
end
