# ID_NOEXTN -- Remove standard image extensions.

procedure id_noextn (image)

char	image[ARB]	# Image name

int	i, strldx()

begin
	# .??h
	i = strldx ("h", image)
	if (i > 4)
	   if (image[i-3] == '.')
	       call strcpy (image[i+1], image[i-3], ARB)
end
