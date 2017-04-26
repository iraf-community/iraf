# ID_NOEXTN -- Remove standard image extensions.

procedure id_noextn (image)

char	image[ARB]	# Image name

int	strlen()

begin
	call xt_imroot (image, image, strlen (image))
end
