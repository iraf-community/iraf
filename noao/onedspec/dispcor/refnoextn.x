# REFNOEXTN -- Strip any image extensions

procedure refnoextn (spec)

char	spec[ARB]		# Image name

int	i, strlen()
bool	streq()

begin
	i = strlen (spec)
	call imgimage (spec, spec, i)

	i = strlen (spec)
	switch (spec[i]) {
	case 'h':
	    if (i > 3 && spec[i-3] == '.')
		spec[i-3] = EOS
	case 'l':
	    if (i > 2 && streq (spec[i-3], ".pl"))
		spec[i-2] = EOS
	}
end
