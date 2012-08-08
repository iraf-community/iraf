# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRCPY -- Copy string s1 to s2.

procedure strcpy (s1, s2, maxch)

char	s1[ARB], s2[ARB]
int	maxch, i

begin
	do i = 1, maxch {
	    s2[i] = s1[i]
	    if (s2[i] == EOS)
		return
	}

	s2[maxch+1] = EOS
end
