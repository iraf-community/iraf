# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GETCHAR -- Get a character from the standard input.

char procedure getchar (ch)

char	ch			# character (output)
char	getc()

begin
	return (getc (STDIN, ch))
end
