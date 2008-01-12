procedure listout (file)

#  Copy lines of a text file to STDOUT

## 12 June 1992  ZGL

char	file[ARB]		# File name

int	dp
pointer	sp, lb

int	open(), getline()

begin
	dp = open (file, READ_ONLY, TEXT_FILE)

	call smark (sp)
	call salloc (lb, SZ_LINE, TY_CHAR)

	while (getline (dp, Memc[lb]) != EOF)
	    call printf (Memc[lb])

	call close (dp)
	call sfree (sp)
end
