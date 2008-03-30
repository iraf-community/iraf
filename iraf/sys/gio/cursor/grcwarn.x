# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GRC_WARN -- Called in an error handler to intercept an error message string
# and write it to the workstation in the status line.

procedure grc_warn (fd)

int	fd			# output stream

size_t	sz_val
int	errcode
pointer	sp, msg, ip
int	errget()

begin
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (msg, sz_val, TY_CHAR)

	errcode = errget (Memc[msg], SZ_LINE)
	for (ip=msg;  Memc[ip] != EOS && Memc[ip] != '\n';  ip=ip+1)
	    ;
	Memc[ip] = EOS

	call stg_putline (fd, " - ")
	call stg_putline (fd, Memc[msg])

	call sfree (sp)
end
