# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>

# XMJBUF -- Return a char pointer to the IRAF main interpreter context restart
# buffer (for ZDOJMP restarts).

procedure xmjbuf (bp)

pointer	bp			#O pointer to jumpbuf

int	a_jb, a_mem
int	jumpbuf[LEN_JUMPBUF]
common	/JUMPCOM/ jumpbuf

begin
	call zlocva (jumpbuf[1], a_jb)
	call zlocva (Memc, a_mem)
	bp = a_jb - a_mem + 1
end
