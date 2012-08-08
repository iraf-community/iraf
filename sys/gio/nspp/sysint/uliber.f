        subroutine uliber (errcode, pkerrmsg, msglen)

	character*80 pkerrmsg
	integer errcode, msglen
	integer*2 sppmsg(81)
	integer SZLINE
	parameter (SZLINE=80)

c unpack the fortran character string, call fulib to output the string.
c
	call f77upk (pkerrmsg, sppmsg, SZLINE)
	call fulib  (errcode, sppmsg, msglen)

	end
