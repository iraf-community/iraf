# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>
include	"pllseg.h"

# PLL_NEXTSEG -- Internal routine called by the PLLSEG.H routines to get the
# next segment of a line list.

procedure pll_nextseg (ll, ld)

short	ll[ARB]			#I input line list
int	ld[LEN_PLLDES]		#I list list i/o descriptor

int	ip
int	opcode, data

begin
	for (ip = ld_ip(ld);  ip <= LL_LEN(ll);  ip = ld_ip(ld)) {
	    ld_ip(ld) = ip + 1
	    opcode = I_OPCODE(ll[ip])
	    data   = I_DATA(ll[ip])

	    switch (opcode) {
	    case I_ZN:
		ld_nleft(ld) = data
		ld_value(ld) = 0
		return
	    case I_HN:
		ld_nleft(ld) = data
		ld_value(ld) = ld_hi(ld)
		return
	    case I_PN:
		ld_nleft(ld) = data - 1
		ld_value(ld) = 0
		ld_next_nleft(ld) = 1
		ld_next_value(ld) = ld_hi(ld)
		return

	    case I_SH:
		ip = ip + 1
		ld_ip(ld) = ip + 1
		ld_hi(ld) = (int(ll[ip]) * I_SHIFT) + data
	    case I_IH:
		ld_hi(ld) = ld_hi(ld) + data
	    case I_DH:
		ld_hi(ld) = ld_hi(ld) - data

	    case I_IS, I_DS:
		if (opcode == I_IS)
		    ld_hi(ld) = ld_hi(ld) + data
		else
		    ld_hi(ld) = ld_hi(ld) - data

		ld_nleft(ld) = 1
		ld_value(ld) = ld_hi(ld)
		return
	    }
	}

	ld_value(ld) = 0
end
