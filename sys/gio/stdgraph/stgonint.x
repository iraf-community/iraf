# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<xwhen.h>
include	"stdgraph.h"

# STG_ONINT -- Interrupt handler for the stdgraph kernel.  If an interrupt
# occurs while we are posted to an exception, branch to the last ZSVJMP.
# (This library procedure is not currently used by the kernel).

procedure stg_onint (vex, next_handler)

int	vex			# virtual exception
int	next_handler		# next exception handler in chain
int	jmpbuf[LEN_JUMPBUF]
common	/stgxin/ jmpbuf

begin
	call xer_reset()
	call zdojmp (jmpbuf, vex)
end
