# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<xwhen.h>

define	LEN_SAVE	10

# INTR_DISABLE, INTR_ENABLE -- Disable interrupts to protect a critical
# section of code.  The interrupt handler is saved on a stack and restored
# when interupts are reenabled.

procedure intr_disable()

int	sp
int	save[LEN_SAVE]
common	/zintde/ sp, save

begin
	sp = sp + 1
	if (sp > LEN_SAVE)
	    call sys_panic (1, "interrupt save stack overflow")

	call zxwhen (X_INT, X_IGNORE, save[sp])
end


# INTR_ENABLE -- Reenable interrupts (restore saved interrupt handler).

procedure intr_enable()

int	junk
int	sp
int	save[LEN_SAVE]
common	/zintde/ sp, save

begin
	if (sp <= 0)
	    call sys_panic (1, "interrupt save stack underflow")

	call zxwhen (X_INT, save[sp], junk)
	sp = sp - 1
end


# INTR_RESET -- Clear the interrupt handler save stack.

procedure intr_reset()

int	sp
int	save[LEN_SAVE]
common	/zintde/ sp, save

begin
	sp = 0
end
