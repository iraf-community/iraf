# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpio.h"

# QPIO_CLOSE -- Close the QPIO descriptor.  If writing to the event list,
# the output bucket is automatically flushed and the event list header updated.

procedure qpio_close (io)

pointer	io			#I QPIO descriptor

begin
	if (IO_DEBUG(io) > 1) {
	    call eprintf ("qpio_close (%xX)\n")
		call pargi (io)
	}

	call qpio_sync (io)

	if (IO_EX(io) != NULL && IO_EXCLOSE(io) == YES)
	    call qpex_close (IO_EX(io))
	if (IO_PL(io) != NULL && IO_PLCLOSE(io) == YES)
	    call pl_close (IO_PL(io))
	if (IO_FD(io) != NULL)
	    call close (IO_FD(io))

	if (IO_BP(io) != NULL)
	    call mfree (IO_BP(io), TY_SHORT)
	if (IO_RL(io) != NULL)
	    call mfree (IO_RL(io), TY_INT)
	if (IO_MINEVL(io) != NULL)
	    call mfree (IO_MINEVL(io), TY_SHORT)
	if (IO_MAXEVL(io) != NULL)
	    call mfree (IO_MAXEVL(io), TY_SHORT)
	if (IO_YLENVP(io) != NULL)
	    call mfree (IO_YLENVP(io), TY_INT)
	if (IO_YOFFVP(io) != NULL)
	    call mfree (IO_YOFFVP(io), TY_INT)
	if (IO_DD(io) != NULL)
	    call mfree (IO_DD(io), TY_STRUCT)
	if (IO_BBMASK(io) != NULL)
	    call plr_close (IO_BBMASK(io))
	if (IO_PARAM(io) != NULL)
	    call mfree (IO_PARAM(io), TY_CHAR)
	if (IO_MASK(io) != NULL)
	    call mfree (IO_MASK(io), TY_CHAR)

	call mfree (io, TY_STRUCT)
end
