# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

define	SZ_PROGRAM	256

# STG_CTRL -- Fetch an encoder format string from the graphcap entry and
# use it to encode zero, one, or two integer arguments into a control string.
# Put the control string to the output device.

procedure stg_ctrl (cap)

char	cap[ARB]		# name of device capability to be encoded
pointer	sp, prog
int	stg_encode(), ttygets()
include	"stdgraph.com"

begin
	call smark (sp)
	call salloc (prog, SZ_PROGRAM, TY_CHAR)

	# Fetch the program from the graphcap file.  Zero is returned if the
	# device does not have the named capability, in which case the function
	# is inapplicable and should be ignored.

	if (ttygets (g_tty, cap, Memc[prog], SZ_PROGRAM) > 0) {
	    # Encode the output string and write the encoded string to the
	    # output file.
	    g_reg[E_IOP] = 1
	    if (stg_encode (Memc[prog], g_mem, g_reg) == OK) {
		g_mem[g_reg[E_IOP]] = EOS
		call ttyputs (g_out, g_tty, g_mem, 1)
	    }
	}

	call sfree (sp)
end


# STG_CTRL1 -- Encode one integer argument.

procedure stg_ctrl1 (cap, arg1)

char	cap[ARB]		# device capability
int	arg1
include	"stdgraph.com"

begin
	g_reg[1] = arg1
	call stg_ctrl (cap)
end


# STG_CTRL2 -- Encode two integer arguments.

procedure stg_ctrl2 (cap, arg1, arg2)

char	cap[ARB]		# device capability
int	arg1, arg2
include	"stdgraph.com"

begin
	g_reg[1] = arg1
	g_reg[2] = arg2
	call stg_ctrl (cap)
end


# STG_CTRL3 -- Encode three integer arguments.

procedure stg_ctrl3 (cap, arg1, arg2, arg3)

char	cap[ARB]		# device capability
int	arg1, arg2, arg3
include	"stdgraph.com"

begin
	g_reg[1] = arg1
	g_reg[2] = arg2
	g_reg[3] = arg3
	call stg_ctrl (cap)
end
