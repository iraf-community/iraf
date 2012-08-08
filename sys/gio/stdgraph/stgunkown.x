# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STG_UNKNOWN -- The unknown instruction.  Called by the interpreter whenever
# an unrecognized opcode is encountered.  Should never be called.

procedure stg_unknown (gki)

short	gki[ARB]		# the GKI instruction
int	fd, verbose
common	/stgcom/ fd, verbose

begin
	call fprintf (fd, "unknown\n")
end
