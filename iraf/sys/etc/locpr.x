# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# LOCPR -- Return the entry point address of a procedure, suitable for input
# to a ZCALL prcocedure to call the target procedure indirectly.

int procedure locpr (proc)

extern	proc()			# external procedure
int	epa

begin
	call zlocpr (proc, epa)
	return (epa)
end
