# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STGESCAPE.X -- Stdgraph kernel escape handing code.  This is the interface
# between the stdgraph kernel and any supported escape packages.  These driver
# routines return TRUE if they recognize the escape and it is private to the
# package, FALSE if the other escape packages may also be interested in the
# routine.
#
#	stg_escape		standard GKI escape entry point
#
#	sge_wstran		transform and output escape
#	sge_spoolesc		process and escape into frame buffer	
#
# To add support for a new package of escapes, and entry for the driver routine
# for each family of escapes must be added to each of these procedures.


# STG_ESCAPE -- Pass a device dependent instruction on to the kernel.  
# The stdgraph kernel does not have any escape functions at present.

procedure stg_escape (fn, instruction, nwords)

int	fn			#I function code
short	instruction[ARB]	#I instruction data words
int	nwords			#I length of instruction

bool	sgm_execute()		# GIM (Gterm) imaging excapes

begin
	if (sgm_execute (fn, instruction, nwords))
	    return
end


# SGE_WSTRAN -- Stdgraph escape handling routine called by an interactive
# client (e.g the CL in cursor mode) to apply the workstation transformation
# to a escape and execute the escape.  This routine is called for all
# escapes regardless of whether any transformation is necessary, leaving
# it up to the escape code to decide what to do.

procedure sge_wstran (fn, instruction, x1,y1, x2,y2)

int	fn			#I escape sequence function opcode
short	instruction[ARB]	#I escape instruction data
real	x1, y1			#I NDC coords of display rect
real	x2, y2			#I NDC coords of display rect

bool	sgm_wstran()		# GIM (Gterm) imaging excapes

begin
	if (sgm_wstran (fn, instruction, x1,y1, x2,y2))
	    return
end


# SGE_WSENABLE -- Stdgraph escape handling routine called by an
# interactive client (e.g the CL in cursor mode) to test whether cursor mode
# scaling of graphics instructions is enabled when cursor mode zoom/pan is
# done.  Cursor mode scaling may be disabled if the kernel or graphics device
# does the scaling itself.

bool procedure sge_wsenable ()

bool	enable
bool	sgm_wsenable()

begin
	if (sgm_wsenable (enable))
	    return (enable)
end


# SGE_SPOOLESC -- Stdgraph escape handling routine called by an interactive
# client (e.g the CL in cursor mode) to retain, delete, or edit an escape
# instruction stored in a frame buffer.  Ordinary drawing instructions are
# normally retained.  If the instruction should only be executed when issued
# it should be deleted.  Sometimes an instruction is edited or replaced by
# a different one to be executed the next time the buffered graphics is drawn.
# Sometimes when an instruction is seen earlier instructions must be edited
# or deleted.  This routine is called for all escapes, it is up to the escape
# code to decide what to do.  The delete instruction callback is called as
# delete_fcn(tr,gki) to delete the instruction pointed to by GKI.

procedure sge_spoolesc (tr, gki, fn, instruction, bp, buftop, delete_fcn)

pointer	tr			#I arg to delete_fcn
pointer	gki			#I pointer to escape instruction
int	fn			#U escape sequence function opcode
short	instruction[ARB]	#U escape instruction data
pointer	bp			#I frame buffer pointer
pointer	buftop			#I top+1 of buffered data
int	delete_fcn		#I function called to delete an instruction

bool	sgm_spoolesc()		# GIM (Gterm) imaging excapes

begin
	if (sgm_spoolesc (tr, gki, fn, instruction, bp, buftop, delete_fcn))
	    return
end
