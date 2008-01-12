# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include "../lib/ids.h"

# IDS_ESCAPE -- Pass a device dependent instruction on to the kernel.  
# Most of the display control work is done here.

procedure ids_escape (fn, instruction, nwords)

int	fn				# function code
short	instruction[ARB]		# instruction data words
int	nwords				# length of instruction

pointer p,q
int	ids_dcopy()
short	frames[IDS_MAXIMPL+2]		# storage for frame data
short	color[IDS_MAXGCOLOR+1]		# ditto for color
short	bitpl[IDS_MAXBITPL+1]		# ditto for graphics bit plane
short	quad[5]				# 4 quadrant information
int	count, count2, total
int	junk

short	gki[GKI_ESCAPE_LEN]
data	gki[1] /BOI/, gki[2] /GKI_ESCAPE/

include "../lib/ids.com"

begin
	switch(fn) {
	    
	    case IDS_RESET:
		call ids_reset(instruction[1])

	    case IDS_SET_IP:
		p = IDS_FRAME(i_kt)
		count = ids_dcopy(instruction[1], Mems[p])
		call ids_expand(Mems[p],i_maxframes, true)
		q = IDS_BITPL(i_kt)
		junk = ids_dcopy ( instruction[count+1], Mems[q])
		call ids_expand(Mems[q],IDS_MAXBITPL, false)
		i_image = true
		call zsetup (Mems[p], Mems[q], i_image)

	    case IDS_SET_GP:
		p = IDS_FRAME(i_kt)
		count = ids_dcopy(instruction[1], Mems[p])
		call ids_expand(Mems[p],i_maxgraph, false)
		q = IDS_BITPL(i_kt)
		junk = ids_dcopy ( instruction[count+1], Mems[q])
		call ids_expand(Mems[q],IDS_MAXBITPL, false)
		i_image = false
		call zsetup (Mems[p], Mems[q], i_image)

	    case IDS_DISPLAY_I:
		count = ids_dcopy(instruction[2], frames[1])
		call ids_expand(frames[1], i_maxframes, true)
		count2 = ids_dcopy (instruction[2+count], color[1])
		call ids_expand(color[1], IDS_MAXGCOLOR, false)
		total = count + count2
		count = ids_dcopy(instruction[total+2], quad[1])
		call ids_expand(quad[1], 4, false)
		call zdisplay_i(instruction[1], frames[1], color, quad)

	    case IDS_DISPLAY_G:
		count = ids_dcopy(instruction[2], bitpl[1])
		call ids_expand(bitpl[1], i_maxgraph, false)
		count2 = ids_dcopy (instruction[2+count], color[1])
		call ids_expand(color[1], IDS_MAXGCOLOR, false)
		total = count + count2
		count = ids_dcopy(instruction[total+2], quad[1])
		call ids_expand(quad[1], 4, false)
		call zdisplay_g(instruction[1], bitpl, color, quad)

	    case IDS_SAVE:
		call idssave(instruction[1], nwords)

	    case IDS_RESTORE:
		call idsrestore(instruction[1], nwords)

	    case IDS_CONTROL:
		count = ids_dcopy(instruction[IDS_CTRL_FRAME], frames[1])
		call ids_expand(frames[1], i_maxframes, true)
		count2 = ids_dcopy (instruction[IDS_CTRL_FRAME+count], color[1])
		call ids_expand(color[1], IDS_MAXGCOLOR, false)
		total = count + count2
		call zcontrol(instruction[IDS_CTRL_REG],
			instruction[IDS_CTRL_RW],
			frames[1], color[1],
			instruction[total+IDS_CTRL_FRAME],
			instruction[IDS_CTRL_N],
			instruction[total+IDS_CTRL_FRAME+1] )
		# if a read, would like to return the information in gki format
		# but no mechanism (yet?) for that
	}
end

# IDS_DCOPY -- copy frame and bitplane information; return the number of
# items copied, including the IDS_EOD (whose presence is required and assumed).

int procedure ids_dcopy(from, to)

short	from[ARB]		# from this storage
short	to[ARB]			# to this area

int	i			# count

begin
	i = 0
	repeat {
	    i = i + 1
	    to[i] = from[i]
	} until ( to[i] == IDS_EOD )
	return (i)
end
