# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "../lib/ids.h"

# IDS_SAVE -- Save the control state of the display, together with
# zero to all of the image and graphics planes.

procedure ids_save (data, n)

short	data[ARB]		# instruction data words
short	n			# count of data words

int	fd			# binary file output descriptor
short	i, j
short	frame[IDS_MAXIMPL+1]	# frames to save
short	graph[IDS_MAXGRPL+1]	# graph planes to save

include "../lib/ids.com"

begin
	# do we need to check n ??

	# determine file descriptor to write (opened by upper end)
	# ( assume upper end has saved whatever data it wanted and
	#   leaves fd pointing at control information offset)
	# then squirrel away the frame data

	fd = data[1]

	# image data

	i = 1
	j = 0
	repeat {
	    i = i + 1
	    j = j + 1
	    frame[j] = data[i]
	} until ( data[i] == IDS_EOD )
	call write(fd, j, SZ_SHORT)
	call write(fd, frame[1], j*SZ_SHORT)

	# graph data

	j = 0
	repeat {
	    i = i + 1
	    j = j + 1
	    graph[j] = data[i]
	} until ( data[i] == IDS_EOD )
	call write(fd, j, SZ_SHORT)
	call write(fd, graph[1], j*SZ_SHORT)

	# get all control information

	call zdev_save(fd)

	# get image data

	if ( frame[1] == IDS_EOD) {
	    for ( i = 1 ; i <= i_maxframes ; i = i + 1)
		frame[i] = i
	    frame[i+1] = IDS_EOD
	}
	if ( frame[1] != 0 ) {
	    for ( i = 1 ; frame[i] != IDS_EOD ; i = i + 1)
	        call zim_save (fd, frame[i])
	}

	# get graphics data

	if ( graph[1] == IDS_EOD) {
	    for ( i = 1 ; i <= i_maxgraph ; i = i + 1)
		graph[i] = i
	    graph[i+1] = IDS_EOD
	}
	if ( graph[1] != 0 ) {
	    for ( i = 1 ; graph[i] != IDS_EOD ; i = i + 1)
	        call zgr_save (fd, graph[i])
	}

	# upper end to close file
end
