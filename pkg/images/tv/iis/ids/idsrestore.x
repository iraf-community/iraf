# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "../lib/ids.h"

# IDS_RESTORE -- Restore the control state of the display, together with
# zero to all of the image and graphics planes.

procedure ids_restore (data, n)

short	data[ARB]		# instruction data words
short	n			# number of data words

int	fd			# binary file output descriptor
short	i, j
short	frame[IDS_MAXIMPL+1]	# frames to save
short	graph[IDS_MAXGRPL+1]	# graph planes to save
short	buffer[IDS_MAXDATA]	# for data storage

include "../lib/ids.com"

begin
	# determine file descriptor to read (opened by upper end)
	# ( assume upper end has retrieved whatever data it stored and
	#   leaves fd pointing at control information offset)
	# then retrieve the frame data

	fd = data[1]

	# image data

	call read(fd, i, SZ_SHORT)
	call read(fd, buffer, i)
	j = 0
	i = 0
	repeat {
	    i = i + 1
	    j = j + 1
	    frame[j] = buffer[i]
	} until ( (buffer[i] == IDS_EOD) || ( j == i_maxframes) )
	frame[i+1] = IDS_EOD

	# graph data

	call read(fd, i, SZ_SHORT)
	call read(fd, buffer, i)
	i = 0
	j = 0
	repeat {
	    i = i + 1
	    j = j + 1
	    graph[j] = buffer[i]
	} until ( (buffer[i] == IDS_EOD) || ( j == i_maxgraph) )
	graph[i+1] = IDS_EOD

	# get all control information

	call zdev_restore(fd)

	# get image data

	if ( frame[1] == IDS_EOD) {
	    for ( i = 1 ; i <= i_maxframes ; i = i + 1)
		frame[i] = i
	    frame[i+1] = IDS_EOD
	}
	if ( frame[1] != 0 ) {
	    for ( i = 1 ; frame[i] != IDS_EOD ; i = i + 1)
	        call zim_restore (fd, frame[i])
	}

	# get graphics data

	if ( graph[1] == IDS_EOD) {
	    for ( i = 1 ; i <= i_maxgraph ; i = i + 1)
		graph[i] = i
	    graph[i+1] = IDS_EOD
	}
	if ( graph[1] != 0 ) {
	    for ( i = 1 ; graph[i] != IDS_EOD ; i = i + 1)
	        call zgr_restore (fd, graph[i])
	}

	# upper end to close file
end
