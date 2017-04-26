include	<mach.h>


# GET_NEXT_IMAGE -- Use root filename and ranges string (if any) to
#                  generate the next image filename. Return EOF
#                  when image list is exhausted.

int procedure get_next_image (infile, records, nrecs, image, sz_name)

int	infile, records[ARB], nrecs, sz_name
char	image[sz_name]

int	next_num, stat
int	flag1, flag2, flag3
char	image_0[SZ_FNAME]

int	clgfil(), get_next_entry(), strlen()

common	/gnicom/ flag1, flag2

data	flag3/YES/

begin
	# Reset initializer, record counter, and get root name
	if ((flag1 == YES) || (flag3 == YES)) {
	    next_num  = -1
	    call rst_get_entry ()
	}

	# If no ranges specified, act like template expander
	if (nrecs == MAX_INT) {
	    stat = clgfil (infile, image, sz_name)

	# Otherwise append record numbers to first template expansion
	} else {
	    if (flag1 == YES) {
		stat = clgfil (infile, image_0, sz_name)
		if (stat == EOF)
		    return (stat)
	    }

	    stat = get_next_entry (records, next_num)
	    if (stat != EOF) {
	 	call strcpy (image_0, image, sz_name)
		call sprintf (image[strlen(image)+1], sz_name, ".%04d")
		call pargi (next_num)
	    }
	}

	flag1 = NO
	flag3 = NO
	return (stat)
end


# Reset the initialization parameter to TRUE

procedure reset_next_image ()

int	flag1, flag2
common	/gnicom/ flag1, flag2

begin
	flag1 = YES
end


# GET_NEXT_ENTRY -- Given a list of ranges and the current file number,
# find and return the next file number in order of entry.  
# EOF is returned at the end of the list.

int procedure get_next_entry (ranges, number)

int	ranges[ARB]		# Range array
int	number			# Both input and output parameter

int	ip, first, last, step, next_number, remainder
int	flag1, flag2, flag3

common	/gnicom/ flag1, flag2

data	flag3/YES/

begin
	number = number + 1
	next_number = MAX_INT
	if ((flag2 == YES) || (flag3 == YES)) {
	    ip = 1
	    flag2 = NO
	    flag3 = NO
	}

	first = min (ranges[ip], ranges[ip+1])
	last = max (ranges[ip], ranges[ip+1])
	step = ranges[ip+2]

	if (number >= first && number <= last) {
	    remainder = mod (number - first, step)
	    if (remainder == 0)
		return (number)
	    if (number - remainder + step <= last)
		next_number = number - remainder + step
	    else
		go to 10

	} else if (first > number)
	    next_number = min (next_number, first)

	else {
10	    ip = ip + 3
	    if (ranges[ip] != -1 && ranges[ip+1] !=0 && ranges[ip+2] !=0)
		next_number = min (ranges[ip], ranges[ip+1])
	}

	if (next_number == MAX_INT) {
	    ip = 1
	    flag2 = YES
	    return (EOF)

	} else {
	    number = next_number
	    return (number)
	}
end

procedure rst_get_entry ()

int	first, flag2
common	/gnicom/ first, flag2

begin
	flag2 = YES
end
