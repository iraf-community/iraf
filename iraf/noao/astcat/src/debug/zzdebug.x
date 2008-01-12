include <fset.h>

# T_TEST2 -- Test Doug's idea of how to access a piece of memory as a
# binary file using the pushback technique. This works but is a bit
# inefficient.

procedure t_test2()

pointer	inbuf, outbuf
int	i, fd, nchars, ntimes
int	open(), read()

begin
	# Allocate a char array.
	call malloc (inbuf, 1000, TY_CHAR)
	call malloc (outbuf, 200, TY_CHAR)
	do i = 1, 1000
	    Memc[inbuf+i-1] = i

	# Open char array as a binary file.
	fd = open ("dev$null", READ_ONLY, BINARY_FILE)
	call fseti (fd, F_PBBSIZE, 1008)
	call unread (fd, Memc[inbuf], 1000)

	# Try to read the data.
	ntimes = 1
	nchars = read (fd, Memc[outbuf], 200)
	while (nchars != EOF) {
	    call printf ("ntimes=%d nchars=%d firstchar = %d\n")
		call pargi (ntimes)
		call pargi (nchars)
		call pargi (int(Memc[outbuf]))
	    nchars = read (fd, Memc[outbuf], 200)
	    ntimes = ntimes + 1
	}

	call close (fd)

	# Free char array.
	call mfree (inbuf, TY_CHAR)
	call mfree (outbuf, TY_CHAR)
end


# T_TEST3 -- Test Doug's idea of how to access a piece of memory as a
# binary file using the spool file technique. This works but is still a bit
# inefficient.

procedure t_test3()

pointer	inbuf, outbuf
int	i, fd, ntimes, nchars
int	open(), read()

begin
	# Allocate a char array.
	call malloc (inbuf, 1000, TY_CHAR)
	call malloc (outbuf, 200, TY_CHAR)
	do i = 1, 1000
	    Memc[inbuf+i-1] = i

	# Open char array as a binary file.
	fd = open ("dev$null", READ_WRITE, SPOOL_FILE)
	call write (fd, Memc[inbuf], 1000)
	call seek (fd, BOF)

	# Try to read the data.
	ntimes = 1
	nchars = read (fd, Memc[outbuf], 200)
	while (nchars != EOF) {
	    call printf ("ntimes=%d nchars=%d firstchar = %d\n")
		call pargi (ntimes)
		call pargi (nchars)
		call pargi (int(Memc[outbuf]))
	    nchars = read (fd, Memc[outbuf], 200)
	    ntimes = ntimes + 1
	}

	call close (fd)

	# Free char array.
	call mfree (inbuf, TY_CHAR)
	call mfree (outbuf, TY_CHAR)
end

# T_TEST5 -- Test Doug's idea of how to access a piece of memory as a
# text file using the spool file technique. This works but is still a bit
# inefficient.

procedure t_test5()

pointer	inbuf, outbuf
int	i, fd, ntimes, nchars
long	note()
int	open(), getline()

begin
	# Allocate a char array.
	call malloc (inbuf, 1000, TY_CHAR)
	call malloc (outbuf, 200, TY_CHAR)
	do i = 1, 200
	    Memc[inbuf+i-1] = 'a'
	Memc[inbuf+199] = '\n'
	do i = 201, 400
	    Memc[inbuf+i-1] = 'b'
	Memc[inbuf+399] = '\n'
	do i = 401, 600
	    Memc[inbuf+i-1] = 'c'
	Memc[inbuf+599] = '\n'
	do i = 601, 800
	    Memc[inbuf+i-1] = 'd'
	Memc[inbuf+799] = '\n'
	do i = 801, 1000
	    Memc[inbuf+i-1] = 'e'
	Memc[inbuf+999] = '\n'

	# Open char array as a binary file.
	fd = open ("dev$null", READ_WRITE, SPOOL_FILE)
	call write (fd, Memc[inbuf], 1000)
	call seek (fd, BOF)

	# Try to read the data.
	ntimes = 1
	#nchars = read (fd, Memc[outbuf], 200)
	nchars = getline (fd, Memc[outbuf])
	while (nchars != EOF) {
	    call printf ("ntimes=%d nchars=%d firstchar = %c seek=%d\n")
		call pargi (ntimes)
		call pargi (nchars)
		call pargc (Memc[outbuf])
		call pargl (note(fd))
	    #nchars = read (fd, Memc[outbuf], 200)
	    nchars = getline (fd, Memc[outbuf])
	    ntimes = ntimes + 1
	}

	call close (fd)

	# Free char array.
	call mfree (inbuf, TY_CHAR)
	call mfree (outbuf, TY_CHAR)
end
