# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>



# BLKRP -- Block replicate an image.

procedure blkrpd (in, out, blkfac)

pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer
int	blkfac[ARB]		# Block replication factors

int	i, j, ndim, nin, nout
pointer	sp, buf, buf1, buf2, buf3, v1, v2, v3, ptrin, ptrout
pointer	imgl1d(), impl1d(), imgnld(), impnld()

begin
	call smark (sp)

	ndim = IM_NDIM(in)
	nin = IM_LEN(in, 1)
	nout = nin * blkfac[1]
	IM_LEN(out,1) = nout

	if (ndim == 1) {
	    # For one dimensional images do the replication directly.

	    buf1 = imgl1d (in)
	    buf2 = impl1d (out)
	    ptrin = buf1
	    ptrout = buf2
	    do i = 1, nin {
		do j = 1, blkfac[1] {
		    Memd[ptrout] = Memd[ptrin]
		    ptrout = ptrout + 1
		}
		ptrin = ptrin + 1
	    }

	} else {
	    # For higher dimensional images use line access routines.

	    do i = 2, ndim
	        IM_LEN(out,i) = IM_LEN(in,i) * blkfac[i]

	    # Allocate memory.
	    call salloc (buf, nout, TY_DOUBLE)
	    call salloc (v1, IM_MAXDIM, TY_LONG)
	    call salloc (v2, IM_MAXDIM, TY_LONG)
	    call salloc (v3, IM_MAXDIM, TY_LONG)

	    # Initialize the input line vector and the output section vectors.
	    call amovkl (long(1), Meml[v1], IM_MAXDIM)
	    call amovkl (long(1), Meml[v2], IM_MAXDIM)
	    call amovkl (long(1), Meml[v3], IM_MAXDIM)

	    # For each output line compute a block replicated line from the
	    # input image.  If the line replication factor is greater than
	    # 1 then simply repeat the input line.  This algorithm is
	    # sequential in both the input and output image though the
	    # input image will be recycled for each repetition of higher
	    # dimensions.

	    while (impnld (out, buf2, Meml[v2]) != EOF) {
		# Get the input vector corresponding to the output line.
		do i = 2, ndim
		    Meml[v1+i-1] = (Meml[v3+i-1] - 1) / blkfac[i] + 1
		i = imgnld (in, buf1, Meml[v1])

		# Block replicate the columns.
		if (blkfac[1] == 1)
		    buf3 = buf1
		else {
	            ptrin = buf1
	            ptrout = buf
	            do i = 1, nin {
		        do j = 1, blkfac[1] {
		            Memd[ptrout] = Memd[ptrin]
		            ptrout = ptrout + 1
		        }
		        ptrin = ptrin + 1
	            }
		    buf3 = buf
		}

		# Copy the input line to the output line.
		call amovd (Memd[buf3], Memd[buf2], nout)

		# Repeat for each repetition of the input line.
		for (i=2;  i <= blkfac[2];  i=i+1) {
	            j = impnld (out, buf2, Meml[v2])
		    call amovd (Memd[buf3], Memd[buf2], nout)
		}

		call amovl (Meml[v2], Meml[v3], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end


# BLKRP -- Block replicate an image.

procedure blkrpl (in, out, blkfac)

pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer
int	blkfac[ARB]		# Block replication factors

int	i, j, ndim, nin, nout
pointer	sp, buf, buf1, buf2, buf3, v1, v2, v3, ptrin, ptrout
pointer	imgl1l(), impl1l(), imgnll(), impnll()

begin
	call smark (sp)

	ndim = IM_NDIM(in)
	nin = IM_LEN(in, 1)
	nout = nin * blkfac[1]
	IM_LEN(out,1) = nout

	if (ndim == 1) {
	    # For one dimensional images do the replication directly.

	    buf1 = imgl1l (in)
	    buf2 = impl1l (out)
	    ptrin = buf1
	    ptrout = buf2
	    do i = 1, nin {
		do j = 1, blkfac[1] {
		    Meml[ptrout] = Meml[ptrin]
		    ptrout = ptrout + 1
		}
		ptrin = ptrin + 1
	    }

	} else {
	    # For higher dimensional images use line access routines.

	    do i = 2, ndim
	        IM_LEN(out,i) = IM_LEN(in,i) * blkfac[i]

	    # Allocate memory.
	    call salloc (buf, nout, TY_LONG)
	    call salloc (v1, IM_MAXDIM, TY_LONG)
	    call salloc (v2, IM_MAXDIM, TY_LONG)
	    call salloc (v3, IM_MAXDIM, TY_LONG)

	    # Initialize the input line vector and the output section vectors.
	    call amovkl (long(1), Meml[v1], IM_MAXDIM)
	    call amovkl (long(1), Meml[v2], IM_MAXDIM)
	    call amovkl (long(1), Meml[v3], IM_MAXDIM)

	    # For each output line compute a block replicated line from the
	    # input image.  If the line replication factor is greater than
	    # 1 then simply repeat the input line.  This algorithm is
	    # sequential in both the input and output image though the
	    # input image will be recycled for each repetition of higher
	    # dimensions.

	    while (impnll (out, buf2, Meml[v2]) != EOF) {
		# Get the input vector corresponding to the output line.
		do i = 2, ndim
		    Meml[v1+i-1] = (Meml[v3+i-1] - 1) / blkfac[i] + 1
		i = imgnll (in, buf1, Meml[v1])

		# Block replicate the columns.
		if (blkfac[1] == 1)
		    buf3 = buf1
		else {
	            ptrin = buf1
	            ptrout = buf
	            do i = 1, nin {
		        do j = 1, blkfac[1] {
		            Meml[ptrout] = Meml[ptrin]
		            ptrout = ptrout + 1
		        }
		        ptrin = ptrin + 1
	            }
		    buf3 = buf
		}

		# Copy the input line to the output line.
		call amovl (Meml[buf3], Meml[buf2], nout)

		# Repeat for each repetition of the input line.
		for (i=2;  i <= blkfac[2];  i=i+1) {
	            j = impnll (out, buf2, Meml[v2])
		    call amovl (Meml[buf3], Meml[buf2], nout)
		}

		call amovl (Meml[v2], Meml[v3], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end


# BLKRP -- Block replicate an image.

procedure blkrpr (in, out, blkfac)

pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer
int	blkfac[ARB]		# Block replication factors

int	i, j, ndim, nin, nout
pointer	sp, buf, buf1, buf2, buf3, v1, v2, v3, ptrin, ptrout
pointer	imgl1r(), impl1r(), imgnlr(), impnlr()

begin
	call smark (sp)

	ndim = IM_NDIM(in)
	nin = IM_LEN(in, 1)
	nout = nin * blkfac[1]
	IM_LEN(out,1) = nout

	if (ndim == 1) {
	    # For one dimensional images do the replication directly.

	    buf1 = imgl1r (in)
	    buf2 = impl1r (out)
	    ptrin = buf1
	    ptrout = buf2
	    do i = 1, nin {
		do j = 1, blkfac[1] {
		    Memr[ptrout] = Memr[ptrin]
		    ptrout = ptrout + 1
		}
		ptrin = ptrin + 1
	    }

	} else {
	    # For higher dimensional images use line access routines.

	    do i = 2, ndim
	        IM_LEN(out,i) = IM_LEN(in,i) * blkfac[i]

	    # Allocate memory.
	    call salloc (buf, nout, TY_REAL)
	    call salloc (v1, IM_MAXDIM, TY_LONG)
	    call salloc (v2, IM_MAXDIM, TY_LONG)
	    call salloc (v3, IM_MAXDIM, TY_LONG)

	    # Initialize the input line vector and the output section vectors.
	    call amovkl (long(1), Meml[v1], IM_MAXDIM)
	    call amovkl (long(1), Meml[v2], IM_MAXDIM)
	    call amovkl (long(1), Meml[v3], IM_MAXDIM)

	    # For each output line compute a block replicated line from the
	    # input image.  If the line replication factor is greater than
	    # 1 then simply repeat the input line.  This algorithm is
	    # sequential in both the input and output image though the
	    # input image will be recycled for each repetition of higher
	    # dimensions.

	    while (impnlr (out, buf2, Meml[v2]) != EOF) {
		# Get the input vector corresponding to the output line.
		do i = 2, ndim
		    Meml[v1+i-1] = (Meml[v3+i-1] - 1) / blkfac[i] + 1
		i = imgnlr (in, buf1, Meml[v1])

		# Block replicate the columns.
		if (blkfac[1] == 1)
		    buf3 = buf1
		else {
	            ptrin = buf1
	            ptrout = buf
	            do i = 1, nin {
		        do j = 1, blkfac[1] {
		            Memr[ptrout] = Memr[ptrin]
		            ptrout = ptrout + 1
		        }
		        ptrin = ptrin + 1
	            }
		    buf3 = buf
		}

		# Copy the input line to the output line.
		call amovr (Memr[buf3], Memr[buf2], nout)

		# Repeat for each repetition of the input line.
		for (i=2;  i <= blkfac[2];  i=i+1) {
	            j = impnlr (out, buf2, Meml[v2])
		    call amovr (Memr[buf3], Memr[buf2], nout)
		}

		call amovl (Meml[v2], Meml[v3], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end


# BLKRP -- Block replicate an image.

procedure blkrps (in, out, blkfac)

pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer
int	blkfac[ARB]		# Block replication factors

int	i, j, ndim, nin, nout
pointer	sp, buf, buf1, buf2, buf3, v1, v2, v3, ptrin, ptrout
pointer	imgl1s(), impl1s(), imgnls(), impnls()

begin
	call smark (sp)

	ndim = IM_NDIM(in)
	nin = IM_LEN(in, 1)
	nout = nin * blkfac[1]
	IM_LEN(out,1) = nout

	if (ndim == 1) {
	    # For one dimensional images do the replication directly.

	    buf1 = imgl1s (in)
	    buf2 = impl1s (out)
	    ptrin = buf1
	    ptrout = buf2
	    do i = 1, nin {
		do j = 1, blkfac[1] {
		    Mems[ptrout] = Mems[ptrin]
		    ptrout = ptrout + 1
		}
		ptrin = ptrin + 1
	    }

	} else {
	    # For higher dimensional images use line access routines.

	    do i = 2, ndim
	        IM_LEN(out,i) = IM_LEN(in,i) * blkfac[i]

	    # Allocate memory.
	    call salloc (buf, nout, TY_SHORT)
	    call salloc (v1, IM_MAXDIM, TY_LONG)
	    call salloc (v2, IM_MAXDIM, TY_LONG)
	    call salloc (v3, IM_MAXDIM, TY_LONG)

	    # Initialize the input line vector and the output section vectors.
	    call amovkl (long(1), Meml[v1], IM_MAXDIM)
	    call amovkl (long(1), Meml[v2], IM_MAXDIM)
	    call amovkl (long(1), Meml[v3], IM_MAXDIM)

	    # For each output line compute a block replicated line from the
	    # input image.  If the line replication factor is greater than
	    # 1 then simply repeat the input line.  This algorithm is
	    # sequential in both the input and output image though the
	    # input image will be recycled for each repetition of higher
	    # dimensions.

	    while (impnls (out, buf2, Meml[v2]) != EOF) {
		# Get the input vector corresponding to the output line.
		do i = 2, ndim
		    Meml[v1+i-1] = (Meml[v3+i-1] - 1) / blkfac[i] + 1
		i = imgnls (in, buf1, Meml[v1])

		# Block replicate the columns.
		if (blkfac[1] == 1)
		    buf3 = buf1
		else {
	            ptrin = buf1
	            ptrout = buf
	            do i = 1, nin {
		        do j = 1, blkfac[1] {
		            Mems[ptrout] = Mems[ptrin]
		            ptrout = ptrout + 1
		        }
		        ptrin = ptrin + 1
	            }
		    buf3 = buf
		}

		# Copy the input line to the output line.
		call amovs (Mems[buf3], Mems[buf2], nout)

		# Repeat for each repetition of the input line.
		for (i=2;  i <= blkfac[2];  i=i+1) {
	            j = impnls (out, buf2, Meml[v2])
		    call amovs (Mems[buf3], Mems[buf2], nout)
		}

		call amovl (Meml[v2], Meml[v3], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end

