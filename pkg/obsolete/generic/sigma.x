# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


# SIGMA -- Compute sigma line from image lines with rejection.

procedure sigmas (data, nimages, mean, sigma, npts)

pointer	data[nimages]		# Data vectors
int	nimages			# Number of data vectors
real	mean[npts]		# Mean vector
real	sigma[npts]		# Sigma vector (returned)
int	npts			# Number of points in each vector

real	val, sig, pixval
int	i, j, n, n1

begin
	n = nimages - 1
	do i = 1, npts {
	    val = mean[i]
	    sig = 0.
	    n1 = n
	    do j = 1, nimages {
		pixval = Mems[data[j]+i-1]
		if (IS_INDEFS (pixval))
		    n1 = n1 - 1
		else
		    sig = sig + (pixval - val) ** 2
	    }
	    if (n1 > 0)
	        sigma[i] = sqrt (sig / n1)
	    else
	        sigma[i] = 0.
	}
end


# WTSIGMA -- Compute scaled and weighted sigma line from image lines with
# rejection.

procedure wtsigmas (data, scales, zeros, wts, nimages, mean, sigma, npts)

pointer	data[nimages]		# Data vectors
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data vectors
real	mean[npts]		# Mean vector
real	sigma[npts]		# Sigma vector (returned)
real	val, sig, pixval
int	npts			# Number of points in each vector

int	i, j, n
real	sumwts

begin
	do i = 1, npts {
	    val = mean[i]
	    n = 0
	    sig = 0.
	    sumwts = 0.
	    do j = 1, nimages {
		pixval = Mems[data[j]+i-1]
		if (!IS_INDEFS (pixval)) {
		    n = n + 1
		    sig = sig + wts[j]*(pixval/scales[j]-zeros[j]-val) ** 2
		    sumwts = sumwts + wts[j]
		}
	    }
	    if (n > 1)
	        sigma[i] = sqrt (sig / sumwts * n / (n - 1))
	    else
	        sigma[i] = 0.
	}
end

# SIGMA -- Compute sigma line from image lines with rejection.

procedure sigmai (data, nimages, mean, sigma, npts)

pointer	data[nimages]		# Data vectors
int	nimages			# Number of data vectors
real	mean[npts]		# Mean vector
real	sigma[npts]		# Sigma vector (returned)
int	npts			# Number of points in each vector

real	val, sig, pixval
int	i, j, n, n1

begin
	n = nimages - 1
	do i = 1, npts {
	    val = mean[i]
	    sig = 0.
	    n1 = n
	    do j = 1, nimages {
		pixval = Memi[data[j]+i-1]
		if (IS_INDEFI (pixval))
		    n1 = n1 - 1
		else
		    sig = sig + (pixval - val) ** 2
	    }
	    if (n1 > 0)
	        sigma[i] = sqrt (sig / n1)
	    else
	        sigma[i] = 0.
	}
end


# WTSIGMA -- Compute scaled and weighted sigma line from image lines with
# rejection.

procedure wtsigmai (data, scales, zeros, wts, nimages, mean, sigma, npts)

pointer	data[nimages]		# Data vectors
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data vectors
real	mean[npts]		# Mean vector
real	sigma[npts]		# Sigma vector (returned)
real	val, sig, pixval
int	npts			# Number of points in each vector

int	i, j, n
real	sumwts

begin
	do i = 1, npts {
	    val = mean[i]
	    n = 0
	    sig = 0.
	    sumwts = 0.
	    do j = 1, nimages {
		pixval = Memi[data[j]+i-1]
		if (!IS_INDEFI (pixval)) {
		    n = n + 1
		    sig = sig + wts[j]*(pixval/scales[j]-zeros[j]-val) ** 2
		    sumwts = sumwts + wts[j]
		}
	    }
	    if (n > 1)
	        sigma[i] = sqrt (sig / sumwts * n / (n - 1))
	    else
	        sigma[i] = 0.
	}
end

# SIGMA -- Compute sigma line from image lines with rejection.

procedure sigmal (data, nimages, mean, sigma, npts)

pointer	data[nimages]		# Data vectors
int	nimages			# Number of data vectors
real	mean[npts]		# Mean vector
real	sigma[npts]		# Sigma vector (returned)
int	npts			# Number of points in each vector

real	val, sig, pixval
int	i, j, n, n1

begin
	n = nimages - 1
	do i = 1, npts {
	    val = mean[i]
	    sig = 0.
	    n1 = n
	    do j = 1, nimages {
		pixval = Meml[data[j]+i-1]
		if (IS_INDEFL (pixval))
		    n1 = n1 - 1
		else
		    sig = sig + (pixval - val) ** 2
	    }
	    if (n1 > 0)
	        sigma[i] = sqrt (sig / n1)
	    else
	        sigma[i] = 0.
	}
end


# WTSIGMA -- Compute scaled and weighted sigma line from image lines with
# rejection.

procedure wtsigmal (data, scales, zeros, wts, nimages, mean, sigma, npts)

pointer	data[nimages]		# Data vectors
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data vectors
real	mean[npts]		# Mean vector
real	sigma[npts]		# Sigma vector (returned)
real	val, sig, pixval
int	npts			# Number of points in each vector

int	i, j, n
real	sumwts

begin
	do i = 1, npts {
	    val = mean[i]
	    n = 0
	    sig = 0.
	    sumwts = 0.
	    do j = 1, nimages {
		pixval = Meml[data[j]+i-1]
		if (!IS_INDEFL (pixval)) {
		    n = n + 1
		    sig = sig + wts[j]*(pixval/scales[j]-zeros[j]-val) ** 2
		    sumwts = sumwts + wts[j]
		}
	    }
	    if (n > 1)
	        sigma[i] = sqrt (sig / sumwts * n / (n - 1))
	    else
	        sigma[i] = 0.
	}
end

# SIGMA -- Compute sigma line from image lines with rejection.

procedure sigmar (data, nimages, mean, sigma, npts)

pointer	data[nimages]		# Data vectors
int	nimages			# Number of data vectors
real	mean[npts]		# Mean vector
real	sigma[npts]		# Sigma vector (returned)
int	npts			# Number of points in each vector

real	val, sig, pixval
int	i, j, n, n1

begin
	n = nimages - 1
	do i = 1, npts {
	    val = mean[i]
	    sig = 0.
	    n1 = n
	    do j = 1, nimages {
		pixval = Memr[data[j]+i-1]
		if (IS_INDEFR (pixval))
		    n1 = n1 - 1
		else
		    sig = sig + (pixval - val) ** 2
	    }
	    if (n1 > 0)
	        sigma[i] = sqrt (sig / n1)
	    else
	        sigma[i] = 0.
	}
end


# WTSIGMA -- Compute scaled and weighted sigma line from image lines with
# rejection.

procedure wtsigmar (data, scales, zeros, wts, nimages, mean, sigma, npts)

pointer	data[nimages]		# Data vectors
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data vectors
real	mean[npts]		# Mean vector
real	sigma[npts]		# Sigma vector (returned)
real	val, sig, pixval
int	npts			# Number of points in each vector

int	i, j, n
real	sumwts

begin
	do i = 1, npts {
	    val = mean[i]
	    n = 0
	    sig = 0.
	    sumwts = 0.
	    do j = 1, nimages {
		pixval = Memr[data[j]+i-1]
		if (!IS_INDEFR (pixval)) {
		    n = n + 1
		    sig = sig + wts[j]*(pixval/scales[j]-zeros[j]-val) ** 2
		    sumwts = sumwts + wts[j]
		}
	    }
	    if (n > 1)
	        sigma[i] = sqrt (sig / sumwts * n / (n - 1))
	    else
	        sigma[i] = 0.
	}
end

# SIGMA -- Compute sigma line from image lines with rejection.

procedure sigmad (data, nimages, mean, sigma, npts)

pointer	data[nimages]		# Data vectors
int	nimages			# Number of data vectors
double	mean[npts]		# Mean vector
double	sigma[npts]		# Sigma vector (returned)
int	npts			# Number of points in each vector

double	val, sig, pixval
int	i, j, n, n1

begin
	n = nimages - 1
	do i = 1, npts {
	    val = mean[i]
	    sig = 0.
	    n1 = n
	    do j = 1, nimages {
		pixval = Memd[data[j]+i-1]
		if (IS_INDEFD (pixval))
		    n1 = n1 - 1
		else
		    sig = sig + (pixval - val) ** 2
	    }
	    if (n1 > 0)
	        sigma[i] = sqrt (sig / n1)
	    else
	        sigma[i] = 0.
	}
end


# WTSIGMA -- Compute scaled and weighted sigma line from image lines with
# rejection.

procedure wtsigmad (data, scales, zeros, wts, nimages, mean, sigma, npts)

pointer	data[nimages]		# Data vectors
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data vectors
double	mean[npts]		# Mean vector
double	sigma[npts]		# Sigma vector (returned)
double	val, sig, pixval
int	npts			# Number of points in each vector

int	i, j, n
real	sumwts

begin
	do i = 1, npts {
	    val = mean[i]
	    n = 0
	    sig = 0.
	    sumwts = 0.
	    do j = 1, nimages {
		pixval = Memd[data[j]+i-1]
		if (!IS_INDEFD (pixval)) {
		    n = n + 1
		    sig = sig + wts[j]*(pixval/scales[j]-zeros[j]-val) ** 2
		    sumwts = sumwts + wts[j]
		}
	    }
	    if (n > 1)
	        sigma[i] = sqrt (sig / sumwts * n / (n - 1))
	    else
	        sigma[i] = 0.
	}
end

# SIGMA -- Compute sigma line from image lines with rejection.

procedure sigmax (data, nimages, mean, sigma, npts)

pointer	data[nimages]		# Data vectors
int	nimages			# Number of data vectors
complex	mean[npts]		# Mean vector
complex	sigma[npts]		# Sigma vector (returned)
int	npts			# Number of points in each vector

complex	val, sig, pixval
int	i, j, n, n1

begin
	n = nimages - 1
	do i = 1, npts {
	    val = mean[i]
	    sig = 0.
	    n1 = n
	    do j = 1, nimages {
		pixval = Memx[data[j]+i-1]
		if (IS_INDEFX (pixval))
		    n1 = n1 - 1
		else
		    sig = sig + (pixval - val) ** 2
	    }
	    if (n1 > 0)
	        sigma[i] = sqrt (sig / n1)
	    else
	        sigma[i] = 0.
	}
end


# WTSIGMA -- Compute scaled and weighted sigma line from image lines with
# rejection.

procedure wtsigmax (data, scales, zeros, wts, nimages, mean, sigma, npts)

pointer	data[nimages]		# Data vectors
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data vectors
complex	mean[npts]		# Mean vector
complex	sigma[npts]		# Sigma vector (returned)
complex	val, sig, pixval
int	npts			# Number of points in each vector

int	i, j, n
real	sumwts

begin
	do i = 1, npts {
	    val = mean[i]
	    n = 0
	    sig = 0.
	    sumwts = 0.
	    do j = 1, nimages {
		pixval = Memx[data[j]+i-1]
		if (!IS_INDEFX (pixval)) {
		    n = n + 1
		    sig = sig + wts[j]*(pixval/scales[j]-zeros[j]-val) ** 2
		    sumwts = sumwts + wts[j]
		}
	    }
	    if (n > 1)
	        sigma[i] = sqrt (sig / sumwts * n / (n - 1))
	    else
	        sigma[i] = 0.
	}
end

