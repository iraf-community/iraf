	SUBROUTINE LUBKSD (A, N, NP, INDX, B)

C Double-precision version of LUBKSB.
C
C Solves a matrix equation AX = B.  Before using this routine you must
C call ludcmd to decompose the matrix A (in-place) into lower and upper
C triangular portions.  The vector B is input to this routine, and the
C answer X is returned in B.
C
C	double	a(np,np)	i: matrix returned by ludcmp
C	int	n		i: logical size of a is n x n
C	int	np		i: space allocated for a is np x np
C	int	indx(n)		i: index returned by ludcmp
C	double	b(n)		io: input b, output x in equation ax = b
C
C This routine simply calls the LU decomposition routine provided by LAPACK.

	INTEGER N, NP
	DOUBLE PRECISION A(NP,NP)
	INTEGER INDX(N)
	DOUBLE PRECISION B(N)

	INTEGER STATUS

	CALL DGETRS('N', N, 1, A, NP, INDX, B, N, STATUS)
	RETURN
	END
