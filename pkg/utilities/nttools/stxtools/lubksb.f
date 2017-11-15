	SUBROUTINE LUBKSB (A, N, NP, INDX, B)

C Solves a matrix equation AX = B.  Before using this routine you must
C call ludcmp to decompose the matrix A (in-place) into lower and upper
C triangular portions.  The vector B is input to this routine, and the
C answer X is returned in B.
C
C	real	a(np,np)	i: matrix returned by ludcmp
C	int	n		i: logical size of a is n x n
C	int	np		i: space allocated for a is np x np
C	int	indx(n)		i: index returned by ludcmp
C	real	b(n)		io: input b, output x in equation ax = b
C
C This routine simply calls the LU decomposition routine provided by LAPACK.

	INTEGER N, NP
	REAL	A(NP,NP)
	INTEGER INDX(N)
	REAL	B(N)

	INTEGER STATUS

	CALL SGETRS('N', N, 1, A, NP, INDX, B, N, STATUS)
	RETURN
	END
