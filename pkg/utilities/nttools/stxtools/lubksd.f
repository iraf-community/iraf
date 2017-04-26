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
C 1988 Oct 28  From Numerical Recipes.
C 1992 Sep 10  Rename from LUBKSB and convert to double precision.

	INTEGER N, NP
	DOUBLE PRECISION A(NP,NP)
	INTEGER INDX(N)
	DOUBLE PRECISION B(N)

	DOUBLE PRECISION SUM
	INTEGER II, LL, I, J

	II = 0
	DO 20 I = 1, N
	    LL = INDX(I)
	    SUM = B(LL)
	    B(LL) = B(I)
	    IF (II .NE. 0) THEN
		DO 10 J = II, I-1
		    SUM = SUM - A(I,J) * B(J)
 10		CONTINUE
	    ELSE IF (SUM .NE. 0.) THEN
		II = I
	    ENDIF
	    B(I) = SUM
 20	CONTINUE

	DO 40 I = N, 1, -1
	    SUM = B(I)
	    IF (I .LT. N) THEN
		DO 30 J = I+1, N
		    SUM = SUM - A(I,J) * B(J)
 30		CONTINUE
	    ENDIF
	    B(I) = SUM / A(I,I)
 40	CONTINUE

	RETURN
	END
