      SUBROUTINE TRANS(Y,A,X)
      DIMENSION  Y(10), A(10), X(10)      
	COMMON /NLCPAR/XC(10), N, FIXSEP
	LOGICAL FIXSEP
C----- TRANSOFRMATION FOR GAUSSIAN LINES
C
C----- 'N' GAUSSIAN LINES
C
	Y(1)=EXP(-0.5*((X(1)-XC(1)-A(2))/A(1))**2)
	DO 1000 I=2,N
		IF(FIXSEP) THEN
			DELTA=A(2)
		ELSE
			DELTA=A(2*I)
		ENDIF
		Y(1)=Y(1)+ABS(A(2*I-1)*EXP(-0.5*((X(1)-XC(I)-DELTA)/
     *                        A(1))**2))
1000	CONTINUE
C            
      RETURN      
      END      
