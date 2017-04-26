# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

procedure srf_test()

char	temp[SZ_LINE]
real    z[20,30], x[20], y[30], s[6]
int	mm[20,30,2]
real	tx, ty
int	i, j, m, n, isize
real	xt, yt, dum

int	ifr, istp, irots, idrx, idry, idrz, iupper, iskirt, ncla, hskirt, ispval
real	theta, chi, clo, cinc
common  /srfip1/ ifr, istp, irots, idrx, idry, idrz, iupper, iskirt,
	  ncla, theta, hskirt, chi, clo, cinc, ispval

begin
      # Some initialization that was originally in data statements:
      tx = 0.4375
      ty = 0.9667
      m = 20
      n = 30
      s[1] = 4.0
      s[2] = 5.0
      s[3] = 3.0
      s[4] = 0.0
      s[5] = 0.0
      s[6] = 0.0

      # Define function values and store in z
      DO  I=1,M
	 X(I) =	-1.+FLOAT(I-1)/FLOAT(M-1)*2.
   
      DO  J=1,N
	 Y(J) =	-1.+FLOAT(J-1)/FLOAT(N-1)*2.
   
      DO  J=1,N {
	 DO  I=1,M
	    Z(I,J) = EXP(-2.*SQRT(X(I)**2+Y(J)**2))
      }

      # Initialize block data before changing parameters.
      call srfabd

      IFR = 0
      IDRZ = 1

      CALL GSELNT (0)
      call f77pak ("DEMONSTRATION PLOT FOR PWRZS", temp, SZ_LINE)
      CALL WTSTR (TX,TY,temp,2,0,0)

      CALL SRFACE (X,Y,Z,MM,M,M,N,S,0.)
#
# PUT PWRZS LABELS ON PICTURE
#
      ISIZE = 35
      call f77pak ("FRONT", temp, SZ_LINE)
      CALL PWRZS (0.,1.1,0.,temp,5,ISIZE,-1,3,0)
      call f77pak ("SIDE", temp, SZ_LINE)
      CALL PWRZS (1.1,0.,0.,temp,4,ISIZE,2,-1,0)
      call f77pak (" BACK BACK BACK BACK BACK", temp, SZ_LINE)
      CALL PWRZS (0.,-1.1,.2,temp,25,ISIZE,-1,3,0)
#
# RESTORE SRFACE PARAMETERS TO DEFAULT
#
      IFR = 1
      IDRZ = 0
end
