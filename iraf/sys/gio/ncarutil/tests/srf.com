int	ifr, istp, irots, idrx, idry, idrz, iupper, iskirt, ncla, hskirt, ispval
real	theta, chi, clo, cinc
common  /srfip1/ ifr, istp, irots, idrx, idry, idrz, iupper, iskirt,
	  ncla, theta, hskirt, chi, clo, cinc, ispval
