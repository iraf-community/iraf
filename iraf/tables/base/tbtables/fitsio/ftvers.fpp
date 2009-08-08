C------------------------------------------------------------------------------
C   This software was prepared by High Energy Astrophysic Science Archive
C   Research Center (HEASARC) at the NASA Goddard Space Flight Center. Users
C   shall not, without prior written permission of the U.S. Government,
C   establish a claim to statutory copyright.  The Government and others acting
C   on its behalf, shall have a royalty-free, non-exclusive, irrevocable,
C   worldwide license for Government purposes to publish, distribute,
C   translate, copy, exhibit, and perform such material. 
C------------------------------------------------------------------------------
        subroutine ftvers(vernum)

C       Returns the current revision number of the FITSIO package.
C       The revision number will be incremented whenever any modifications,
C       bug fixes, or enhancements are made to the package

        real vernum
C       version 4.06   18 Aug   1995   ftdelt bug; ftpmsg saves latest errors
C       version 4.05    2 Aug   1995   another bug in ftfrcl in reseting tstart
C       version 4.04   12 Jul   1995   bug in ftfrcl in resetting tstart
C       version 4.03    3 Jul   1995   bug in restoring CHDU when moving to EOF
C       version 4.02   20 Jun   1995   modified checksum algorithm
C       version 4.01   30 May   1995   many changes
C       version 3.711  30 Jan   1995   ftgphx was cutting BSCALE to 20 chars
C       version 3.710  27 Jan   1995   fix ftgcnn, fitsmac; add ftirec, ftdrec
C       version 3.700  29 Dec   1994   public release
C       version 3.623   8 Nov   1994   ftgkys, ftgnst, checksum
C       version 3.622   7 Nov   1994   ftgclj R*8 alignment; I*2 overflow fti4i2
C       version 3.621   4 Nov   1994   fixed endhd position in ftgrec
C       version 3.62    2 Nov   1994   ftgcx[ijd] routines added
C       version 3.612  31 Oct   1994   restored previous FTIBLK algorithm
C       version 3.61   26 Oct   1994   ftirow and ftdrow to modify tables
C       version 3.6    18 Oct   1994   ftukyX, range checking, new EOF checks
C       version 3.512  20 Sep   1994   fixed writing header fill in FTWEND 
C       version 3.511  20 Sep   1994   removed '=' from CONTINUE on long strings
C       version 3.51   14 Sep   1994   long string convention and IEEE support
C       version 3.504  22 Aug   1994   fixed bug in ftcopy making files too big
C       version 3.503   8 Aug   1994   fixed bug in ftcopy making files too big
C       version 3.502  26 Jul   1994   explicitly write data fill bytes
C       version 3.501  19 Jul   1994   minor changes for FTOOLS release
C       version 3.500  29 Jun   1994   added error message stack
C       version 3.415  07 Jun   1994   fixed ftmahd and ftgrec
C       version 3.414  18 May   1994   modify ftmoff and ftpbyt for status 112
C       version 3.413  18 Mar   1994   Cray port added
C       version 3.412  01 Mar   1994   SUN internal read problem in ftgthd
C       version 3.411  25 Feb   1994   fixed 107 error when reading byte column
C       version 3.410  21 Jan   1994   bug fixes in Alpha VMS version
C       version 3.409  21 Dec   1993   long string bug; HP support
C       version 3.408  09 Nov   1993   Alpha VMS open; ftgthd -; 210 status
C       version 3.407  02 Nov   1993   initialize TABLEs with blanks; ftrdef
C       version 3.406  26 Oct   1993   ftgtdm bug - last not initialized
C                                      modified to read unknown extenstions
C       version 3.405  21 Oct   1993   ftpini bug with GROUP format files
C       version 3.404   7 Oct   1993   new TDIM subroutines, new error status
C       version 3.403   1 Sept  1993   initialize strlen in ftpkys
C       version 3.402  23 Aug   1993   bug in ftgcno
C       version 3.401  20 Aug   1993   minor change to ftpi1b
C       version 3.4  - 11 Aug   1993
C       version 3.31 -  2 Feb   1993
C       version 3.3  - 28 Oct   1992
C       version 3.21 -  8 July  1992
C       version 3.20 - 30 Mar   1992
C       version 3.10 -  4 Nov   1991
C       version 3.01 - 27 Sept  1991
C       version 3.00 - 12 Sept  1991
C       version 2.99 - 24 July  1991
C       version 2.0  -  1 May   1991
C       version 1.3  -  2 April 1991
C       version 1.22 - 22 March 1991
C       version 1.21 - 20 March 1991

        vernum=4.06
        end
