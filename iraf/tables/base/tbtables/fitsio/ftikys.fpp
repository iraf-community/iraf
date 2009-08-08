C--------------------------------------------------------------------------
        subroutine ftikys(ounit,keywrd,strval,comm,status)

C       insert a string keyword into the header at the current position
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       strval  c  keyword value 
C       comm    c  keyword comment 
C       OUTPUT PARAMETERS
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, March 1993
C       Modifed 9/94 to call FTPKLS, supporting the OGIP long string convention

        character*(*) keywrd,comm,strval
        integer ounit,status

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C-------END OF COMMON BLOCK DEFINITIONS:------- -----------------------------

        integer lenval,length,i,nspace,ibuff,nexthd,endhd,nkeys,keypos  

        if (status .gt. 0)return

C       find how many keywords are required to write the string, in case it
C       cannot fit onto one keyword and has to be continued on multiple lines.

        lenval=len(strval)
        length=0
        do 10 i=lenval,1,-1
                if (strval(i:i) .ne. ' ')then
                        length=i
                        go to 20
                end if
10      continue
20      nspace=max(1,(length-2)/67+1)

C       save current pointer values
        ibuff=bufnum(ounit)
        endhd=hdend(ibuff)
        nexthd=nxthdr(ibuff)

C       insert enough spaces in the header at the current location
        call ftghps(ounit,nkeys,keypos,status)

        do 30 i=1,nspace
            call ftirec(ounit,keypos,' ',status)
30      continue

C       temporarily reset position of the end of header to force keyword
C       to be written at the current header position.
        hdend(ibuff)=nexthd

C       write the keyword (supporting the OGIP long string convention)
        call ftpkls(ounit,keywrd,strval,comm,status)

C       reset the next keyword pointer to follow the inserted keyword
        nxthdr(ibuff)=nexthd+80*nspace

C       reset the end-of-header pointer to its real location
        hdend(ibuff)=endhd+80*nspace
        end
