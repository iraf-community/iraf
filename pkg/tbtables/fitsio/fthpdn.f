C--------------------------------------------------------------------------
        subroutine fthpdn(ounit,nbytes,status)

C       shift the binary table heap down by nbyte bytes 

C       ounit   i  fortran output unit number
C       nbytes  i  number of bytes by which to move the heap
C       status  i  returned error status (0=ok)

        integer ounit,nbytes,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 20)
        parameter (nf = 3000)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
        integer tfield,tstart,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tstart(nb),tbcol(nf),rowlen(nb),
     &  tdtype(nf),trept(nf),tscale(nf),tzero(nf),tnull(nf),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*1 buff(5760)
        common/ftheap/buff
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer i,ibuff,ntodo,jpoint,nchar,tstat

        if (status .gt. 0)return

C       get the number of the data buffer used for this unit
        ibuff=bufnum(ounit)

        if (scount(ibuff) .gt. 0)then
            ntodo=scount(ibuff)

C           set pointer to the end of the heap
            jpoint=dtstrt(ibuff)+theap(ibuff)+scount(ibuff)

10          nchar=min(ntodo,5760)
            jpoint=jpoint-nchar

C           move to the read start position
            call ftmbyt(ounit,jpoint,.false.,status)

C           read the heap
            call ftgcbf(ounit,0,nchar,buff,status)

C           move forward to the write start postion
            call ftmbyt(ounit,jpoint+nbytes,.true.,status)

C           write the heap
            call ftpcbf(ounit,0,nchar,buff,status)

C           check for error 
            if (status .gt. 0)then
               call ftpmsg('Error while moving heap down (FTDNHP)')
               return
            end if

C           check for more data in the heap
            ntodo=ntodo-nchar
            if (ntodo .gt. 0)go to 10

C           now overwrite the old fill data with zeros
            do 20 i=1,5760
                buff(i)=char(0)
20          continue

            jpoint=dtstrt(ibuff)+theap(ibuff)
            call ftmbyt(ounit,jpoint,.false.,status)

            ntodo=nbytes
30          nchar=min(ntodo,5760)
            call ftpcbf(ounit,0,nchar,buff,status)
            ntodo=ntodo-nchar
            if (ntodo .gt. 0)go to 30
        end if

C       update the heap starting address
        theap(ibuff)=theap(ibuff)+nbytes

C       try updating the keyword value, if it exists
        tstat=status
        call ftmkyj(ounit,'THEAP',theap(ibuff),'&',status)
        if (status .eq. 202)status=tstat
        end
