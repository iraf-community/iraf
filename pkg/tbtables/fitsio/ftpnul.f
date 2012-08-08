C--------------------------------------------------------------------------
        subroutine ftpnul(ounit,blank,status)

C       Primary Null value definition
C       Define the null value for an integer primary array.
C
C       ounit   i  Fortran I/O unit number
C       blank   i  the value to be use to signify undefined data
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,blank,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne,nf
        parameter (nb = 20)
        parameter (ne = 200)
        parameter (nf = 3000)
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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff,i,ngroup

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

C       if HDU structure is not defined then scan the header keywords
        if (dtstrt(ibuff) .lt. 0)call ftrdef(ounit,status)
        if (status .gt. 0)return

C       test for proper HDU type
        if (hdutyp(ibuff) .ne. 0)then
            status=233
            return
        end if
            
C       the primary array is actually interpreted as a binary table.  There
C       are two columns for each group: the first column contains the 
C       group parameters, if any, and the second column contains the
C       primary array of data. 

        ngroup=tfield(ibuff)/2
        do 10 i=1,ngroup
                tnull(i*2+tstart(ibuff))=blank
10      continue
        end
