C----------------------------------------------------------------------
        subroutine ftfrcl(iunit,status)

C       free up space in the common blocks that contain descriptors to
C       the columns in the HDU that is being closed.  The various parameters
C       describing each table column (e.g., starting byte address, datatype,
C       tscale, tzero, etc.) are stored in 1-D arrays, and the tstart
C       parameter gives the starting element number in the arrays 
C       for each unit number.  If a table is closed, then all the
C       descriptors for that table columns must be overwritten by
C       shifting any descriptors that follow it in the 1-D arrays to the left.

C       iunit   i  fortran unit number
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC,May, 1995

        integer iunit,status

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
        character cnull*16, cform*8
        common/ft0003/cnull(nf),cform(nf)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,n2shft,i,j1,j2

C       ignore input status and flush columns regardless of input status value

        ibuff=bufnum(iunit)

        if (status .eq. -999)then
C           just initialize the descriptors as undefined
            tstart(ibuff)=-1
        else if (tstart(ibuff) .lt. 0)then
C           descriptors are already undefined; just return
        else if (tfield(ibuff) .eq. 0)then
C           table had no columns so just reset pointers as undefined
            tstart(ibuff)=-1
            dtstrt(ibuff)=-2000000000
        else
C           calc number of descriptors to be shifted over the recovered space
            n2shft=nxtfld-(tstart(ibuff)+tfield(ibuff))
     
            if (n2shft .gt. 0)then
                j1=tstart(ibuff)
                j2=j1+tfield(ibuff)
                do 10 i=1,n2shft
C                   shift the descriptors
                    j1=j1+1
                    j2=j2+1
                    tbcol(j1)=tbcol(j2)
                    tdtype(j1)=tdtype(j2)
                    trept(j1)=trept(j2)
                    tscale(j1)=tscale(j2)
                    tzero(j1)=tzero(j2)
                    tnull(j1)=tnull(j2)
                    cnull(j1)=cnull(j2)
                    cform(j1)=cform(j2)
10              continue
            end if

C           update pointer to next vacant column discriptor location
            nxtfld=nxtfld-tfield(ibuff)

C           update starting pointer for other opened files
            do 20 i=1,nb
                if (tstart(i) .gt. tstart(ibuff))then
                    tstart(i)=tstart(i)-tfield(ibuff)
                end if
20          continue

C           set pointers for this unit as undefined
            tstart(ibuff)=-1
            dtstrt(ibuff)=-2000000000
        end if
        end                
