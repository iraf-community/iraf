C--------------------------------------------------------------------------
        subroutine ftkshf(iunit,colmin,colmax,incre,status)

C       shift the index value on any existing column keywords
C       This routine will modify the name of any keyword that begins with 'T'
C       and has an index number in the range COLMIN - COLMAX, inclusive.

C       if incre is positive, then the index values will be incremented.
C       if incre is negative, then the kewords with index = COLMIN
C       will be deleted and the index of higher numbered keywords will
C       be decremented.

C       iunit   i  Fortran I/O unit number
C       colmin  i  starting column number to be incremented
C       colmax  i  maximum column number to be increment
C       incre   i  amount by which the index value should be shifted
C       status  i  returned error status (0=ok)

        integer iunit,colmin,colmax,incre,status

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,typhdu,tflds,nkeys,nmore,nrec,ival,tstat,i1
        character rec*80,newkey*8,q*4

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

C       test that the CHDU is an ASCII table or BINTABLE
        typhdu=hdutyp(ibuff)
        if (typhdu .ne. 1 .and. typhdu .ne. 2)then
                status=235
                call ftpmsg('Can only operate on TABLE or '//
     &          'BINTABLE extension (FTKSHF)')
                return
        end if

C       test column number limits
        tflds=tfield(ibuff)
        if (colmin .lt. 1 .or. colmax .lt. 1)then
             status=302
             return
        else if (colmin .gt. colmax .or. colmin .gt. tflds)then
             return
        end if

C       get the number of keywords in the header
        call ftghsp(iunit,nkeys,nmore,status)

C       go thru header starting with the 9th keyword looking for 'TxxxxNNN'

        nrec=9     
100     call ftgrec(iunit,nrec,rec,status)

        if (rec(1:1) .eq. 'T')then
            q=rec(2:5)
            i1=6

C           search list of 5-character 'official' indexed keywords
            if ( q .eq. 'BCOL' .or. q .eq. 'FORM' .or. q .eq. 'TYPE'
     &      .or. q .eq. 'UNIT' .or. q .eq. 'NULL' .or. q .eq. 'SCAL'
     &      .or. q .eq. 'ZERO' .or. q .eq. 'DISP')go to 20

C           search list of 5-character 'local' indexed keywords
            if ( q .eq. 'LMIN' .or. q .eq. 'LMAX' .or. q .eq. 'DMIN'
     &      .or. q .eq. 'DMAX' .or. q .eq. 'CTYP' .or. q .eq. 'CRPX' 
     &      .or. q .eq. 'CRVL' .or. q .eq. 'CDLT' .or. q .eq. 'CROT'
     &      .or. q .eq. 'CUNI')go to 20

            q=rec(1:4)
            i1=5
C           search list of 4-character 'official' indexed keywords
            if (q .eq. 'TDIM')go to 20
                 
C           no match so go on to next keyword
            go to 90

20          continue
C           try reading the index number suffix 
            tstat=0
            call ftc2ii(rec(i1:8),ival,tstat)
            if (tstat .eq. 0 .and. ival .ge. colmin .and.
     &          ival .le. colmax)then
                if (incre .le. 0 .and. ival .eq. colmin)then
C                   delete keyword related to this column
                    call ftdrec(iunit,nrec,status)
                    nkeys=nkeys-1
                    nrec=nrec-1
                else
                    ival=ival+incre
                    i1=i1-1
                    call ftkeyn(rec(1:i1),ival,newkey,status)
                    rec(1:8)=newkey
C                   modify the index number of this keyword
                    call ftmrec(iunit,nrec,rec,status)
                end if
            end if
        end if

90      nrec=nrec+1
        if (nrec .le. nkeys)go to 100
        end
