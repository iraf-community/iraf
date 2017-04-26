C--------------------------------------------------------------------------
        subroutine ftgkns(iunit,keywrd,nstart,nmax,strval,nfound,
     &                    status)

C       read an array of character string values from  header records
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name
C       nstart  i  starting sequence number (usually 1)
C       nmax    i  number of keywords to read
C       OUTPUT PARAMETERS:
C       strval  i  array of output keyword values
C       nfound  i  number of keywords found
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,strval(*)
        integer iunit,nstart,nmax,nfound,status,tstat
        integer nkeys,mkeys,i,ival,nend,namlen,indval,ibuff
        character inname*8,keynam*8
        character*80 value,comm

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        if (status .gt. 0)return

C       get the number of the data buffer used for this unit
        ibuff=bufnum(iunit)

C       for efficiency, we want to search just once through the header
C       for all the keywords which match the root.

        nfound=0
        nend=nstart+nmax-1
        inname=keywrd
        call ftupch(inname)

C       find the length of the root name
        namlen=0
        do 5 i=8,1,-1
                if (inname(i:i) .ne. ' ')then
                        namlen=i
                        go to 6
                end if
5       continue
6       if (namlen .eq. 0)return

C       get the number of keywords in the header
        call ftghsp(iunit,nkeys,mkeys,status)

        do 10 i=3,nkeys
                call ftgrec(iunit,i,value,status)
                if (status .gt. 0)return
                keynam=value(1:8)
                if (keynam(1:namlen) .eq. inname(1:namlen))then

C                   try to interpret the remainder of the name as an integer
                    tstat=status
                    call ftc2ii(keynam(namlen+1:8),ival,status)
                    if (status .le. 0)then
                      if (ival .le. nend .and. ival .ge. nstart)then

C                       OK, this looks like a valid keyword; Reset the
C                       next-header-keyword pointer by one record, then
C                       call ftgkys to read it. (This does  support
C                       long continued string values)

                        nxthdr(ibuff)=nxthdr(ibuff)-80
                        indval=ival-nstart+1
                        call ftgkys(iunit,keynam,strval(indval),
     &                              comm,status)

                        nfound=max(nfound,indval)                       
                      end if
                    else
                        if (status .eq. 407)then
                                status=tstat
                        else
                                return
                        end if
                    end if
                end if
10      continue
        end
