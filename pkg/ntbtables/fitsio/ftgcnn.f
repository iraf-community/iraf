C--------------------------------------------------------------------------
        subroutine ftgcnn(iunit,casesn,templt,colnam,colnum,status)

C       determine the column name and number corresponding to an input 
C       column name template string.  The template may contain the * and ?
C       wildcards.  Status = 237 is returned if match is not unique.
C       One may call this routine again with input status=237  to
C       get the next match.  

C       iunit   i  Fortran i/o unit number
C       casesn  l  true if an exact case match of the names is required
C       templt  c  templt for column name
C       colnam  c  name of (first) column that matchs the template
C       colnum  i  number of the column (first column = 1)
C                       (a value of 0 is returned if the column is not found)
C       status  i  returned error status

C       written by Wm Pence, HEASARC/GSFC, December 1994

        integer iunit,colnum,status
        character*(*) templt,colnam
        logical casesn

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
        integer colpnt,untpnt
        common/ftname/colpnt,untpnt
C       END OF COMMON BLOCK DEFINITIONS------------------------------------

        integer ibuff,i,nfound,tstat,ival
        logical match,exact,founde,foundw,unique
        character*80 errmsg
        character*68 tname(999)
        save tname

        ibuff=bufnum(iunit)

C       load the common block with names, if not already defined
        if (colpnt .eq. -999 .or. iunit .ne. untpnt)then
            do 10 i=1,tfield(ibuff)
                tname(i)=' '
10          continue
            call ftgkns(iunit,'TTYPE',1,nf,tname,nfound,status)
            if (status .gt. 0)return
            untpnt=iunit
            colpnt=1
        end if

        if (status .le. 0)then
            tstat=0
            colpnt=1
        else if (status .eq. 237)then
C           search for next non-unique match, starting from the previous match
            tstat=237
            status=0
        else
            return
        end if

        colnam=' '
        colnum=0


C       set the 'found exact' and 'found wildcard' flags to false
        founde=.false.
        foundw=.false.
       
        do 100 i=colpnt,tfield(ibuff)
C               test for match between template and column name
                call ftcmps(templt,tname(i),casesn,match,exact)

                if (match)then
                    if (founde .and. exact)then
C                       warning: this is the second exact match we've found
C                       reset pointer to first match so next search starts there
                        colpnt=colnum+1
                        status=237
                        return
                    else if (founde)then
C                       already found exact match so ignore this non-exact match
                    else if (exact)then
C                       this is the first exact match we have found, so save it.
                        colnam=tname(i)
                        colnum=i
                        founde=.true.
                    else if (foundw)then
C                       we have already found a wild card match, so not unique
C                       continue searching for other matches
                        unique=.false.
                    else
C                       this is the first wild card match we've found. save it
                        colnam=tname(i)
                        colnum=i
                        foundw=.true.
                        unique=.true.
                    end if
                end if
100     continue        

C       OK, we've checked all the names now see if we got any matches
        if (founde)then
C           we did find 1 exact match
            if (tstat .eq. 237)status=237
        else if (foundw)then
C           we found one or more wildcard matches
C           report error if not unique
            if (.not. unique .or. tstat .eq. 237)status=237
        else
C           didn't find a match; check if template is a simple positive integer
            call ftc2ii(templt,ival,tstat)
            if (tstat .eq. 0 .and. ival .le. tfield(ibuff) 
     &          .and. ival .gt. 0)then
                colnum=ival
                colnam=tname(ival)
            else
                status=219
                if (tstat .ne. 237)then
                  errmsg='FTGCNN: Could not find column: '//templt
                  call ftpmsg(errmsg)
                end if
            end if
        end if

C       reset pointer so next search starts here if input status=237
        colpnt=colnum+1
        end
