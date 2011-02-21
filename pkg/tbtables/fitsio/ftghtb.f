C----------------------------------------------------------------------
        subroutine ftghtb(iunit,maxfld,ncols,nrows,nfield,ttype,
     &                    tbcol,tform,tunit,extnam,status)

C       read required standard header keywords from an ASCII table extension 
C
C       iunit   i  Fortran i/o unit number
C       maxfld  i  maximum no. of fields to read; dimension of ttype
C       OUTPUT PARAMETERS:
C       ncols   i  number of columns in the table
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array)
C       tbcol   i  beginning column of each field (array)
C       tform   c  Fortran-77 format of each field (array)
C       tunit   c  units of each field (array)
C       extnam  c  name of table (optional)
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,maxfld,ncols,nrows,nfield,status,tbcol(*)
        integer i,nfind,maxf,tstat
        character*(*) ttype(*),tform(*),tunit(*),extnam
        character comm*72

        call ftgttb(iunit,ncols,nrows,nfield,status)
        if (status .gt. 0)return

        if (maxfld .le. 0)then
                maxf=nfield
        else
                maxf=min(maxfld,nfield)
        end if

C       initialize optional keywords
        do 10 i=1,maxf
                ttype(i)=' '
                tunit(i)=' '
10      continue

        call ftgkns(iunit,'TTYPE',1,maxf,ttype,nfind,status)
        call ftgkns(iunit,'TUNIT',1,maxf,tunit,nfind,status)

        if (status .gt. 0)return

        call ftgknj(iunit,'TBCOL',1,maxf,tbcol,nfind,status)
        if (status .gt. 0 .or. nfind .ne. maxf)then
C               couldn't find the required TBCOL keywords
                status=231
        call ftpmsg('Required TBCOL keyword(s) not found in ASCII'//
     &  ' table header (FTGHTB).')
                return
        end if

        call ftgkns(iunit,'TFORM',1,maxf,tform,nfind,status)
        if (status .gt. 0 .or. nfind .ne. maxf)then
C               couldn't find the required TFORM keywords
                status=232
        call ftpmsg('Required TFORM keyword(s) not found in ASCII'//
     &  ' table header (FTGHTB).')
                return
        end if

        extnam=' '
        tstat=status
        call ftgkys(iunit,'EXTNAME',extnam,comm,status)
C       this keyword is not required, so ignore 'keyword not found' status
        if (status .eq. 202)status=tstat
        end
