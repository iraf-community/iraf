C----------------------------------------------------------------------
        subroutine ftghbn(iunit,maxfld,nrows,nfield,ttype,tform,
     &                    tunit,extnam,pcount,status)

C       read required standard header keywords from a binary table extension 
C
C       iunit   i  Fortran i/o unit number
C       maxfld  i  maximum no. of fields to read; size of ttype array
C       OUTPUT PARAMETERS:
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array)
C       tform   c  format of each field (array)
C       tunit   c  units of each field (array)
C       extnam  c  name of table (optional)
C       pcount  i  size of special data area following the table (usually = 0)
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,maxfld,ncols,nrows,nfield,pcount,status,tstat
        integer maxf,i,nfind
        character*(*) ttype(*),tform(*),tunit(*),extnam
        character comm*72

C       check that this is a valid binary table and get parameters
        call ftgtbn(iunit,ncols,nrows,pcount,nfield,status)
        if (status .gt. 0)return

        if (maxfld .lt. 0)then
              maxf=nfield
        else if (maxfld .eq. 0)then
              go to 20
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

        call ftgkns(iunit,'TFORM',1,maxf,tform,nfind,status)
        if (status .gt. 0 .or. nfind .ne. maxf)then
                status=232
                return
        end if

20      extnam=' '
        tstat=status
        call ftgkys(iunit,'EXTNAME',extnam,comm,status)
C       this keyword is not required, so ignore status
        if (status .eq. 202)status =tstat
        end
