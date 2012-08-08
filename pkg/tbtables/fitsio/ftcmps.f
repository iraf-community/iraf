C--------------------------------------------------------------------------
        subroutine ftcmps(templt,string,casesn,match,exact)

C       compare the template to the string and test if they match.
C       The strings are limited to 68 characters or less (the max. length
C       of a FITS string keyword value.  This routine reports whether
C       the two strings match and whether the match is exact or
C       involves wildcards.

C       this algorithm is very similar to the way unix filename wildcards
C       work except that this first treats a wild card as a literal character
C       when looking for a match.  If there is no literal match, then
C       it interpretes it as a wild card.  So the template 'AB*DE'
C       is considered to be an exact rather than a wild card match to
C       the string 'AB*DE'.

C       templt    C input template (may includ % or * wild cards)
C       string    C input string to be compared to template
C       casesn    L should comparison be case sensitive?
C       match     L (output) does the template match the string?
C       exact     L (output) are the strings an exact match (true) or
C                            is it a wildcard match (false)

C       written by Wm Pence, HEASARC/GSFC, December 1994

        character*(*) templt,string
        logical casesn,match,exact
        character*68 temp,str
        integer tlen,slen,t1,s1

        tlen=len(templt)
        slen=len(string)
        tlen=min(tlen,68)
        slen=min(tlen,68)

        match=.false.
        exact=.true.
        temp=templt
        str=string
        if (.not. casesn)then
            call ftupch(temp)
            call ftupch(str)
        end if

C       check for exact match
        if (temp .eq. str)then
            match=.true.
            return
        else
C           the strings are not identical, any match cannot be exact
            exact=.false.
        end if

        t1=1
        s1=1
10      continue
        if (t1 .gt. tlen .or. s1 .gt. slen)then
C           completely scanned one or both strings, so it must be a match
            match=.true.
            return
        end if

C       see if the characters in the 2 strings are an exact match
        if (temp(t1:t1) .eq. str(s1:s1))then
            s1=s1+1
            t1=t1+1
        else
            exact=.false.
            if (temp(t1:t1) .eq. '?')then
C               The '?' wild card matches anything
                s1=s1+1
                t1=t1+1
            else if (temp(t1:t1) .eq. '*')then
C               get next character from template and look for it in the string
                t1=t1+1
                if (t1 .le. tlen)then
                    if (temp(t1:t1) .eq. ' ')then
C                       * is followed by a space, so a match is guaranteed
                        t1=tlen+1
                    else
20                      continue
                        if (temp(t1:t1) .eq. str(s1:s1))then
C                           found a matching character
                            t1=t1+1
                            s1=s1+1
                        else
C                           increment the string pointer and try again
                            s1=s1+1
                            if (s1 .le. slen)then
                                go to 20
                            else
C                               hit end of string and failed to find a match
                                return
                            end if
                        end if
                    end if
                end if
            else
C               match failed
                return
            end if
        end if
        go to 10
        end
