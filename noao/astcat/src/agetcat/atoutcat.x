

# AT_GPPARS -- Update the AGETCAT task algorithm parameter sets.

procedure at_gppars (at)

pointer at                      #I the pointer to the main astrom structure

begin
        # Update the region definition parameters.
        call at_prcpset ("aregpars", at)

        # Update the catalog filtering parameters.
        call at_pfspset ("afiltpars", at)
end


# AT_FPPARS -- Update the AFILTCAT task algorithm parameter sets.

procedure at_fppars (at)

pointer at                      #I the pointer to the main astrom structure

begin
        # Update the catalog filtering parameters.
        call at_pfspset ("afiltpars", at)
end


# AT_GIPPARS -- Update the AGETIM task algorithm parameter sets.

procedure at_gippars (at)

pointer at                      #I the pointer to the main astrom structure

begin
        # Update the region definition parameters.
        call at_prcpset ("aregpars", at)

        # Update the default wcs parameters.
        #call at_pwcpset ("awcspars", at)

        # Update the default image data parameters.
        #call at_pimpset ("aimpars", at)
end


# AT_HPPARS -- Update the AHEDIT task algorithm parameter sets.

procedure at_hppars (at)

pointer at                      #I the pointer to the main astrom structure

begin
        # Update the default wcs parameters.
        call at_pwcpset ("awcspars", at)

        # Update the default image data parameters.
        call at_pimpset ("aimpars", at)
end


# AT_IPPARS -- Update the AIMFIND task algorithm parameter sets.

procedure at_ippars (at)

pointer at                      #I the pointer to the main astrom structure

begin
        # Update the catalog filtering parameters.
        call at_pfspset ("afiltpars", at)
end
