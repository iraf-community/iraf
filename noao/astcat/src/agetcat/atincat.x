# AT_GAPARS -- Read in the algorithm parameters for the AGETCAT task.

procedure at_gapars (at)

pointer at                      #I the pointer to the main astrom structure

begin
        # Initialize the region parameters.
        call at_grcpset ("aregpars", at)

        # Initialize the catalog filter / selection parameters.
        call at_gfspset ("afiltpars", at)
end


# AT_FAPARS -- Read in the algorithm parameters for the AFILTCAT task.

procedure at_fapars (at)

pointer at                      #I the pointer to the main astrom structure

begin
        # Initialize the catalog filter / selection parameters.
        call at_gfspset ("afiltpars", at)
end


# AT_GIAPARS -- Read in the algorithm parameters for the AGETIM task.

procedure at_giapars (at)

pointer at                      #I the pointer to the main astrom structure

begin
        # Initialize the region parameters.
        call at_grcpset ("aregpars", at)

        # Initialize the default wcs parameters.
        #call at_gwcpset ("awcspars", at)

        # Initialize the default image data parameters.
        #call at_gimpset ("aimpars", at)
end


# AT_HAPARS -- Read in the algorithm parameters for the AHEDIT task.

procedure at_hapars (at)

pointer at                      #I the pointer to the main astrom structure

begin
        # Initialize the default wcs parameters.
        call at_gwcpset ("awcspars", at)

        # Initialize the default image data parameters.
        call at_gimpset ("aimpars", at)
end


# AT_IAPARS -- Read in the algorithm parameters for the AIMFIND task.

procedure at_iapars (at)

pointer at                      #I the pointer to the main astrom structure

begin
        # Initialize the catalog filter / selection parameters.
        call at_gfspset ("afiltpars", at)
end
