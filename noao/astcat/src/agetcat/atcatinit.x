
# AT_AGINIT -- Inititialize the AGETCAT task structure.

procedure at_aginit (at)

pointer at                      #O the pointer to the astrometry descriptor

begin
        # Initialize the astrometry structure.
        call at_ainit (at)

        # Initialize the i/o structure.
        call at_ioinit (at)

        # Initialize the region definition structure.
        call at_rcinit (at)

        # Initialize the filtering / selection structure.
        call at_fsinit (at)
end


# AT_AGFREE -- Free the AGETCAT task structure.

procedure at_agfree (at)

pointer at                      #U the pointer to the astrometry descriptor

begin
        # Free the filtering / selection structure.
        call at_fsfree (at)

        # Free the field center structure.
        call at_rcfree (at)

        # Free the i/o structure
        call at_iofree (at)

        # Free the astrometry structure.
        call at_afree (at)
end


# AT_AFINIT -- Inititialize the AFILTCAT task structure.

procedure at_afinit (at)

pointer at                      #O the pointer to the astrometry descriptor

begin
        # Initialize the astrometry structure.
        call at_ainit (at)

        # Initialize the i/o structure.
        call at_ioinit (at)

        # Initialize the filtering / selection structure.
        call at_fsinit (at)
end


# AT_AFFREE -- Free the AFILTCAT task structure.

procedure at_affree (at)

pointer at                      #U the pointer to the astrometry descriptor

begin
        # Free the filtering / selection structure.
        call at_fsfree (at)

        # Free the i/o structure
        call at_iofree (at)

        # Free the astrometry structure.
        call at_afree (at)
end


# AT_AIGINIT -- Inititialize the agetim task structure.

procedure at_aiginit (at)

pointer at                      #O the pointer to the astrometry descriptor

begin
        # Initialize the astrometry structure.
        call at_ainit (at)

        # Initialize the i/o structure.
        call at_ioinit (at)

        # Initialize the region definition structure.
        call at_rcinit (at)

        # Initialize the default wcs structure.
        #call at_wcinit (at)

        # Initialize the default image data structure.
        #call at_iminit (at)
end


# AT_AIGFREE -- Free the agetim task structure.

procedure at_aigfree (at)

pointer at                      #U the pointer to the astrometry descriptor

begin
        # Free the default image data structure.
        #call at_imfree (at)

        # Free the default wcs structure.
        #call at_wcfree (at)

        # Free the field center structure.
        call at_rcfree (at)

        # Free the i/o structure
        call at_iofree (at)

        # Free the astrometry structure.
        call at_afree (at)
end


# AT_AHINIT -- Inititialize the AHEDIT task structure.

procedure at_ahinit (at)

pointer at                      #O the pointer to the astrometry descriptor

begin
        # Initialize the astrometry structure.
        call at_ainit (at)

        # Initialize the i/o structure.
        call at_ioinit (at)

        # Initialize the default wcs structure.
        call at_wcinit (at)

        # Initialize the default image data structure.
        call at_iminit (at)
end


# AT_AHFREE -- Free the AHEDIT task structure.

procedure at_ahfree (at)

pointer at                      #U the pointer to the astrometry descriptor

begin
        # Free the default image data structure.
        call at_imfree (at)

        # Free the default wcs structure.
        call at_wcfree (at)

        # Free the i/o structure
        call at_iofree (at)

        # Free the astrometry structure.
        call at_afree (at)
end
