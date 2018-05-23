INITIALIZATION.
  PERFORM initialization.

FORM initialization .

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = VALUE sscr_restrict(
                               opt_list_tab = VALUE #(
                               ( name    = 'EQ_LE_GE'
                                 options = VALUE #(
                                 eq = abap_true
                                 le = abap_true
                                 ge = abap_true ) ) )
                               ass_tab = VALUE #(
                               ( kind    = 'S'
                                 name    = 'S_SPMON'
                                 sg_main = 'I'
                                 sg_addy = ''
                                 op_main = 'EQ_LE_GE' ) ) )
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      OTHERS                 = 9.

ENDFORM.