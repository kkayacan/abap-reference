DATA excluded_options  TYPE rsoptions.
DATA tab_and_field     TYPE rstabfield.
DATA r_werks TYPE RANGE OF t001w-werks.

tab_and_field = VALUE #( tablename = 'T001W' fieldname = 'WERKS' ).
excluded_options = VALUE #( cp = abap_true nb = abap_true ne = abap_true np = abap_true ).

CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
  EXPORTING
    just_incl         = 'X'
    excluded_options  = excluded_options
    tab_and_field     = tab_and_field
  TABLES
    range             = r_werks
  EXCEPTIONS
    no_range_tab      = 1
    cancelled         = 2
    internal_error    = 3
    invalid_fieldname = 4
    OTHERS            = 5.