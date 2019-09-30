REPORT yp_salv_mini.

TABLES: bkpf.

DATA o_salv TYPE REF TO cl_salv_table.
DATA t_bkpf TYPE TABLE OF bkpf.

SELECT-OPTIONS: s_budat FOR bkpf-budat.

START-OF-SELECTION.

  SELECT * FROM  bkpf INTO TABLE t_bkpf
         WHERE  budat IN s_budat.

END-OF-SELECTION.

  CALL METHOD cl_salv_table=>factory
    IMPORTING
      r_salv_table = o_salv
    CHANGING
      t_table      = t_bkpf.

  o_salv->get_functions( )->set_all( abap_true ).
  o_salv->get_columns( )->set_optimize( abap_true ).
  o_salv->get_columns( )->set_key_fixation( value = abap_true ).
  o_salv->get_columns( )->set_column_position( columnname = 'BUDAT'
                                               position   = 1 ).

  o_salv->display( ).
