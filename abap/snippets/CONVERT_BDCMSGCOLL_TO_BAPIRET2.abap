DATA: t_bdcmsg TYPE TABLE OF bdcmsgcoll,
      t_return TYPE TABLE OF bapiret2.

CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
  TABLES
    imt_bdcmsgcoll = t_bdcmsg
    ext_return     = t_return.