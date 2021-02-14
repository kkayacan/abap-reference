*----------------------------------------------------------------------*
*                                                                      *
*       Output-modules for infotype 9011                               *
*                                                                      *
*----------------------------------------------------------------------*





DATA : BEGIN OF w ,
         r1           TYPE c,
         r2           TYPE c,
         commnt01(85) TYPE c,
         commnt02(85) TYPE c,
         commnt03(85) TYPE c,
         comment(255) TYPE c,
         icrdt        TYPE zhr_icrdx , " icra dairesi açıklaması
         sub_t        TYPE stext,
       END OF w .

DATA : BEGIN OF t ,
         debtc TYPE zhr_debtc , "
         mx    TYPE zhr_oran  , " maaş / oran
         my    TYPE zhr_oran  , " maaş / oran
         ix    TYPE zhr_oran  ,  " ikramiye / oran
         iy    TYPE zhr_oran  , " ikramiye / oran
         dx    TYPE zhr_oran  , " diğer gelirler / oran
         dy    TYPE zhr_oran  , " diğer gelirler / oran
       END OF t .

*-
DATA t9012   LIKE pa9012 OCCURS 0 WITH HEADER LINE .
DATA l_total TYPE zhr_debto .

DATA: BEGIN OF p ,
        banka(60),
      END OF   p.


*----------------------------------------------------------------------*
*       MODULE  P9011 OUTPUT                                           *
*----------------------------------------------------------------------*
*       Default values, Texts                                          *
*----------------------------------------------------------------------*
MODULE p9011 OUTPUT.
  IF PSYST-NSELC EQ YES.
* read text fields etc.; do this whenever the screen is show for the
*  first time:
*   PERFORM RExxxx.
    IF PSYST-IINIT = YES AND PSYST-IOPER = INSERT.
* generate default values; do this the very first time on insert only:
*     PERFORM GET_DEFAULT.
      CLEAR w.
    ENDIF.
  ENDIF.




  CASE p9011-ctype .
    WHEN 1 . w-r1 = 'X' . w-r2 = space .
    WHEN 2 . w-r2 = 'X' . w-r1 = space .
  ENDCASE .

* Screen Invisible ..
  CASE p9011-subty .
    WHEN '01' .
      PERFORM screen_invisible USING : '001' 0 0 , '002' 0 0 .
    WHEN '02' .
      CASE 'X' .
        WHEN w-r1 .
          PERFORM screen_invisible USING '003' 0 0 .

          IF p9011-mx IS NOT INITIAL OR
             p9011-cx IS NOT INITIAL OR
             p9011-dx IS NOT INITIAL   .
            MOVE : p9011-mx TO t-mx , p9011-my TO t-my ,
                   p9011-dx TO t-dx , p9011-dy TO t-dy ,
                   p9011-ix TO t-ix , p9011-iy TO t-iy .
          ENDIF .

          CLEAR : p9011-mx , p9011-ix , p9011-dx ,
                  p9011-my , p9011-iy , p9011-dy .

          IF NOT t-debtc IS INITIAL .
            MOVE t-debtc TO p9011-debtc .
            CLEAR t-debtc .
          ELSE .
            MOVE p9011-debtc TO t-debtc .
          ENDIF .
        WHEN w-r2 .
          PERFORM screen_invisible USING '002' 0 0 .

          IF t-mx IS NOT INITIAL OR
             t-ix IS NOT INITIAL OR
             t-dx IS NOT INITIAL   .
            MOVE : t-mx TO p9011-mx , t-my TO p9011-my ,
                   t-ix TO p9011-ix , t-iy TO p9011-iy ,
                   t-dx TO p9011-dx , t-dy TO p9011-dy .

            CLEAR: t-mx , t-ix , t-dx ,
                   t-my , t-iy , t-dy .
          ENDIF .

          IF NOT p9011-debtc IS INITIAL .
            MOVE p9011-debtc TO t-debtc .
          ENDIF .
          CLEAR p9011-debtc .
      ENDCASE .
    WHEN '03' .
      PERFORM screen_invisible USING : '005' 0 0 , '006' 0 0 .
      PERFORM screen_invisible USING : '003' 0 0 , '004' 0 0 .
    WHEN '04' .
      PERFORM screen_invisible USING : '001' 0 0 , '002' 0 0 .
  ENDCASE .

*
  IF p9011-status IS INITIAL .
    p9011-status = 1 .
  ENDIF .

  IF p9011-status EQ 2 AND p9011-debto NE 0 .
    p9011-status = space .
    MESSAGE TEXT-011 TYPE 'S' DISPLAY LIKE 'E' .
  ENDIF .

*
  CLEAR w-sub_t .
  SELECT SINGLE stext FROM t591s INTO w-sub_t
   WHERE sprsl EQ sy-langu
     AND infty EQ '9011'
     AND subty EQ p9011-subty .

  IF NOT p9011-icrdr IS INITIAL .
    CLEAR w-icrdt .
    SELECT SINGLE icrdx FROM zicdhr_pa_t001 INTO w-icrdt
      WHERE icrdr EQ p9011-icrdr .
  ENDIF .

  IF NOT p9011-commnt IS INITIAL .
    SPLIT p9011-commnt AT '#' INTO w-commnt01 w-commnt02 w-commnt03 .
  ELSE .
    IF NOT w-commnt01 IS INITIAL OR
       NOT w-commnt02 IS INITIAL OR
       NOT w-commnt03 IS INITIAL   .
     IF sy-ucomm NE 'NXT' AND
        sy-ucomm NE 'INS'."Ece
      CONCATENATE w-commnt01 w-commnt02 w-commnt03
      INTO p9011-commnt SEPARATED BY '#' .
     ELSE.
      CLEAR: w-commnt01, w-commnt02, w-commnt03.
     ENDIF.
    ENDIF .
  ENDIF .

*-
  CLEAR : t9012 , t9012[] , l_total .
  SELECT * FROM pa9012 INTO TABLE t9012
   WHERE pernr EQ p9011-pernr
     AND logid EQ p9011-logid .

  IF sy-subrc EQ 0 .
    LOOP AT t9012. "WHERE ( subty = '01' OR subty EQ '02' ) .
      ADD t9012-debtc TO l_total .
    ENDLOOP .
  ENDIF .

  IF NOT l_total IS INITIAL .
    MOVE : l_total TO p9011-debtc .
  ENDIF .

  IF p9011-debtt GT 0 .
    p9011-debto = p9011-debtt - l_total .
  ELSE  .
    p9011-debto = 0 .
  ENDIF .

  IF p9011-debto LT 0 .
    p9011-debto = 0 .
  ENDIF.

  IF p9011-debtt NE 0 AND p9011-debto EQ 0 .
    p9011-status = 2 .
    LOOP AT SCREEN .
      CHECK screen-group3 EQ '004' .
      screen-input     = 0 .
      screen-invisible = 0 .
      MODIFY SCREEN .    EXIT .
    ENDLOOP .
  ENDIF .


ENDMODULE.
*----------------------------------------------------------------------*
*       MODULE  P9011L OUTPUT                                          *
*----------------------------------------------------------------------*
*       read texts for listscreen
*----------------------------------------------------------------------*
MODULE p9011l OUTPUT.
* PERFORM RExxxx.

  CLEAR w-icrdt .
  SELECT SINGLE icrdx FROM zicdhr_pa_t001 INTO w-icrdt
   WHERE icrdr EQ p9011-icrdr .

  CLEAR w-sub_t .
  SELECT SINGLE stext FROM t591s INTO w-sub_t
   WHERE sprsl EQ sy-langu
     AND infty EQ '9005'
     AND subty EQ p9011-subty .


ENDMODULE.




*&---------------------------------------------------------------------*
*&      Form  SCREEN_INVISIBLE
*&---------------------------------------------------------------------*
FORM screen_invisible USING if_group if_input if_inv .

  LOOP AT SCREEN .
    CASE screen-group2 .
      WHEN if_group .
        screen-input     = if_input .
        screen-invisible = if_inv   .
        MODIFY SCREEN .
    ENDCASE .
  ENDLOOP .

ENDFORM.                                             " screen_invisible

*&---------------------------------------------------------------------*
*&      module  desc_control  input
*&---------------------------------------------------------------------*
MODULE desc_control INPUT.
  CONCATENATE w-commnt01 w-commnt02 w-commnt03
         INTO p9011-commnt SEPARATED BY '#' .
ENDMODULE.                                        " desc_control  input

*&---------------------------------------------------------------------*
*&      module  rate_control  input
*&---------------------------------------------------------------------*
MODULE rate_control INPUT.

ENDMODULE.                                        " rate_control  input

*&---------------------------------------------------------------------*
*&      module  status_control  input
*&---------------------------------------------------------------------*
MODULE status_control INPUT.
  IF p9011-status EQ 2 .
    MESSAGE TEXT-011 TYPE 'S' DISPLAY LIKE 'E' .
  ENDIF .
ENDMODULE.                                      " status_control  input

*&---------------------------------------------------------------------*
*&      module  ctype_control  input
*&---------------------------------------------------------------------*
MODULE ctype_control INPUT.
  IF NOT w-r1 IS INITIAL . MOVE 1 TO p9011-ctype . ENDIF .
  IF NOT w-r2 IS INITIAL . MOVE 2 TO p9011-ctype . ENDIF .
ENDMODULE.                                       " ctype_control  input

*&---------------------------------------------------------------------*
*&      module  clear_variable  input
*&---------------------------------------------------------------------*
MODULE clear_variable INPUT.
  CHECK sy-ucomm EQ 'UPD' . CLEAR : t , w .
ENDMODULE.                                      " clear_variable  input