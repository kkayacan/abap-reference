  DATA : jobname      TYPE tbtcjob-jobname VALUE 'ZADESECARD',
         jobclass     TYPE tbtcjob-jobclass VALUE 'A',
         jobcount     TYPE tbtcjob-jobcount,
         lv_startdate LIKE sy-datum,
         lv_starttime LIKE sy-uzeit,
         authcknam    TYPE tbtcjob-authcknam,
         lv_varname   TYPE rsvar-variant,
         lwa_vardesc  TYPE varid.

  DATA: ls_varid    TYPE varid,
        lt_varit    TYPE TABLE OF varit,
        lt_rsparams TYPE TABLE OF rsparams.

  IF NOT ( ipspar-infty = '0000' AND ( ipspar-massn = '01' OR ipspar-massn = '10' ) ).
    RETURN.
  ENDIF.

  authcknam = sy-uname.

  lv_startdate = sy-datum.
  lv_starttime = sy-uzeit + 600.

  CONCATENATE  'CARD' lv_starttime INTO lv_varname.

  lwa_vardesc-report = 'ZHR_SEND_ADESECARD_FORM'.

* fill VARID structure - Variantenkatalog, variant description

  ls_varid-mandt        = sy-mandt.
  ls_varid-report       = lwa_vardesc-report.
  ls_varid-variant      = lv_varname.
  ls_varid-flag1        = space.
  ls_varid-flag2        = space.
  ls_varid-transport    = space.
  ls_varid-environmnt   = 'A'.         "Variant for batch and online
  ls_varid-protected    = space.
  ls_varid-secu         = space.
  ls_varid-version      = '1'.
  ls_varid-ename        = sy-uname.
  ls_varid-edat         = sy-datum.
  ls_varid-etime        = sy-uzeit.
  ls_varid-aename       = space.
  ls_varid-aedat        = space.
  ls_varid-aetime       = space.
  ls_varid-mlangu       = sy-langu.

*.fill VARIT structure - Variantentexte; variant texts
  APPEND INITIAL LINE TO lt_varit ASSIGNING FIELD-SYMBOL(<ls_varit>).
  <ls_varit>-mandt      = sy-mandt.
  <ls_varit>-langu      = sy-langu.
  <ls_varit>-report     = lwa_vardesc-report.
  <ls_varit>-variant    = lv_varname.
  <ls_varit>-vtext      = lv_varname.

  APPEND INITIAL LINE TO lt_rsparams ASSIGNING FIELD-SYMBOL(<ls_rsparams>).
  <ls_rsparams>-selname = 'P_PERNR'.
  <ls_rsparams>-kind    = 'P'.
  <ls_rsparams>-sign    = 'I'.
  <ls_rsparams>-option  = 'EQ'.
  <ls_rsparams>-low     = ipspar-pernr.

  APPEND INITIAL LINE TO lt_rsparams ASSIGNING <ls_rsparams>.
  <ls_rsparams>-selname = 'P_INFTY'.
  <ls_rsparams>-kind    = 'P'.
  <ls_rsparams>-sign    = 'I'.
  <ls_rsparams>-option  = 'EQ'.
  <ls_rsparams>-low     = ipspar-infty.

  APPEND INITIAL LINE TO lt_rsparams ASSIGNING <ls_rsparams>.
  <ls_rsparams>-selname = 'P_MASSN'.
  <ls_rsparams>-kind    = 'P'.
  <ls_rsparams>-sign    = 'I'.
  <ls_rsparams>-option  = 'EQ'.
  <ls_rsparams>-low     = ipspar-massn.

  APPEND INITIAL LINE TO lt_rsparams ASSIGNING <ls_rsparams>.
  <ls_rsparams>-selname = 'P_DATUM'.
  <ls_rsparams>-kind    = 'P'.
  <ls_rsparams>-sign    = 'I'.
  <ls_rsparams>-option  = 'EQ'.
  <ls_rsparams>-low     = ipspar-begda.

* Create Variant
  CALL FUNCTION 'RS_CREATE_VARIANT'
    EXPORTING
      curr_report               = ls_varid-report
      curr_variant              = ls_varid-variant
      vari_desc                 = ls_varid
    TABLES
      vari_contents             = lt_rsparams
      vari_text                 = lt_varit
    EXCEPTIONS
      illegal_report_or_variant = 1
      illegal_variantname       = 2
      not_authorized            = 3
      not_executed              = 4
      report_not_existent       = 5
      report_not_supplied       = 6
      variant_exists            = 7
      variant_locked            = 8
      OTHERS                    = 9.

  IF sy-subrc <> 0.

  ENDIF.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = jobname
      jobclass         = jobclass
    IMPORTING
      jobcount         = jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc = 0.

    CALL FUNCTION 'JOB_SUBMIT'
      EXPORTING
        authcknam               = authcknam
        jobcount                = jobcount
        jobname                 = jobname
        report                  = ls_varid-report
        variant                 = ls_varid-variant
      EXCEPTIONS
        bad_priparams           = 1
        bad_xpgflags            = 2
        invalid_jobdata         = 3
        jobname_missing         = 4
        job_notex               = 5
        job_submit_failed       = 6
        lock_failed             = 7
        program_missing         = 8
        prog_abap_and_extpg_set = 9
        OTHERS                  = 10.

    IF sy-subrc = 0.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = jobcount
          jobname              = jobname
          sdlstrtdt            = lv_startdate
          sdlstrttm            = lv_starttime
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          OTHERS               = 9.
      IF sy-subrc = 0.

        CALL FUNCTION 'RS_VARIANT_DELETE'
          EXPORTING
            report               = ls_varid-report
            variant              = ls_varid-variant
            flag_confirmscreen   = 'X'
            flag_delallclient    = 'X'
          EXCEPTIONS
            not_authorized       = 1
            not_executed         = 2
            no_report            = 3
            report_not_existent  = 4
            report_not_supplied  = 5
            variant_locked       = 6
            variant_not_existent = 7
            no_corr_insert       = 8
            variant_protected    = 9
            OTHERS               = 10.
        IF sy-subrc <> 0.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.