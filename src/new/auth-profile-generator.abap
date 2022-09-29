REPORT zprofilegen MESSAGE-ID 5@.

TABLES: usr10, agr_1016, agr_prof.

CONSTANTS:    state_altered VALUE 'A',
              state_saved   VALUE 'S',
              state_gen     VALUE 'G',
              state_nothing VALUE ' ',
              x TYPE abap_bool VALUE 'X',
      new_old               VALUE 'O',
      status_standard       VALUE 'S',
              var_kz_left   VALUE '(',
              var_kz_right  VALUE ')'.

DATA: gv_gen TYPE c LENGTH 1,
      i_prof LIKE pt1016 OCCURS 10 WITH HEADER LINE,
      global_act_objid LIKE agr_define-agr_name,
      state_old,
      state_save VALUE 'A',
      profil_vergeben VALUE 'X',
      profile_text    LIKE usr11-ptext,
      stellen_profile LIKE usr10-profn,
BEGIN OF i_auth   OCCURS 100.
        INCLUDE STRUCTURE pt1250.
        INCLUDE STRUCTURE tpr_sta_of.
DATA: END OF i_auth,
BEGIN OF i_au_fld OCCURS 100.
        INCLUDE STRUCTURE pt1251.
DATA: END OF i_au_fld,
      zwei_hochkomma(2) VALUE '''''',
      ein_hochkomma(1) VALUE '''',
      leerstring(3) VALUE ''' ''',
      vormerken_generieren,
BEGIN OF orgebenen OCCURS 30.
        INCLUDE STRUCTURE usorg_db.
DATA: END OF orgebenen,
      BEGIN OF s_stat.
*       INCLUDE STRUCTURE HROBJECT.
DATA:   objid LIKE agr_define-agr_name,
        stellen_text LIKE agr_texts-text.    " Langtext zur Stelle
*       STELLEN_KURZ(12),    " Kurztext zur Stelle
*       BEGDA LIKE SY-DATUM,
*       ENDDA LIKE SY-DATUM,
*       VTASK LIKE HRRHAP-VTASK.
        INCLUDE STRUCTURE tpr_sta_of.
        INCLUDE STRUCTURE tpr_sta_in.
DATA: END OF s_stat,
      antwort,
      kz_show_mode,
      global_profile_name LIKE agr_prof-profile,
      global_profile_text LIKE agr_prof-ptext,
      d_auth LIKE i_auth     OCCURS 0 WITH HEADER LINE,
      d_au_fld LIKE i_au_fld OCCURS 0 WITH HEADER LINE,
      i_storg  LIKE pt1252  OCCURS 100 WITH HEADER LINE,
      db_storg LIKE i_storg OCCURS 100 WITH HEADER LINE,
      m_storg  LIKE i_storg OCCURS 100 WITH HEADER LINE,
      t_auth LIKE i_auth     OCCURS 10 WITH HEADER LINE,
      BEGIN OF name_auth OCCURS 100,
         object LIKE usobt-object,
         auth   LIKE usr12-auth,
         count(2) TYPE n,
      END OF name_auth,
      i_varb LIKE pt1254 OCCURS 10 WITH HEADER LINE,
      i_dfies LIKE dfies OCCURS 100 WITH HEADER LINE,
      i_vari LIKE pt1253 OCCURS 10 WITH HEADER LINE,
      d_vari LIKE pt1253 OCCURS 10 WITH HEADER LINE.

PARAMETERS agr_name TYPE agr_define-agr_name OBLIGATORY DEFAULT 'Z_EAK_006'.
PARAMETERS profile  TYPE usr10-profn OBLIGATORY DEFAULT 'T-GV130131'.
PARAMETERS ptext    TYPE usr11-ptext OBLIGATORY DEFAULT 'Rol için profil Z_EAK_006'.
PARAMETERS vstel TYPE vstel OBLIGATORY DEFAULT '1000'.

global_act_objid = agr_name.
profile_text = ptext.
stellen_profile = profile.

i_auth-object = 'V_LIKP_VST'.
i_auth-auth   = profile && '00'.
i_auth-modified = 'U'.
i_auth-neu = 'O'.
i_auth-node = '000007'.
i_auth-atext = 'Teslimat: Sevkiyat noktaları için yetkiler'.
APPEND i_auth.

i_au_fld-object = i_auth-object.
i_au_fld-auth   = i_auth-auth.
i_au_fld-field  = 'ACTVT'.
i_au_fld-low    = '01'.
i_au_fld-modified = 'U'.
i_au_fld-neu      = 'O'.
i_au_fld-node     = '000008'.
APPEND i_au_fld.
i_au_fld-low    = '04'.
APPEND i_au_fld.
i_au_fld-field  = 'VSTEL'.
i_au_fld-low    = vstel.
i_au_fld-modified = 'M'.
i_au_fld-node     = '000009'.
APPEND i_au_fld.

orgebenen-field = 'VSTEL'.
orgebenen-langu = sy-langu.
orgebenen-varbl = '$VSTEL'.
orgebenen-vtext = 'Shipping point'.
APPEND orgebenen.

s_stat-objid = agr_name.

db_storg-varbl = '$VSTEL'.
APPEND db_storg.

*i_dfies dolacak

PERFORM act_generate_profile USING abap_true gv_gen.

FORM act_generate_profile USING dialog gen.
  DATA: text1(50) TYPE c,
        text2(50) TYPE c,
        actvt(2)  TYPE c,
        ret       LIKE sy-subrc,
        var       LIKE usvar-varbl.
  DATA: h_prof   LIKE i_prof OCCURS 10  WITH HEADER LINE,
        b_usval  LIKE usval  OCCURS 100 WITH HEADER LINE,
        b_usaut  LIKE usaut  OCCURS 100 WITH HEADER LINE,
        o_usaut  LIKE usaut  OCCURS 100 WITH HEADER LINE,
        prof_in  LIKE uspro  OCCURS 100 WITH HEADER LINE,
        prof_out LIKE uspro  OCCURS 100 WITH HEADER LINE.

* Check whether the authorization for the profile generation exists
  CALL FUNCTION 'PRGN_AUTH_ACTIVITY_GROUP'  "note 367299
       EXPORTING
            activity_group  = global_act_objid
            action_generate = 'X'
       EXCEPTIONS
            not_authorized  = 1
            OTHERS          = 2.
  IF sy-subrc NE 0.
    if dialog eq x.
      message e425(s#) with s_stat-objid.
    endif.
    gen = 'A'.
    EXIT.
  ENDIF.

  state_old = state_save.
  gen = x.
* Check whether the role has been completely maintained
  if dialog eq x.
    if s_stat-status_var gt 0.
      text1 = 'Es gibt offene Orgebenen.'(314).
    endif.
    if s_stat-status_mis gt 0.
      text2 = 'Es gibt offene Berechtigungen'(310).
    endif.
    if s_stat-status_var gt 0 or s_stat-status_mis gt 0.
      call function 'PRGN_POPUP_TO_DECIDE'
        exporting
          defaultoption = '2'
          textline1     = text1
          textline2     = text2
          text_option1  = 'Generieren'(311)
          text_option2  = 'Nachpflegen'(312)
          titel         = 'Profil generieren'(313)
        importing
          answer        = antwort
        exceptions
          others        = 1.
      if antwort ne '1'.
*       Cancellation
        gen = space.
        message s001.                  " 'Aktion abgebrochen'.
        exit.
      endif.
    endif.
*   Saving the authorization data of the role
    if ( state_save eq state_nothing or state_save eq state_altered )
         and kz_show_mode eq space.
      perform db_save_auth using space ret.
      if ret ne 0.
        gen = space.
        exit.
      endif.
    endif.
    state_save = state_saved.
  endif.

  " Profilname available
  IF profil_vergeben IS INITIAL.
    if dialog eq x.
      message s051(5@).
    endif.
    gen = 'P'. "Program error
    RETURN.
  ENDIF.


* Saving the old profiles
  h_prof[] = i_prof[].
* Check and copy of existing profiles
  LOOP AT i_prof WHERE generated = x.
    prof_in-profn = i_prof-profile.
    prof_in-ptext = profile_text.
    APPEND prof_in.
  ENDLOOP.
  IF sy-subrc NE 0.
*   Profile name does not exist yet.
    prof_in-profn    = stellen_profile.
    prof_in-profn+10 = space.
    prof_in-ptext    = profile_text.
    APPEND prof_in.
    actvt = '01'.
  ELSE.
*   note 1365174
*   Consistency check: Profile list must contain torso
    CLEAR ret.
    SORT prof_in BY profn.
    READ TABLE prof_in INDEX 1.
    IF prof_in-profn NE stellen_profile.
      REFRESH prof_in.
      ret = 4.
    ELSE.
*     Check whether the profile has already been generated at least once
      CLEAR usr10.
      SELECT SINGLE * FROM usr10 WHERE profn = stellen_profile
                                 AND   typ   = 'G'.     "#EC CI_GENBUFF
      IF sy-subrc NE 0.
        ret = 4.
      ENDIF.
    ENDIF.
    IF ret EQ 4.
      actvt = '01'.
    ELSE.
      actvt = '02'.
    ENDIF.
*   end of correction (note 1365174)
  ENDIF.
* Copy of the authorization data of the role
  REFRESH: b_usaut, b_usval.
  LOOP AT i_auth WHERE deleted = space.
    b_usaut-action = '01'.
    b_usaut-objct  = i_auth-object.
    b_usaut-auth   = i_auth-auth.
    b_usaut-atext  = i_auth-atext.
    APPEND b_usaut.
  ENDLOOP.
  IF sy-subrc NE 0.
*   note 943796:
*   If the profile was never generated before (ACTVT = 01) the procedure
*   can be cancelled. Otherwise the authorizations inside the profile
*   will be deleted because the role doesn't contain any authorization
*   data.
    IF actvt EQ '01'.
      if dialog eq x.
        message s053 with global_act_objid.
      endif.
      gen = 'E'.
      EXIT.
    ELSE.
      actvt = '06'.
    ENDIF.
*   end of correction (943796)
  ELSE.
    LOOP AT i_au_fld WHERE deleted = space.
      b_usval-objct  = i_au_fld-object.
      b_usval-auth   = i_au_fld-auth.
      b_usval-sfield = i_au_fld-field.
      b_usval-von    = i_au_fld-low.
      b_usval-bis    = i_au_fld-high.
      PERFORM is_org USING i_au_fld-field var ret.
      IF ret EQ 0 AND b_usval-von EQ var.
*       Removing the technical identifier of org fields; Otherwise it
*       would be copied to the generated authorization.
        CLEAR: b_usval-von, b_usval-bis.
        APPEND b_usval.
        CONTINUE.
      ENDIF.
      IF b_usval-von EQ ein_hochkomma  OR
         b_usval-von EQ zwei_hochkomma.
        CLEAR b_usval-von.
      ENDIF.
      IF b_usval-bis EQ leerstring     OR
         b_usval-bis EQ ein_hochkomma  OR
         b_usval-bis EQ zwei_hochkomma.
        CLEAR b_usval-bis.
      ENDIF.
      APPEND b_usval.
    ENDLOOP.
*   Insertion of the string ' ' in VON fields containing space.
    CALL FUNCTION 'SUPRN_CORRECT_INPUT_TABLES'
      TABLES
        values = b_usval
        auths  = b_usaut
      EXCEPTIONS
        OTHERS = 1.
  ENDIF.
* Profile generation
  CALL FUNCTION 'SUSR_INTERFACE_PROF'
    EXPORTING
      profile                    = stellen_profile
      ptext                      = profile_text
      action                     = actvt
      no_check_in_create_mode    = 'X'
      no_check_in_update_mode    = 'X'
      dialog                     = 'X'
    TABLES
      values                     = b_usval
      auths                      = b_usaut
      prof_in                    = prof_in
      prof_out                   = prof_out
    EXCEPTIONS
      not_authorized_for_auth    = 1
      not_authorized_for_profile = 2
      authorization_overflow     = 3
      OTHERS                     = 4.
  IF sy-subrc EQ 0.
*   note 939061
    REFRESH i_prof.
*   end of correction (note 939061)
*   note 943796:
*   Copy of generated profiles into the global table
    IF NOT prof_out[] IS INITIAL.
      LOOP AT prof_out.
        CLEAR i_prof.
        i_prof-profile   = prof_out-profn.
        i_prof-generated = x.
        i_prof-pstate    = prof_out-pstate.
        READ TABLE i_prof WITH KEY profile = prof_out-profn.
        IF sy-subrc NE 0.
          APPEND i_prof.
        ENDIF.
      ENDLOOP.
    ELSE.
*     Even though the profile data was deleted one entry in AGR_1016
*     must survive.
      CLEAR i_prof.
      i_prof-profile   = stellen_profile.
      i_prof-generated = x.
      i_prof-pstate    = 'A'.
      APPEND i_prof.
    ENDIF.
*   end of correction (note 943796)
*   Updating table AGR_1016
    CLEAR vormerken_generieren.
    PERFORM db_save_1016 USING x.
    if dialog eq x.
*     Success message
      message s018.
      state_save = state_gen.
    endif.
*   Database update
    PERFORM db_update_database.
*   Update of the time stamp for the profile generation
    CALL FUNCTION 'PRGN_SET_GENERATE_TIMESTAMP'
      EXPORTING
        activity_group = agr_name "s_stat-objid
      EXCEPTIONS
        OTHERS         = 1.
*   Adjustment of the profile assignments to users
    PERFORM check_and_update_users TABLES h_prof i_prof
                                   USING  ret.
  ELSE.
    CASE sy-subrc.
      WHEN 1 OR 2.
*       No authorization for maintaining the profile or the
*       authorizations
        gen = 'A'.
      WHEN 3.
*       Too many values per authorization
        gen = 'O'.
      WHEN OTHERS.
        gen = space.
    ENDCASE.
    ROLLBACK WORK.
    CALL FUNCTION 'PRGN_CLEAR_BUFFER'
      EXCEPTIONS
        OTHERS = 1.
    IF profil_vergeben EQ 'X'.
      SELECT SINGLE * FROM agr_1016
                      WHERE agr_name = global_act_objid. "#EC CI_GENBUFF
      IF sy-subrc NE 0.
        profil_vergeben = space.
      ENDIF.
    ENDIF.
    state_save = state_old.
  ENDIF.

* Exits for SAP components und customers
  DATA: l_ssm_cust LIKE ssm_cust OCCURS 0 WITH HEADER LINE.
  SELECT SINGLE * FROM ssm_cust INTO l_ssm_cust
         WHERE id = 'SAP_AFTER_PROF_GEN'.
  IF sy-subrc = 0 AND l_ssm_cust-path <> space.
    CALL FUNCTION l_ssm_cust-path
      EXPORTING
        activity_group = global_act_objid
        generated      = gen.
  ENDIF.
  SELECT SINGLE * FROM ssm_cust INTO l_ssm_cust
         WHERE id = 'Z_AFTER_PROF_GEN'.
  IF sy-subrc = 0 AND l_ssm_cust-path <> space.
    CALL FUNCTION l_ssm_cust-path
      EXPORTING
        activity_group = global_act_objid
        generated      = gen.
  ENDIF.

ENDFORM.

FORM check_and_update_users TABLES it_profs_old STRUCTURE pt1016
                                   it_profs_new STRUCTURE pt1016
                            USING  ret          TYPE      sysubrc.

  DATA: lt_profs_del     TYPE TABLE OF pt1016,
        lt_profs_add     TYPE TABLE OF pt1016,
        lt_ust04_old     TYPE          suid_tt_ust04,
        lt_prof_asgm_dif TYPE          suid_tt_cd_usl04,
        lt_return        TYPE          bapirettab.

  DATA: ls_vusl04 TYPE vusl04.

  DATA: ld_torso_old TYPE xuprofile,
        lf_error     TYPE char01.

  FIELD-SYMBOLS: <prof>  TYPE pt1016,
                 <ust04> TYPE ust04.

* If old profiles don't exist user assignments can't be selected.
  READ TABLE it_profs_old ASSIGNING <prof> INDEX 1.
  IF sy-subrc NE 0. EXIT. ENDIF.

* Checking changes between old and new profiles
  SORT it_profs_old BY profile.
  SORT it_profs_new BY profile.
  REFRESH: lt_profs_del, lt_profs_add.
  TRY.
      CALL METHOD cl_susr_basic_tools=>calc_diff_of_2_tables_flex
        EXPORTING
          it_current               = it_profs_old[]
          it_new                   = it_profs_new[]
          id_compare_no_of_columns = 1
        IMPORTING
          et_delete                = lt_profs_del
          et_insert                = lt_profs_add.
    CATCH cx_susr_basic_tools.
  ENDTRY.
  IF lt_profs_del[] IS INITIAL AND lt_profs_add[] IS INITIAL.
*   No profile changes => No adjustments required
    EXIT.
  ENDIF.

* Selecting user assignments of the old profiles
  ld_torso_old = <prof>-profile(10).
  PERFORM chk_prof_asgm_buffer(saplprgn_userprof) USING    ld_torso_old
                                                  CHANGING lt_ust04_old.
  IF lt_ust04_old[] IS INITIAL. EXIT. ENDIF.

* Determining user assignments to be deleted and to be added
  SORT lt_ust04_old BY bname profile.
  CLEAR: ls_vusl04, lt_prof_asgm_dif.
  ls_vusl04-mandt = sy-mandt.
  LOOP AT lt_ust04_old ASSIGNING <ust04>.
    AT NEW bname.
      ls_vusl04-bname = <ust04>-bname.
      ls_vusl04-kz    = 'D'.
      LOOP AT lt_profs_del ASSIGNING <prof>.
        ls_vusl04-profile = <prof>-profile.
        APPEND ls_vusl04 TO lt_prof_asgm_dif.
      ENDLOOP.
      ls_vusl04-kz = 'I'.
      LOOP AT lt_profs_add ASSIGNING <prof>.
        ls_vusl04-profile = <prof>-profile.
        APPEND ls_vusl04 TO lt_prof_asgm_dif.
      ENDLOOP.
    ENDAT.
  ENDLOOP.

* Updating profile assignments
  CLEAR lt_return.
  PERFORM upd_gener_prof_asgm IN PROGRAM saplprgn_userprof
                              USING    space lt_prof_asgm_dif space
                              CHANGING lt_return lf_error.
  IF NOT lt_return IS INITIAL.
    CASE lf_error.
      WHEN 'A'.
*       Authorization failure
        CALL FUNCTION 'PRGN_SET_USERPROF_AUTH_FLAG'
          EXPORTING
            activity_group = global_act_objid.
      WHEN 'E'.
*       User enqueue problem
        CALL FUNCTION 'PRGN_SET_USERPROF_ENQUEUE_FLAG'
          EXPORTING
            activity_group = global_act_objid.
    ENDCASE.
*   Initializing cache table for profile assignments
    PERFORM upd_gt_ust04(saplprgn_userprof) USING 'X' lt_prof_asgm_dif.
*   Displaying error log
    PERFORM display_return(saplprgn_userprof) USING lt_return.
  ELSE.
*   Profile assignments successfully adjusted
*   Updating cache table for profile assignments
    PERFORM upd_gt_ust04(saplprgn_userprof) USING space
                                                  lt_prof_asgm_dif.
  ENDIF.

ENDFORM.

FORM db_save_1016 USING keep_pstate.
  IF keep_pstate EQ space.
    LOOP AT i_prof WHERE generated = x.
      i_prof-pstate = space.           " Kz zurücksetzen
      MODIFY i_prof.
    ENDLOOP.
  ENDIF.
  SORT i_prof.
  CALL FUNCTION 'PRGN_1016_SAVE_PROFILE_NAME'
    EXPORTING
      activity_group = agr_name "s_stat-objid
    TABLES
      i_prof         = i_prof
    EXCEPTIONS
      OTHERS         = 1.
ENDFORM.

FORM db_update_database.
  CALL FUNCTION 'PRGN_UPDATE_DATABASE'
    EXCEPTIONS
      OTHERS = 1.
  CALL FUNCTION 'PRGN_CLEAR_BUFFER'
    EXCEPTIONS
      OTHERS = 1.
ENDFORM.

FORM is_org USING field var ret.
  READ TABLE orgebenen WITH KEY field = field BINARY SEARCH.
  IF sy-subrc = 0.
    ret = 0.
    var = orgebenen-varbl.
  ELSE.
    ret = 4.
    CLEAR var.
  ENDIF.
ENDFORM.

form db_save_auth using do_update subrc.

  data: ret               type sy-subrc
      , var               like orgebenen-varbl
      , d_auth_korrekt    type pt1250 occurs 0 with header line
      , lv_return         type sy-subrc
      .

* Assigning the profile name if not yet done
  if profil_vergeben eq space.

    " Any Authorization for creating a profile
    perform auth_check(saplsusb) using 'P' space space '01' lv_return. " Add Dummy Profile

    if lv_return is initial.
      if global_profile_name eq space.
*        perform get_prof_name using stellen_profile profile_text ret.
      else.
        stellen_profile = global_profile_name.
        profile_text    = global_profile_text.
        ret = 0.
      endif.
      case ret.
        when 0.
          profil_vergeben = x.
          perform act_reorg.
          delete i_prof where variant = space and generated = x.
          i_prof-profile   = stellen_profile.
          i_prof-generated = x.
          clear: i_prof-variant, i_prof-pstate.
          append i_prof.
          clear agr_prof.
          agr_prof-mandt    = sy-mandt.
          agr_prof-agr_name = s_stat-objid.
          agr_prof-langu    = sy-langu.
          agr_prof-profile  = stellen_profile.
          agr_prof-ptext    = profile_text.
          modify agr_prof.
        when 8.
          ret = 0.
        when others.
          message s001.
          subrc = 4.
          exit.
      endcase.
    endif.
  else.
*   note 506825:
*   Renaming the authorizations (Can be necessary after role upload)
    if stellen_profile ne i_auth-auth(10).
      perform act_reorg.
    endif.
  endif.

  subrc = 0.
  d_auth[]   = i_auth[].
  d_au_fld[] = i_au_fld[].
  perform variant_set_db.
* Resetting some flags in d_au_fld and d_auth
  loop at d_au_fld.
    d_au_fld-copied = space.
    d_au_fld-neu    = new_old.
    if d_au_fld-modified eq status_standard.
      perform is_org using d_au_fld-field var ret.
      if ret eq 0.
        d_au_fld-low = var.
        clear d_au_fld-high.
      endif.
    endif.
    modify d_au_fld.
  endloop.
  delete adjacent duplicates from d_au_fld.
  loop at d_auth.
    d_auth-neu    = new_old.
    d_auth-copied = space.
    modify d_auth.
  endloop.
* Saving the data
  d_auth_korrekt[] = d_auth[].
* Authorizations (AGR_1250)
  call function 'PRGN_1250_SAVE_AUTH_DATA'
    exporting
      activity_group = s_stat-objid
    tables
      auth_data      = d_auth_korrekt
    exceptions
      others         = 1.
* Authorization values (AGR_1251)
  call function 'PRGN_1251_SAVE_FIELD_VALUES'
    exporting
      activity_group = s_stat-objid
    tables
      field_values   = d_au_fld
    exceptions
      others         = 1.
  perform variable_pick_all.
  perform db_save_1016 using space.
* Org values (AGR_1252)
  perform org_collect.
  if db_storg[] ne i_storg[].
    call function 'PRGN_1252_SAVE_ORG_LEVELS'
      exporting
        activity_group = s_stat-objid
      tables
        org_levels     = i_storg
      exceptions
        others         = 1.
    db_storg[] = i_storg[].
  endif.
* Database update
  state_save = state_saved.
  if do_update eq x.
    perform db_update_database.
  endif.
* note 518540 (2)
* Re-initialization of table m_storg
  clear m_storg[].
* end of correction (note 518540)
endform.

FORM act_reorg.
  DATA: new_auth LIKE i_auth-auth.
* Initialization of the auxiliary table T_AUTH and authorization
* counters
  REFRESH: t_auth, name_auth.
* Copying the authorizations to T_AUTH
  t_auth[] = i_auth[].
  REFRESH i_auth.
* Reorganisation
  LOOP AT t_auth.
*   Determination of a new counter
    PERFORM auth_new_auth TABLES   i_auth
                          USING    t_auth-object
                          CHANGING new_auth.
*   Assignment of the new number to all values within the authorization
    LOOP AT i_au_fld WHERE object = t_auth-object
                     AND   auth   = t_auth-auth.
      i_au_fld-auth = new_auth.
      MODIFY i_au_fld INDEX sy-tabix TRANSPORTING auth.
    ENDLOOP.
*   Assignment of the new number to the authorization itself (Sorting is
*   ensured, therefore APPEND is sufficient.)
    i_auth      = t_auth.
    i_auth-auth = new_auth.
    APPEND i_auth.
  ENDLOOP.
  SORT: i_auth, i_au_fld.
* Updating the tree structure
*  PERFORM tree_set_status USING x.
ENDFORM.

FORM auth_new_auth TABLES   c_auth   STRUCTURE i_auth
                   USING    object   LIKE      usobt-object
                   CHANGING new_auth LIKE      usr12-auth.
  DATA: tabix        LIKE sy-tabix,
        new_count(2) TYPE n VALUE 0.
* Check whether an authorization number to the object exists at all.
  READ TABLE name_auth WITH KEY object = object BINARY SEARCH.
  tabix = sy-tabix.
  IF sy-subrc NE 0.
*   Initialization of table name_auth
    name_auth-object = object.
    name_auth-auth   = stellen_profile.
    name_auth-count  = 0.
    INSERT name_auth INDEX tabix.
  ENDIF.
* Find the next free authorization number
  DO.
    new_auth    = name_auth-auth.
    new_auth+10 = new_count.
    READ TABLE c_auth WITH KEY object = object
                               auth   = new_auth
                      BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
*     The authorization number must be increased by 1.
      IF new_count GT name_auth-count.
        PERFORM auth_inc_c USING name_auth-count.
        MODIFY name_auth INDEX tabix.
      ENDIF.
      EXIT.
    ENDIF.
*   The maximum number of the counter is 99.
    IF new_count EQ 99.
      EXIT.
    ELSE.
      new_count = new_count + 1.
    ENDIF.
  ENDDO.
ENDFORM.
FORM auth_inc_c USING count TYPE n.
  IF count LT 99.
    count = count + 1.
  ENDIF.
ENDFORM.

FORM org_collect.
  DATA: var LIKE orgebenen-varbl,
        ret LIKE sy-subrc.
  REFRESH i_storg.
* 1. Copying all values that are already on the database
  LOOP AT i_au_fld.
    AT NEW field.
      PERFORM is_org USING i_au_fld-field var ret.
      IF ret EQ 0.
*       Were the org level and its values already inserted? If no, they
*       will be copied from db_storg.
        READ TABLE i_storg WITH KEY varbl = var.
        IF sy-subrc NE 0.
          LOOP AT db_storg WHERE varbl = var.
            APPEND db_storg TO i_storg.
          ENDLOOP.
          IF sy-subrc NE 0.
            CLEAR i_storg.
            i_storg-varbl = var.
            APPEND i_storg.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDAT.
  ENDLOOP.
* note 518540 (2)
* 2. Copying all new values not maintained via the org level screen
  LOOP AT m_storg.
    IF m_storg-low EQ '*'.
      DELETE i_storg WHERE varbl = m_storg-varbl.
    ELSEIF m_storg-low NE space.
      DELETE i_storg WHERE varbl = m_storg-varbl
                     AND   low   = space.
    ENDIF.
    READ TABLE i_storg WITH KEY varbl = m_storg-varbl
                                low   = m_storg-low
                                high  = m_storg-high
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      INSERT m_storg INTO i_storg INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
  SORT i_storg.
* end of correction (note 518540)
ENDFORM.

FORM VARIABLE_PICK_ALL.
  DATA: INDEX LIKE SY-TABIX.
  REFRESH I_VARB.
                                       " Alle Felder mit Variablen
  LOOP AT I_AU_FLD WHERE LOW(1) = VAR_KZ_LEFT.
    " Sicherheitshalber Check ob auch die schließende Klammer da...
    CHECK I_AU_FLD-LOW CS VAR_KZ_RIGHT.
                                       " Domänenname holen
    READ TABLE I_DFIES WITH KEY FIELDNAME = I_AU_FLD-FIELD
                       BINARY SEARCH.
    CHECK SY-SUBRC = 0.
    READ TABLE I_VARB WITH KEY USERVAR = I_AU_FLD-LOW
                      BINARY SEARCH.
    INDEX = SY-TABIX.
    IF SY-SUBRC <> 0.
                                       " noch nicht da -> einfügen
      I_VARB-USERVAR   = I_AU_FLD-LOW.
      I_VARB-DOMNAME   = I_DFIES-DOMNAME.
      INSERT I_VARB INDEX INDEX.
    ELSE.
      " schon da. Stimmt Domäne überein ??
      IF I_VARB-DOMNAME <> I_DFIES-DOMNAME.
        " ungleich. äussert ungeschickt. kein F4 drin!
        CLEAR: I_VARB-DOMNAME.
        MODIFY I_VARB INDEX INDEX.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

form variant_set_db.
  DATA: H_VARI LIKE I_VARI OCCURS 0 WITH HEADER LINE.
                                       " Alle Varianten zusammensuchen
  LOOP AT I_VARI.
                                       " Aktuelle Info aus i_vari
    APPEND I_VARI TO H_VARI.
                                       " Andere Sprachen aus D_VARI
    LOOP AT D_VARI WHERE VARIANT = I_VARI-VARIANT
                   AND   LANGU <> SY-LANGU.
      APPEND D_VARI TO H_VARI.
    ENDLOOP.
  ENDLOOP.
  SORT H_VARI.
                                       " auf db-tabelle kopieren
  D_VARI[] = H_VARI[].
ENDFORM.