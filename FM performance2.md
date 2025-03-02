FUNCTION zcon_fm_get_data_main.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IR_BUKRS) TYPE  /IWBEP/T_COD_SELECT_OPTIONS
*"     REFERENCE(IR_FICTR) TYPE  /IWBEP/T_COD_SELECT_OPTIONS
*"     REFERENCE(IR_COMIT) TYPE  /IWBEP/T_COD_SELECT_OPTIONS OPTIONAL
*"     REFERENCE(IR_GJAHR) TYPE  /IWBEP/T_COD_SELECT_OPTIONS
*"     REFERENCE(IR_STAGE) TYPE  /IWBEP/T_COD_SELECT_OPTIONS OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_HEAD) TYPE  ZFM_SUM_RPT_HEAD_T
*"     REFERENCE(ET_RESTYPE) TYPE  ZTFM_BUDGET_T
*"     REFERENCE(ET_RESTYPE_PI) TYPE  ZTFM_BUDGET_PI_T
*"     REFERENCE(ET_COMMIT) TYPE  ZFM_COMMIT_OUT_T
*"     REFERENCE(ET_COMMIT_PI) TYPE  ZFM_COMMIT_OUT_T
*"     REFERENCE(ET_ACTUAL) TYPE  ZFM_ACTUAL_OUT_T
*"     REFERENCE(ET_ACTUAL_PI) TYPE  ZFM_ACTUAL_OUT_T
*"     REFERENCE(ET_BUDGET_REL) TYPE  ZFM_BGTDOC_OUT_T
*"     REFERENCE(ET_BUDGET_REL_PI) TYPE  ZFM_BGTDOC_OUT_T
*"     REFERENCE(ET_BUDGET_TR) TYPE  ZFM_BGTDOC_OUT_T
*"     REFERENCE(ET_BUDGET_TR_PI) TYPE  ZFM_BGTDOC_OUT_T
*"     REFERENCE(ET_PLANCOST) TYPE  ZFM_PLANCOST_OUT_CPM_T
*"     REFERENCE(ET_PLANCOST_PI) TYPE  ZFM_PLANCOST_OUT_CPM_T
*"----------------------------------------------------------------------
  DATA: ls_input TYPE ztfm_cond01_s,
        lv_from  TYPE sy-tabix,
        ls_wbs   TYPE zfms_wbs_stage,
        lt_wbs   TYPE TABLE OF zfms_wbs_stage.

  ls_input-bukrs    = CORRESPONDING #( ir_bukrs[] ).
  ls_input-fictr    = CORRESPONDING #( ir_fictr[] ).
  ls_input-cmmtitem = CORRESPONDING #( ir_comit[] ).
  ls_input-gjahr    = CORRESPONDING #( ir_gjahr[] ).
  ls_input-stage    = CORRESPONDING #( ir_stage[] ).

  READ TABLE ir_bukrs INTO DATA(lr_bukrs) INDEX 1.

  " begin of Leon
  LOOP AT ir_fictr ASSIGNING FIELD-SYMBOL(<fsr_fictr>).
    APPEND INITIAL LINE TO lt_wbs ASSIGNING FIELD-SYMBOL(<fs_wbs>).
    <fs_wbs>-fictr = <fsr_fictr>-low.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
      EXPORTING
        input     = <fs_wbs>-fictr
      IMPORTING
        output    = <fs_wbs>-posnr
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
  ENDLOOP.
  SELECT i~*
    FROM prhi AS i INNER JOIN @lt_wbs AS t ON i~posnr = t~posnr
    INTO TABLE @DATA(lt_prhi).
  IF sy-subrc = 0.
    SELECT s~*
      FROM prps AS s INNER JOIN @lt_prhi AS i ON s~pspnr = i~up
      INTO TABLE @DATA(lt_prps).
    IF sy-subrc = 0.
      SELECT m~*
        FROM /cpd/d_mp_item AS m INNER JOIN @lt_prps AS t ON m~mp_item_okey = t~objnr
       WHERE m~mp_itm_otyp = '0WBS'
        INTO TABLE @DATA(lt_item).
      IF sy-subrc = 0.
        SELECT h~*
          FROM /cpd/d_mp_hdr AS h INNER JOIN @lt_item AS t ON h~db_key = t~parent_key
         WHERE h~mp_stage IN @ir_stage
          INTO TABLE @DATA(lt_hdr).
      ENDIF.
    ENDIF.
  ENDIF.
  SORT lt_wbs  BY fictr.
  SORT lt_prhi BY posnr.
  SORT lt_prps BY pspnr.
  SORT lt_item BY mp_item_okey.
  SORT lt_hdr BY db_key.
  LOOP AT ls_input-fictr ASSIGNING FIELD-SYMBOL(<fs_fictr>).
    DATA(lv_tabix) = sy-tabix.
    READ TABLE lt_wbs ASSIGNING <fs_wbs> WITH KEY fictr = <fs_fictr>-low BINARY SEARCH.
    DATA(lv_index) = sy-tabix.
    IF sy-subrc = 0.
      READ TABLE lt_prhi ASSIGNING FIELD-SYMBOL(<fs_prhi>) WITH KEY posnr = <fs_wbs>-posnr BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE lt_prps ASSIGNING FIELD-SYMBOL(<fs_prps>) WITH KEY pspnr = <fs_prhi>-up.
        IF sy-subrc = 0.
          READ TABLE lt_item ASSIGNING FIELD-SYMBOL(<fs_item>) WITH KEY mp_item_okey = <fs_prps>-objnr.
          IF sy-subrc = 0 .
            READ TABLE lt_hdr ASSIGNING FIELD-SYMBOL(<fs_hdr>) WITH KEY db_key = <fs_item>-parent_key.
            IF sy-subrc = 0.
              <fs_wbs>-mp_stage = <fs_hdr>-mp_stage.
            ELSE.
              DELETE ls_input-fictr INDEX lv_tabix.
              DELETE lt_wbs INDEX lv_index.
            ENDIF.
          ELSE.
            DELETE ls_input-fictr INDEX lv_tabix.
            DELETE lt_wbs INDEX lv_index.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF ls_input-fictr IS INITIAL.
    RETURN.
  ENDIF.
  " end of Leon

  CALL FUNCTION 'ZFM_GET_BUDGET_MAIN'
    EXPORTING
      is_input         = ls_input
      it_wbs           = lt_wbs
    IMPORTING
      et_head          = et_head
      et_restype       = et_restype
      et_restype_pi    = et_restype_pi
      et_commit        = et_commit
      et_commit_pi     = et_commit_pi
      et_actual        = et_actual
      et_actual_pi     = et_actual_pi
      et_budget_rel    = et_budget_rel
      et_budget_rel_pi = et_budget_rel_pi
      et_budget_tr     = et_budget_tr
      et_budget_tr_pi  = et_budget_tr_pi
      et_plancost      = et_plancost
      et_plancost_pi   = et_plancost_pi.

  SORT et_plancost_pi ASCENDING BY gjahr     fictr    cmmtitem pi.
  SORT et_commit      ASCENDING BY zrpt_year fistl    fipex    pi.
  SORT et_budget_rel  ASCENDING BY fiscyear  fundsctr cmmtitem pi.
  SORT et_budget_tr   ASCENDING BY fiscyear  fundsctr cmmtitem pi.
  SORT et_actual_pi   ASCENDING BY zrpt_year fistl    fipex    pi.

  SELECT *
    FROM zconv_fm_pi_view
    INTO TABLE @DATA(lt_view)
   WHERE gjahr IN @ir_gjahr.

  et_restype_pi = VALUE ztfm_budget_pi_t(
      FOR ls_head IN et_head
      FOR ls_view IN lt_view WHERE ( gjahr = ls_head-gjahr )
      ( gjahr      = ls_head-gjahr
        fictr      = ls_head-fictr
        cmmtitem   = ls_head-cmmtitem
        pi         = ls_view-pi
        zdatefrom  = ls_view-zdatefrom
        zdateto    = ls_view-zdateto )
  ).
  SORT et_restype_pi BY gjahr fictr cmmtitem pi.
  "plancost
  SELECT gjahr, fictr, cmmtitem, pi, SUM( plancost ) AS plancost
    FROM @et_plancost_pi AS _plancost_out
    WHERE fictr IS NOT INITIAL
    GROUP BY gjahr, fictr, cmmtitem, pi
    INTO TABLE @DATA(lt_data_plancost_out)
    .
  IF sy-subrc = 0.
    SORT lt_data_plancost_out BY gjahr fictr cmmtitem pi.
  ENDIF.
  LOOP AT lt_data_plancost_out ASSIGNING FIELD-SYMBOL(<fs_plan>).
    READ TABLE et_restype_pi ASSIGNING FIELD-SYMBOL(<fs_res_pi>) WITH KEY gjahr = <fs_plan>-gjahr
    fictr = <fs_plan>-fictr cmmtitem = <fs_plan>-cmmtitem pi = <fs_plan>-pi BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_res_pi>-plancost = <fs_plan>-plancost.
    ENDIF.
  ENDLOOP.
  CLEAR lt_data_plancost_out.
  "commitment
  SELECT zrpt_year, fistl, fipex, pi,frgrl,refbt, SUM( fkbtr ) AS fkbtr
    FROM @et_commit AS _com_out
    WHERE fistl IS NOT INITIAL
    GROUP BY zrpt_year, fistl, fipex, pi,frgrl,refbt
    INTO TABLE @DATA(lt_data_com_out)
    .
  IF sy-subrc = 0.
    SORT lt_data_com_out BY zrpt_year fistl fipex pi.
  ENDIF.
  LOOP AT lt_data_com_out ASSIGNING FIELD-SYMBOL(<fs_com>).
    READ TABLE et_restype_pi ASSIGNING <fs_res_pi> WITH KEY gjahr = <fs_com>-zrpt_year
    fictr = <fs_com>-fistl cmmtitem = <fs_com>-fipex pi = <fs_com>-pi BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_res_pi>-commit = <fs_res_pi>-commit + <fs_com>-fkbtr.
      IF <fs_com>-frgrl = '' AND <fs_com>-refbt = '020'.
        <fs_res_pi>-zpo_apv_amt = <fs_res_pi>-zpo_apv_amt + <fs_com>-fkbtr.
      ENDIF.
    ENDIF.
  ENDLOOP.
  CLEAR lt_data_com_out.
  "actual
  SELECT zrpt_year, fistl, fipex, pi, SUM( dmbtr ) AS dmbtr
    FROM @et_actual_pi AS _act_out
    WHERE fistl IS NOT INITIAL
    GROUP BY zrpt_year, fistl, fipex, pi
    INTO TABLE @DATA(lt_data_act_out)
    .
  IF sy-subrc = 0.
    SORT lt_data_act_out BY zrpt_year fistl fipex pi.
  ENDIF.
  LOOP AT lt_data_act_out ASSIGNING FIELD-SYMBOL(<fs_act>).
    READ TABLE et_restype_pi ASSIGNING <fs_res_pi> WITH KEY gjahr = <fs_act>-zrpt_year
    fictr = <fs_act>-fistl cmmtitem = <fs_act>-fipex pi = <fs_act>-pi BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_res_pi>-actual = <fs_res_pi>-actual + <fs_act>-dmbtr.
    ENDIF.
  ENDLOOP.
  CLEAR lt_data_act_out.
  "budget release
  SELECT fiscyear, fundsctr, cmmtitem,pi, SUM( tvalx ) AS tvalx
    FROM @et_budget_rel AS _budget_rel
    WHERE fundsctr IS NOT INITIAL
    GROUP BY fiscyear, fundsctr, cmmtitem, pi
    INTO TABLE @DATA(lt_data_budget_rel)
    .
  IF sy-subrc = 0.
    SORT lt_data_budget_rel BY fiscyear fundsctr cmmtitem pi.
  ENDIF.
  LOOP AT lt_data_budget_rel ASSIGNING FIELD-SYMBOL(<fs_rel>).
    READ TABLE et_restype_pi ASSIGNING <fs_res_pi> WITH KEY gjahr = <fs_rel>-fiscyear
    fictr = <fs_rel>-fundsctr cmmtitem = <fs_rel>-cmmtitem pi = <fs_rel>-pi BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_res_pi>-budget_rel = <fs_res_pi>-budget_rel + <fs_rel>-tvalx.
    ENDIF.
  ENDLOOP.
  CLEAR lt_data_budget_rel.
  "budget transfer
  SELECT fiscyear, fundsctr, cmmtitem,pi, SUM( tvalx ) AS tvalx
    FROM @et_budget_tr AS _budget_tr
    WHERE fundsctr IS NOT INITIAL
    GROUP BY fiscyear, fundsctr, cmmtitem, pi
    INTO TABLE @DATA(lt_data_budget_tr)
    .
  IF sy-subrc = 0.
    SORT lt_data_budget_tr BY fiscyear fundsctr cmmtitem pi.
  ENDIF.
  LOOP AT lt_data_budget_tr ASSIGNING FIELD-SYMBOL(<fs_tr>).
    READ TABLE et_restype_pi ASSIGNING <fs_res_pi> WITH KEY gjahr = <fs_tr>-fiscyear
    fictr = <fs_tr>-fundsctr cmmtitem = <fs_tr>-cmmtitem pi = <fs_tr>-pi BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_res_pi>-budget_tr = <fs_res_pi>-budget_tr + <fs_tr>-tvalx.
    ENDIF.
  ENDLOOP.
  CLEAR lt_data_budget_tr.
ENDFUNCTION.
