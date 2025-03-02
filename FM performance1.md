FUNCTION zfm_get_budget_main.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_INPUT) TYPE  ZTFM_COND01_S
*"     REFERENCE(IT_WBS) TYPE  ZFMT_WBS_STAGE
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
  DATA: ls_act_in          TYPE zfm_actual_in_s,
        lt_act_out         TYPE zfm_actual_out_t,
        lt_act_out_pi      TYPE zfm_actual_out_t,
        ls_com_in          TYPE zfm_commit_in_s,
        lt_com_out         TYPE zfm_commit_out_t,
        lt_com_out_pi      TYPE zfm_commit_out_t,
        ls_budget_in       TYPE zfm_bgtdoc_in_s,
        lt_budget_tr       TYPE zfm_bgtdoc_out_t,
        lt_budget_tr_pi    TYPE zfm_bgtdoc_out_t,
        lt_budget_rel      TYPE zfm_bgtdoc_out_t,
        lt_budget_rel_pi   TYPE zfm_bgtdoc_out_t,
        ls_plancost_in     TYPE zfm_plancost_cond_s,
        lt_plancost_out    TYPE zfm_plancost_out_cpm_t,
        lt_plancost_out_pi TYPE zfm_plancost_out_cpm_t,
        lv_from            TYPE sy-tabix,
        lv_gjahr           TYPE gjahr,
        lt_gjahr           TYPE bspl_gjahr_t,
        lt_prte            TYPE tt_prte,
        lt_data            TYPE ztfm_budget_t,
        lv_actual          TYPE wrbtr,
        lv_budget_rel      TYPE wrbtr,
        lv_budget_tr       TYPE wrbtr,
        lv_commit          TYPE wrbtr,
        lv_plancost        TYPE wrbtr,
        lt_head            TYPE zfm_sum_rpt_head_t,
        lt_restype         TYPE ztfm_budget_t,
        lr_posnr           TYPE RANGE OF ps_posnr,
        lv_bezeich         TYPE bezeich,
        lv_post1           TYPE ps_post1,
        lv_bezei           TYPE fm_bezeich,
        lv_monat           TYPE monat VALUE '00'.


  " get the company code
  DATA(lv_bukrs) = is_input-bukrs[ 1 ]-low.

  " get the FM area based on the company code
  SELECT SINGLE fikrs
    FROM t001
    INTO @DATA(lv_fikrs)
   WHERE bukrs = @lv_bukrs.

  SELECT SINGLE kokrs
    FROM csks
    INTO @DATA(lv_kokrs)
   WHERE bukrs = @lv_bukrs.

  SELECT *
    FROM zconv_fm_res_typ
    INTO TABLE @DATA(lt_0001)
   WHERE kokrs = @lv_kokrs.

  " parse the fiscal year
  IF NOT is_input-gjahr IS INITIAL.
    LOOP AT is_input-gjahr ASSIGNING FIELD-SYMBOL(<flr_gjahr>).
      IF <flr_gjahr>-high = '0000'.
        APPEND <flr_gjahr>-low TO lt_gjahr.
      ELSE.
        lv_gjahr = <flr_gjahr>-low.
        DO .
          APPEND lv_gjahr TO lt_gjahr.
          lv_gjahr = lv_gjahr + 1.
          IF lv_gjahr > <flr_gjahr>-high.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDLOOP.
    SORT lt_gjahr.
    DELETE ADJACENT DUPLICATES FROM lt_gjahr.
  ENDIF.

  " Budget Transfer Drill Down
  CLEAR: ls_budget_in.
  ls_budget_in-fikrs    = VALUE #( ( sign = 'I' option = 'EQ' low = lv_fikrs ) ).
  ls_budget_in-fictr    = is_input-fictr.
  ls_budget_in-cmmtitem = is_input-cmmtitem.
  ls_budget_in-gjahr    = is_input-gjahr.
  ls_budget_in-valtype  = VALUE #( ( sign = 'I' option = 'EQ' low = 'B1' ) ).
  CALL FUNCTION 'ZFM_GET_BUDGET_DETAIL'
    EXPORTING
      is_input  = ls_budget_in
    IMPORTING
      et_output = lt_budget_tr.
  SORT lt_budget_tr ASCENDING BY fm_area fiscyear fundsctr cmmtitem.

  " Budget Release Drill Down
  ls_budget_in-valtype  = VALUE #( ( sign = 'I' option = 'EQ' low = 'R1' ) ).
  CALL FUNCTION 'ZFM_GET_BUDGET_DETAIL'
    EXPORTING
      is_input  = ls_budget_in
    IMPORTING
      et_output = lt_budget_rel.
  SORT lt_budget_rel ASCENDING BY fm_area fiscyear fundsctr cmmtitem.

  " Actual Drill Down
  CLEAR:ls_act_in.
  ls_act_in-fikrs    = VALUE #( ( sign = 'I' option = 'EQ' low = lv_fikrs ) ).
  ls_act_in-fictr    = is_input-fictr.
  ls_act_in-cmmtitem = is_input-cmmtitem.
  ls_act_in-gjahr    = is_input-gjahr.
  CALL FUNCTION 'ZFM_DERIVE_ACTUAL'
    EXPORTING
      is_input     = ls_act_in
    IMPORTING
      et_output    = lt_act_out
      et_output_pi = lt_act_out_pi.
  SORT lt_act_out ASCENDING BY zrpt_year fistl fipex zrpt_monat.

  " Commitment Drill Down
  ls_com_in-fikrs    = VALUE #( ( sign = 'I' option = 'EQ' low = lv_fikrs ) ).
  ls_com_in-fictr    = is_input-fictr.
  ls_com_in-cmmtitem = is_input-cmmtitem.
  ls_com_in-gjahr    = is_input-gjahr.
  CALL FUNCTION 'ZFM_DERIVE_COMMIT'
    EXPORTING
      is_input     = ls_com_in
    IMPORTING
      et_output    = lt_com_out
      et_output_pi = lt_com_out_pi.
  SORT lt_com_out ASCENDING BY zrpt_year fistl fipex zrpt_monat.

  " get WBS no amount
  CALL FUNCTION 'ZFM_GET_WBS_NO_AMT'
    EXPORTING
      is_input = is_input
      it_gjahr = lt_gjahr
      it_wbs   = it_wbs
    CHANGING
      ct_prte  = lt_prte
      ct_data  = lt_data.


  CALL FUNCTION 'ZFM_PLANCOST'
    EXPORTING
      is_input       = is_input
      it_gjahr       = lt_gjahr
      it_data        = lt_data
    CHANGING
      ct_plancost    = lt_plancost_out
      ct_plancost_pi = lt_plancost_out_pi.
  SORT lt_plancost_out ASCENDING BY gjahr fictr cmmtitem monat.
  " merge the data
  SORT lt_data ASCENDING BY gjahr fictr cmmtitem.
  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    " fill the header
    APPEND INITIAL LINE TO lt_head ASSIGNING FIELD-SYMBOL(<fs_head>).
    <fs_head>-gjahr        = <fs_data>-gjahr.
    <fs_head>-fictr        = <fs_data>-fictr.
    <fs_head>-bezeich      = <fs_data>-bezeich.
    <fs_head>-cmmtitem     = <fs_data>-cmmtitem.
    <fs_head>-cmmtitem_txt = <fs_data>-cmmtitem_txt.
    <fs_head>-psphi        = <fs_data>-psphi.
    <fs_head>-post1        = <fs_data>-post1.
    <fs_head>-pstrt        = <fs_data>-pstrt.
    <fs_head>-pende        = <fs_data>-pende.
    <fs_head>-display      = <fs_data>-display.
    " drill down monthly view
    CLEAR: lv_monat.
    DO 12 TIMES.
      lv_monat = lv_monat + 1.
      APPEND INITIAL LINE TO lt_restype ASSIGNING FIELD-SYMBOL(<fs_restype>).
      <fs_restype>-gjahr        = <fs_data>-gjahr.
      <fs_restype>-fictr        = <fs_data>-fictr.
      <fs_restype>-bezeich      = <fs_data>-bezeich.
      <fs_restype>-cmmtitem     = <fs_data>-cmmtitem.
      <fs_restype>-cmmtitem_txt = <fs_data>-cmmtitem_txt.
      <fs_restype>-psphi        = <fs_data>-psphi.
      <fs_restype>-post1        = <fs_data>-post1.
      <fs_restype>-monat        = lv_monat.
    ENDDO.
  ENDLOOP.
  SORT lt_head BY  gjahr fictr cmmtitem.
  SORT lt_restype BY gjahr fictr cmmtitem monat.
  "budget transfer
  SELECT fiscyear, fundsctr, cmmtitem,rpmax, SUM( tvalx ) AS tvalx
    FROM @lt_budget_tr AS _budget_tr
    WHERE fundsctr IS NOT INITIAL
    AND fm_area = @lv_fikrs
    GROUP BY fiscyear, fundsctr, cmmtitem, rpmax
    INTO TABLE @DATA(lt_data_budget_tr)
    .
  IF sy-subrc = 0.
    SORT lt_data_budget_tr BY fiscyear fundsctr cmmtitem rpmax.
  ENDIF.
  LOOP AT lt_data_budget_tr ASSIGNING FIELD-SYMBOL(<fs_tr>).
    READ TABLE lt_head ASSIGNING <fs_head> WITH KEY gjahr = <fs_tr>-fiscyear
    fictr = <fs_tr>-fundsctr cmmtitem = <fs_tr>-cmmtitem BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_head>-budget_tr = <fs_head>-budget_tr + <fs_tr>-tvalx.
    ENDIF.
    DATA(lv_rpmax) = <fs_tr>-rpmax.
    IF <fs_tr>-rpmax > 12.
      lv_rpmax = 12.
    ENDIF.
    READ TABLE lt_restype ASSIGNING <fs_restype> WITH KEY gjahr = <fs_tr>-fiscyear
    fictr = <fs_tr>-fundsctr cmmtitem = <fs_tr>-cmmtitem monat = lv_rpmax BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_restype>-budget_tr = <fs_restype>-budget_tr + <fs_tr>-tvalx.
    ENDIF.
  ENDLOOP.
  CLEAR lt_data_budget_tr.
  "budget release
  SELECT fiscyear, fundsctr, cmmtitem,rpmax, SUM( tvalx ) AS tvalx
    FROM @lt_budget_rel AS _budget_rel
    WHERE fundsctr IS NOT INITIAL
    AND fm_area = @lv_fikrs
    GROUP BY fiscyear, fundsctr, cmmtitem, rpmax
    INTO TABLE @DATA(lt_data_budget_rel)
    .
  IF sy-subrc = 0.
    SORT lt_data_budget_rel BY fiscyear fundsctr cmmtitem rpmax.
  ENDIF.
  LOOP AT lt_data_budget_rel ASSIGNING FIELD-SYMBOL(<fs_rel>).
    READ TABLE lt_head ASSIGNING <fs_head> WITH KEY gjahr = <fs_rel>-fiscyear
    fictr = <fs_rel>-fundsctr cmmtitem = <fs_rel>-cmmtitem BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_head>-budget_rel = <fs_head>-budget_rel + <fs_rel>-tvalx.
    ENDIF.
    lv_rpmax = <fs_rel>-rpmax.
    IF <fs_rel>-rpmax > 12.
      lv_rpmax = 12.
    ENDIF.
    READ TABLE lt_restype ASSIGNING <fs_restype> WITH KEY gjahr = <fs_rel>-fiscyear
    fictr = <fs_rel>-fundsctr cmmtitem = <fs_rel>-cmmtitem monat = lv_rpmax BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_restype>-budget_rel = <fs_restype>-budget_rel + <fs_rel>-tvalx.
    ENDIF.
  ENDLOOP.
  CLEAR lt_data_budget_rel.
  "plancost
  SELECT gjahr, fictr, cmmtitem, monat, SUM( plancost ) AS plancost, SUM( zplan_qty ) AS zplan_qty
    FROM @lt_plancost_out AS _plancost_out
    WHERE fictr IS NOT INITIAL
    GROUP BY gjahr, fictr, cmmtitem, monat
    INTO TABLE @DATA(lt_data_plancost_out)
    .
  IF sy-subrc = 0.
    SORT lt_data_plancost_out BY gjahr fictr cmmtitem monat.
  ENDIF.
  LOOP AT lt_data_plancost_out ASSIGNING FIELD-SYMBOL(<fs_plan>).
    READ TABLE lt_head ASSIGNING <fs_head> WITH KEY gjahr = <fs_plan>-gjahr
    fictr = <fs_plan>-fictr cmmtitem = <fs_plan>-cmmtitem BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_head>-plancost = <fs_head>-plancost + <fs_plan>-plancost.
    ENDIF.

    READ TABLE lt_restype ASSIGNING <fs_restype> WITH KEY gjahr = <fs_plan>-gjahr
    fictr = <fs_plan>-fictr cmmtitem = <fs_plan>-cmmtitem monat = <fs_plan>-monat BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_restype>-plancost =  <fs_plan>-plancost.
      <fs_restype>-zplan_qty = <fs_plan>-zplan_qty.
    ENDIF.
  ENDLOOP.
  CLEAR lt_data_plancost_out.
  "actual
  SELECT zrpt_year, fistl, fipex, zrpt_monat, SUM( dmbtr ) AS dmbtr, SUM( zact_qty ) AS zact_qty
    FROM @lt_act_out AS _act_out
    WHERE fistl IS NOT INITIAL
    GROUP BY zrpt_year, fistl, fipex, zrpt_monat
    INTO TABLE @DATA(lt_data_act_out)
    .
  IF sy-subrc = 0.
    SORT lt_data_act_out BY zrpt_year fistl fipex zrpt_monat.
  ENDIF.
  LOOP AT lt_data_act_out ASSIGNING FIELD-SYMBOL(<fs_act>).
    READ TABLE lt_head ASSIGNING <fs_head> WITH KEY gjahr = <fs_act>-zrpt_year
    fictr = <fs_act>-fistl cmmtitem = <fs_act>-fipex BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_head>-actual = <fs_head>-actual + <fs_act>-dmbtr.
    ENDIF.

    READ TABLE lt_restype ASSIGNING <fs_restype> WITH KEY gjahr = <fs_act>-zrpt_year
    fictr = <fs_act>-fistl cmmtitem = <fs_act>-fipex monat = <fs_act>-zrpt_monat BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_restype>-actual =  <fs_act>-dmbtr.
      <fs_restype>-zact_qty =  <fs_act>-zact_qty.
    ENDIF.
  ENDLOOP.
  CLEAR lt_data_act_out.
  "commitment
  SELECT zrpt_year, fistl, fipex, zrpt_monat,frgrl,refbt, SUM( fkbtr ) AS fkbtr
    FROM @lt_com_out AS _com_out
    WHERE fistl IS NOT INITIAL
    GROUP BY zrpt_year, fistl, fipex, zrpt_monat,frgrl,refbt
    INTO TABLE @DATA(lt_data_com_out)
    .
  IF sy-subrc = 0.
    SORT lt_data_com_out BY zrpt_year fistl fipex zrpt_monat.
  ENDIF.
  LOOP AT lt_data_com_out ASSIGNING FIELD-SYMBOL(<fs_com>).
    READ TABLE lt_head ASSIGNING <fs_head> WITH KEY gjahr = <fs_com>-zrpt_year
    fictr = <fs_com>-fistl cmmtitem = <fs_com>-fipex BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_head>-commit = <fs_head>-commit + <fs_com>-fkbtr.
      IF <fs_com>-frgrl = '' AND <fs_com>-refbt = '020'.
        <fs_head>-zpo_apv_amt = <fs_head>-zpo_apv_amt + <fs_com>-fkbtr.
      ENDIF.
    ENDIF.

    READ TABLE lt_restype ASSIGNING <fs_restype> WITH KEY gjahr = <fs_com>-zrpt_year
    fictr = <fs_com>-fistl cmmtitem = <fs_com>-fipex monat = <fs_com>-zrpt_monat BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_restype>-commit = <fs_restype>-commit + <fs_com>-fkbtr.
      IF <fs_com>-frgrl = '' AND <fs_com>-refbt = '020'.
        <fs_restype>-zpo_apv_amt = <fs_restype>-zpo_apv_amt + <fs_com>-fkbtr.
      ENDIF.
    ENDIF.
  ENDLOOP.
  CLEAR lt_data_com_out.

  LOOP AT lt_head ASSIGNING <fs_head>.
    <fs_head>-actual_sum  = <fs_head>-actual     + <fs_head>-commit.
    <fs_head>-available   = <fs_head>-budget_rel - <fs_head>-actual - <fs_head>-commit.
    <fs_head>-ava_rdc     = <fs_head>-budget_rel - <fs_head>-actual_sum.
    IF <fs_head>-plancost IS NOT INITIAL.
      <fs_head>-trans_aft = <fs_head>-trans_bgt + <fs_head>-budget_tr.
    ENDIF.
    <fs_head>-rel_aft = <fs_head>-rel_bgt + <fs_head>-budget_rel.
  ENDLOOP  .

  et_head          = lt_head.
  et_restype       = lt_restype.
  et_plancost      = lt_plancost_out.
  et_plancost_pi   = lt_plancost_out_pi.
  et_actual        = lt_act_out.
  et_actual_pi     = lt_act_out_pi.
  et_commit        = lt_com_out.
  et_commit_pi     = lt_com_out_pi.
  et_budget_rel    = lt_budget_rel.
  et_budget_rel_pi = lt_budget_rel_pi.
  et_budget_tr     = lt_budget_tr.
  et_budget_tr_pi  = lt_budget_tr_pi.

ENDFUNCTION.
