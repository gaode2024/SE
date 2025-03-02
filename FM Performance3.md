  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
**  EXPORTING
**    iv_entity_name           =
**    iv_entity_set_name       =
**    iv_source_name           =
**    it_filter_select_options =
**    it_order                 =
**    is_paging                =
**    it_navigation_path       =
**    it_key_tab               =
**    iv_filter_string         =
**    iv_search_string         =
**    io_expand                =
**    io_tech_request_context  =
**  IMPORTING
**    er_entityset             =
**    et_expanded_clauses      =
**    et_expanded_tech_clauses =
**    es_response_context      =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.
* delete
*    DATA:
*      lv_start_time TYPE timestampl,
*      lv_end_time   TYPE timestampl,
*      lv_diff       TYPE timestampl.
*    GET TIME STAMP FIELD lv_start_time.
* delete

    TYPES: ty_t_itemset     TYPE TABLE OF zcl_zcon_odata_fm_fm_s_mpc=>ts_items     WITH DEFAULT KEY,
           ty_t_plancostset TYPE TABLE OF zcl_zcon_odata_fm_fm_s_mpc=>ts_plancost  WITH DEFAULT KEY,
           ty_t_budget      TYPE TABLE OF zcl_zcon_odata_fm_fm_s_mpc=>ts_budget_tr WITH DEFAULT KEY,
           ty_t_actual      TYPE TABLE OF zcl_zcon_odata_fm_fm_s_mpc=>ts_actual    WITH DEFAULT KEY,
           ty_t_pi          TYPE TABLE OF zcl_zcon_odata_fm_fm_s_mpc=>ts_piview    WITH DEFAULT KEY,
           ty_t_commit      TYPE TABLE OF zcl_zcon_odata_fm_fm_s_mpc=>ts_commit    WITH DEFAULT KEY.
    TYPES: BEGIN OF ty_entity.
             INCLUDE TYPE zcl_zcon_odata_fm_fm_s_mpc=>ts_zcont_s_fm_sumrpt.
    TYPES:   itemsset      TYPE ty_t_itemset,
             plancostset   TYPE ty_t_plancostset,
             plancostpiset TYPE ty_t_plancostset,
             budget_trset  TYPE ty_t_budget,
             budget_relset TYPE ty_t_budget,
             actualset     TYPE ty_t_actual,
             actualpiset   TYPE ty_t_actual,
             commitset     TYPE ty_t_commit,
             piviewset     TYPE ty_t_pi,
           END OF ty_entity.

    DATA: lr_posid           TYPE /iwbep/t_cod_select_options,
          lr_kostl           TYPE /iwbep/t_cod_select_options,
          lr_bukrs           TYPE /iwbep/t_cod_select_options,
          lr_pspid           TYPE /iwbep/t_cod_select_options,
          lr_fictr           TYPE /iwbep/t_cod_select_options,
          lr_gjahr           TYPE /iwbep/t_cod_select_options,
          lr_comit           TYPE /iwbep/t_cod_select_options,
          lr_fipos           TYPE /iwbep/t_cod_select_options,
          lr_khinr           TYPE /iwbep/t_cod_select_options,
          lr_ddplan          TYPE /iwbep/t_cod_select_options,
          lr_zzplanfield     TYPE /iwbep/t_cod_select_options,
          lr_zzsubplanfield  TYPE /iwbep/t_cod_select_options,
          lr_zztechcenter    TYPE /iwbep/t_cod_select_options,
          lr_ztargetstate    TYPE /iwbep/t_cod_select_options,
          lr_zzsubsystem     TYPE /iwbep/t_cod_select_options,
          lr_zzextcooper     TYPE /iwbep/t_cod_select_options,
          lr_zzcooperdetail  TYPE /iwbep/t_cod_select_options,
          lr_vernr           TYPE /iwbep/t_cod_select_options,
          lr_parnr           TYPE /iwbep/t_cod_select_options,
          lr_stat            TYPE /iwbep/t_cod_select_options,
          lr_stage           TYPE /iwbep/t_cod_select_options,
          lrs_ddplan         LIKE LINE OF lr_ddplan,
          lr_budget_tr       TYPE /iwbep/t_cod_select_options,
          lrs_budget_tr      LIKE LINE OF lr_budget_tr,
          lr_budget_rel      TYPE /iwbep/t_cod_select_options,
          lrs_budget_rel     LIKE LINE OF lr_budget_rel,
          lr_actual          TYPE /iwbep/t_cod_select_options,
          lrs_actual         LIKE LINE OF lr_budget_rel,
          lr_commit          TYPE /iwbep/t_cod_select_options,
          lrs_commit         LIKE LINE OF lr_budget_rel,
*            ls_entity    TYPE zcl_zcon_odata_fm_fm_s_mpc=>ts_zcont_s_fm_sumrpt,
          ls_entity          TYPE ty_entity,
*            lt_entity    TYPE zcl_zcon_odata_fm_fm_s_mpc=>tt_zcont_s_fm_sumrpt,
          lt_entity          TYPE TABLE OF ty_entity,
          lt_sort            TYPE abap_sortorder_tab,
          lv_field           TYPE string,
          lv_no_result       TYPE boolean,
          lrs_bukrs          LIKE LINE OF lr_bukrs,
          lt_secid           TYPE ztfm_secid_t,
          lt_item            TYPE ztfm_budget_t,
          lt_pi              TYPE ztfm_budget_pi_t,
          lt_item_tmp        TYPE ztfm_budget_t,
          lv_monat           TYPE numc2,
          lv_bukrs           TYPE bukrs,
          lv_pspid_in        TYPE ps_pspid,
          lv_pspid_out       TYPE ps_pspid,
          ls_act_in          TYPE zfm_actual_in_s,
          lt_act_out         TYPE zfm_actual_out_t,
          lt_act_out_pi      TYPE zfm_actual_out_t,
          ls_com_in          TYPE zfm_commit_in_s,
          lt_com_out         TYPE zfm_commit_out_t,
          ls_budget_in       TYPE zfm_bgtdoc_in_s,
          lt_budget_out      TYPE zfm_bgtdoc_out_t,
          lt_budget_out_pi   TYPE zfm_bgtdoc_out_t,
          ls_plancost_in     TYPE zfm_plancost_cond_s,
          lt_plancost_out    TYPE zfm_plancost_out_cpm_t,
          lt_plancost_out_pi TYPE zfm_plancost_out_cpm_t,
          lt_restype         TYPE ztfm_budget_t,
          lt_restype_pi      TYPE ztfm_budget_pi_t,
          lt_commit          TYPE zfm_commit_out_t,
          lt_commit_pi       TYPE zfm_commit_out_t,
          lt_actual          TYPE zfm_actual_out_t,
          lt_actual_pi       TYPE zfm_actual_out_t,
          lt_bgt_tr          TYPE zfm_bgtdoc_out_t,
          lt_bgt_tr_pi       TYPE zfm_bgtdoc_out_t,
          lt_bgt_rel         TYPE zfm_bgtdoc_out_t,
          lt_bgt_rel_pi      TYPE zfm_bgtdoc_out_t,
          lt_plancost        TYPE zfm_plancost_out_cpm_t,
          lt_plancost_pi     TYPE zfm_plancost_out_cpm_t,
          lt_head            TYPE zfm_sum_rpt_head_t.

    FIELD-SYMBOLS: <fs_tmp> TYPE ztfm_budget,
                   <fs_set> TYPE ty_entity.

    IF NOT iv_entity_set_name  = 'ZCONT_S_FM_SUMRPTSet'.
      RETURN.
    ENDIF.

    DATA lo_filter TYPE  REF TO /iwbep/if_mgw_req_filter.
    DATA lt_filter_select_options TYPE /iwbep/t_mgw_select_option.
    lo_filter = io_tech_request_context->get_filter( ).
    lt_filter_select_options = lo_filter->get_filter_select_options( ).

    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<fs_opt>).
      CASE <fs_opt>-property.
        WHEN 'Bukrs'.
          lr_bukrs = <fs_opt>-select_options.
          lrs_bukrs = lr_bukrs[ 1 ].
          lv_bukrs = lrs_bukrs-low.
        WHEN 'Posid'.
          lr_posid = <fs_opt>-select_options.
        WHEN 'Kostl'.
          lr_kostl = <fs_opt>-select_options.
        WHEN 'Pspid'.
          lr_pspid = <fs_opt>-select_options.
          LOOP AT lr_pspid ASSIGNING FIELD-SYMBOL(<fs_pspid>).
            lv_pspid_in =  <fs_pspid>-low.
            CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
              EXPORTING
                input  = lv_pspid_in
              IMPORTING
                output = lv_pspid_out.
            <fs_pspid>-low = lv_pspid_out.
          ENDLOOP.
        WHEN 'Fictr'.
          lr_fictr = <fs_opt>-select_options.
        WHEN 'Gjahr'.
          lr_gjahr = <fs_opt>-select_options.
        WHEN 'Fipos'.
          lr_fipos = <fs_opt>-select_options.
        WHEN 'Khinr'.
          lr_khinr = <fs_opt>-select_options.
        WHEN 'DdPlancost'.
          lr_ddplan = <fs_opt>-select_options.
        WHEN 'DdBudgetRel'.
          lr_budget_rel = <fs_opt>-select_options.
        WHEN 'DdBudgetTr'.
          lr_budget_tr = <fs_opt>-select_options.
        WHEN 'DdActual'.
          lr_actual = <fs_opt>-select_options.
        WHEN 'DdCommit'.
          lr_commit = <fs_opt>-select_options.
        WHEN 'ZzplanField'.
          lr_zzplanfield = <fs_opt>-select_options.
        WHEN 'ZzcooperDetail'.
          lr_zzcooperdetail = <fs_opt>-select_options.
        WHEN 'ZzsubPlanfield'.
          lr_zzsubplanfield = <fs_opt>-select_options.
        WHEN 'ZztechCenter'.
          lr_zztechcenter = <fs_opt>-select_options.
        WHEN 'ZzsubSystem'.
          lr_zzsubsystem = <fs_opt>-select_options.
        WHEN 'ZzextCooper'.
          lr_zzextcooper = <fs_opt>-select_options.
        WHEN 'Vernr'.
          lr_vernr = <fs_opt>-select_options.
        WHEN 'Parnr'.
          lr_parnr = <fs_opt>-select_options.
        WHEN 'Stat'.
          lr_stat = <fs_opt>-select_options.
        WHEN 'MpStage'.
          lr_stage = <fs_opt>-select_options.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'BUKRS' FIELD lv_bukrs
                                        ID 'ACTVT' FIELD '03'.
    IF sy-subrc <> 0.
      CONCATENATE 'No authority to company code ' lv_bukrs '. Please contact system administrator.' INTO ls_entity-ztext.
      APPEND ls_entity TO lt_entity.
      copy_data_to_ref( EXPORTING is_data = lt_entity CHANGING cr_data = er_entityset ).
      RETURN.
    ENDIF.


    " get second level WBS
    CALL FUNCTION 'ZCON_FM_GET_SECID'
      EXPORTING
        ir_posid          = lr_posid
        ir_kostl          = lr_kostl
        ir_bukrs          = lr_bukrs
        ir_pspid          = lr_pspid
        ir_khinr          = lr_khinr
        ir_zzplanfield    = lr_zzplanfield
        ir_zzsubplanfield = lr_zzsubplanfield
        ir_zztechcenter   = lr_zztechcenter
        ir_zzsubsystem    = lr_zzsubsystem
        ir_zzextcooper    = lr_zzextcooper
        ir_zzcooperdetail = lr_zzcooperdetail
        ir_vernr          = lr_vernr
        ir_parnr          = lr_parnr
      IMPORTING
        ev_no_result      = lv_no_result
      CHANGING
        c_t_secid         = lt_secid.
    IF lv_no_result = abap_true.
      RETURN.
    ENDIF.

    " get funder center
    CALL FUNCTION 'ZFM_GET_FUNDCENTER'
      EXPORTING
        it_secid     = lt_secid
        ir_bukrs     = lr_bukrs
        ir_fictr     = lr_fictr
      IMPORTING
        er_fictr     = lr_fictr
        ev_no_result = lv_no_result.
    IF lv_no_result = abap_true.
      RETURN.
    ENDIF.

    " get main data
    CALL FUNCTION 'ZCON_FM_GET_DATA_MAIN'
      EXPORTING
        ir_bukrs         = lr_bukrs
        ir_fictr         = lr_fictr
        ir_comit         = lr_fipos
        ir_gjahr         = lr_gjahr
        ir_stage         = lr_stage
      IMPORTING
        et_head          = lt_head
        et_restype       = lt_restype
        et_restype_pi    = lt_restype_pi
        et_commit        = lt_commit
        et_commit_pi     = lt_commit_pi
        et_actual        = lt_actual
        et_actual_pi     = lt_actual_pi
        et_budget_rel    = lt_bgt_rel
        et_budget_rel_pi = lt_bgt_rel_pi
        et_budget_tr     = lt_bgt_tr
        et_budget_tr_pi  = lt_bgt_tr_pi
        et_plancost      = lt_plancost
        et_plancost_pi   = lt_plancost_pi.

    SORT lt_actual      ASCENDING BY bukrs fistl kngjahr knbelnr knbuzei.
    SORT lt_actual_pi   ASCENDING BY bukrs fistl kngjahr knbelnr knbuzei.
    SORT lt_bgt_rel     ASCENDING BY fiscyear fundsctr cmmtitem docyear docnr rpmax.
    SORT lt_bgt_tr      ASCENDING BY fiscyear fundsctr cmmtitem docyear docnr rpmax.
    SORT lt_plancost    ASCENDING BY gjahr monat fictr cmmtitem.
    SORT lt_plancost_pi ASCENDING BY gjahr pi fictr cmmtitem.
    SORT lt_commit      ASCENDING BY refbn rfpos zhldt.

    SELECT SINGLE fikrs
      FROM t001
      INTO @DATA(lv_fikrs)
     WHERE bukrs = @lrs_bukrs-low.

    IF NOT lt_head IS INITIAL.
      SELECT t~*
        FROM zcont_fm_tar_val AS t INNER JOIN @lt_head AS d ON t~gjahr = d~gjahr
                                                           AND t~fictr = d~fictr
                                                           AND t~fipex = d~cmmtitem
        INTO TABLE @DATA(lt_target).
      IF sy-subrc = 0.
        SORT lt_target BY gjahr fictr fipex.
      ENDIF.
    ENDIF.

    LOOP AT lt_head ASSIGNING FIELD-SYMBOL(<fs_head>).
      ASSIGN ls_entity TO <fs_set>.
      <fs_set>-fikrs              = lv_fikrs.
      <fs_set>-bukrs              = lrs_bukrs-low.
      <fs_set>-gjahr              = <fs_head>-gjahr.
      <fs_set>-psphi              = <fs_head>-psphi.
      CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
        EXPORTING
          input  = <fs_set>-psphi
        IMPORTING
          output = <fs_set>-pspid.
      <fs_set>-post1              = <fs_head>-post1.
      <fs_set>-fictr              = <fs_head>-fictr.
      <fs_set>-bezeich            = <fs_head>-bezeich.
      <fs_set>-fipex              = <fs_head>-cmmtitem.
      <fs_set>-bezei              = <fs_head>-cmmtitem_txt.
      <fs_set>-display            = <fs_head>-display.
      <fs_set>-pstrt              = <fs_head>-pstrt.
      <fs_set>-pende              = <fs_head>-pende.
      <fs_set>-zpo_apv_amt        = <fs_head>-zpo_apv_amt.
      READ TABLE lt_target INTO DATA(ls_target) WITH KEY gjahr = <fs_head>-gjahr
                                                         fictr = <fs_head>-fictr
                                                         fipex = <fs_head>-cmmtitem BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_set>-ztarget = ls_target-ztarget.
      ENDIF.
      <fs_set>-wtgxxx             = <fs_head>-plancost.
      <fs_set>-ztrans_budget      = <fs_head>-budget_tr.
      <fs_set>-zrelease_budget    = <fs_head>-budget_rel.
      <fs_set>-fkbtr              = <fs_head>-commit.
      <fs_set>-zactual            = <fs_head>-actual.
*      <fs_set>-zconsumed          = <fs_head>-commit + <fs_head>-actual.
      <fs_set>-zconsumed          = <fs_head>-zpo_apv_amt + <fs_head>-actual.
*      <fs_set>-zavail             = <fs_head>-budget_rel - <fs_head>-commit - <fs_head>-actual.
      <fs_set>-zavail             = <fs_head>-budget_rel - <fs_head>-zpo_apv_amt - <fs_head>-actual.

      APPEND ls_entity TO lt_entity.
      CLEAR: ls_entity.
    ENDLOOP.

    SORT lt_entity BY gjahr fictr fipex.

    SORT lt_restype BY gjahr fictr cmmtitem.
    LOOP AT lt_restype ASSIGNING FIELD-SYMBOL(<fs_restype>).
      READ TABLE lt_entity ASSIGNING <fs_set> WITH KEY gjahr    = <fs_restype>-gjahr
                                                                           fictr    = <fs_restype>-fictr
                                                                           fipex = <fs_restype>-cmmtitem
                                                                           BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND <fs_restype> TO <fs_set>-itemsset.
      ENDIF.
    ENDLOOP.

    SORT lt_restype_pi BY gjahr fictr cmmtitem.
    LOOP AT lt_restype_pi ASSIGNING FIELD-SYMBOL(<fs_res_pi>).
      READ TABLE lt_entity ASSIGNING <fs_set> WITH KEY gjahr    = <fs_res_pi>-gjahr
                                                                           fictr    = <fs_res_pi>-fictr
                                                                           fipex = <fs_res_pi>-cmmtitem
                                                                           BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND <fs_res_pi> TO <fs_set>-piviewset.
      ENDIF.
    ENDLOOP.

    SORT lt_plancost BY gjahr fictr cmmtitem.
    LOOP AT lt_plancost ASSIGNING FIELD-SYMBOL(<fs_plan>).
      READ TABLE lt_entity ASSIGNING <fs_set> WITH KEY gjahr    = <fs_plan>-gjahr
                                                                           fictr    = <fs_plan>-fictr
                                                                           fipex = <fs_plan>-cmmtitem
                                                                           BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND <fs_plan> TO <fs_set>-plancostset.
      ENDIF.
    ENDLOOP.
    SORT lt_plancost_pi BY gjahr fictr cmmtitem.
    LOOP AT lt_plancost_pi ASSIGNING FIELD-SYMBOL(<fs_plan_pi>).
      READ TABLE lt_entity ASSIGNING <fs_set> WITH KEY gjahr    = <fs_plan_pi>-gjahr
                                                                           fictr    = <fs_plan_pi>-fictr
                                                                           fipex = <fs_plan_pi>-cmmtitem
                                                                           BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND <fs_plan_pi> TO <fs_set>-plancostpiset.
      ENDIF.
    ENDLOOP.
    SORT lt_bgt_tr BY fiscyear fundsctr cmmtitem.
    LOOP AT lt_bgt_tr ASSIGNING FIELD-SYMBOL(<fs_tr>).
      READ TABLE lt_entity ASSIGNING <fs_set> WITH KEY gjahr    = <fs_tr>-fiscyear
                                                                           fictr    = <fs_tr>-fundsctr
                                                                           fipex = <fs_tr>-cmmtitem
                                                                           BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND <fs_tr> TO <fs_set>-budget_trset.
      ENDIF.
    ENDLOOP.

    SORT lt_bgt_rel BY fiscyear fundsctr cmmtitem.
    LOOP AT lt_bgt_rel ASSIGNING FIELD-SYMBOL(<fs_rel>).
      READ TABLE lt_entity ASSIGNING <fs_set> WITH KEY gjahr    = <fs_rel>-fiscyear
                                                                           fictr    = <fs_rel>-fundsctr
                                                                           fipex = <fs_rel>-cmmtitem
                                                                           BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND <fs_rel> TO <fs_set>-budget_relset.
      ENDIF.
    ENDLOOP.

    SORT lt_actual BY zrpt_year fistl fipex.
    LOOP AT lt_actual ASSIGNING FIELD-SYMBOL(<fs_act>).
      READ TABLE lt_entity ASSIGNING <fs_set> WITH KEY gjahr    = <fs_act>-zrpt_year
                                                                           fictr    = <fs_act>-fistl
                                                                           fipex = <fs_act>-fipex
                                                                           BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND <fs_act> TO <fs_set>-actualset.
      ENDIF.
    ENDLOOP.
    SORT lt_actual_pi BY zrpt_year fistl fipex.
    LOOP AT lt_actual_pi ASSIGNING FIELD-SYMBOL(<fs_act_pi>).
      READ TABLE lt_entity ASSIGNING <fs_set> WITH KEY gjahr    = <fs_act_pi>-zrpt_year
                                                                           fictr    = <fs_act_pi>-fistl
                                                                           fipex = <fs_act_pi>-fipex
                                                                           BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND <fs_act_pi> TO <fs_set>-actualpiset.
      ENDIF.
    ENDLOOP.
    SORT lt_commit BY zrpt_year fistl fipex.
    LOOP AT lt_commit ASSIGNING FIELD-SYMBOL(<fs_com>).
      READ TABLE lt_entity ASSIGNING <fs_set> WITH KEY gjahr    = <fs_com>-zrpt_year
                                                                           fictr    = <fs_com>-fistl
                                                                           fipex = <fs_com>-fipex
                                                                           BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND <fs_com> TO <fs_set>-commitset.
      ENDIF.
    ENDLOOP.
    CLEAR: lt_restype,lt_restype_pi,lt_plancost,lt_plancost_pi,lt_bgt_tr,lt_bgt_rel,lt_actual,lt_actual_pi,lt_commit.
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE lt_entity LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

    copy_data_to_ref( EXPORTING is_data = lt_entity CHANGING cr_data = er_entityset ).
** delete
*    GET TIME STAMP FIELD lv_end_time.
*    lv_diff =  lv_end_time - lv_start_time.
** delete
  ENDMETHOD.
