FUNCTION zfm_plancost1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_INPUT) TYPE  ZTFM_COND01_S
*"     REFERENCE(IT_GJAHR) TYPE  BSPL_GJAHR_T
*"     REFERENCE(IT_DATA) TYPE  ZTFM_BUDGET_T
*"  CHANGING
*"     REFERENCE(CT_PLANCOST) TYPE  ZFM_PLANCOST_OUT_CPM_T
*"     REFERENCE(CT_PLANCOST_PI) TYPE  ZFM_PLANCOST_OUT_CPM_T
*"----------------------------------------------------------------------
*-----------------------------------------------------------------------
*  Author                   : Vaijanath Gambhire - V2UUDNS
*  Functional Owner         : Midhun Mohan
*  Date                     : March 12 2025
*  SAP Charm # / JIRA ID # 	: 5000000502/CFINJSD-5091
*  Description              : FM Drilldown - Plan summary
*  Workbench TR             : V1DK909114
*-----------------------------------------------------------------------
  TYPES: BEGIN OF ty_plancost,
           fictr TYPE fistl,
           posnr TYPE ps_posnr,
           fipex TYPE fipex,
         END OF ty_plancost.

  DATA: lv_posid    TYPE prps-posid,
        lv_posnr    TYPE ps_posnr,
        lv_monat    TYPE monat VALUE '00',
        lt_keyfig   TYPE rsd_t_dta_pro,
        lt_char     TYPE rsd_t_dta_pro,
        lt_message  TYPE bsanly_t_message,
        ls_range    TYPE rsdri_s_range,
        lt_range    TYPE rsdri_t_range,
        lr_data     TYPE REF TO data,
        lv_plancost TYPE wrbtr,
        lv_plan_qty TYPE menge_d,
        lv_fictr    TYPE fistl,
        lv_gjahr    TYPE gjahr,
        lv_last     TYPE sy-datum,
        lv_cost     TYPE wrbtr,
        lv_left     TYPE wrbtr,
        lv_fipex    TYPE fipex,
        lr_posnr    TYPE RANGE OF ps_posnr,
        lt_pr       TYPE TABLE OF zptpt_prepr,
        ls_output   TYPE zfm_plancost_out_cpm_s,
        lt_plancost TYPE zfm_plancost_out_cpm_t,
        ls_plancost_cpm TYPE zfm_plancost_out_cpm_s,
        lt_plancost_cpm TYPE zfm_plancost_out_cpm_t.


  CALL METHOD /cpd/cl_pfp_ip_service=>get_cube_keyfig_name
    EXPORTING
      iv_infoprov = /cpd/cl_pfp_constants=>gc_pfp_cube
    IMPORTING
      et_dta_pro  = lt_keyfig.

  CALL METHOD /cpd/cl_pfp_ip_service=>get_cube_char_name
    EXPORTING
      iv_infoprov = /cpd/cl_pfp_constants=>gc_pfp_cube
    IMPORTING
      et_dta_pro  = lt_char.
* Retain the Required Key figures
  DELETE lt_keyfig WHERE iobjnm <> /cpd/cl_pfp_constants=>gc_tcost
                     AND iobjnm <> /cpd/cl_pfp_constants=>gc_ftrate
                     AND iobjnm <> /cpd/cl_pfp_constants=>gc_qty
                     AND iobjnm <> /cpd/cl_pfp_constants=>gc_pcost.
* Retain the Required char
  DELETE lt_char WHERE iobjnm <> /cpd/cl_pfp_constants=>gc_fpoid
                   AND iobjnm <> zde_cl_con_cpm_pw_util=>gc_mflg
                   AND iobjnm <> /cpd/cl_pfp_constants=>gc_fver
                   AND iobjnm <> /cpd/cl_pfp_constants=>gc_fpid
                   AND iobjnm <> /cpd/cl_pfp_constants=>gc_frtyp
                   AND iobjnm <> /cpd/cl_pfp_constants=>gc_calmonth.

  DATA(lt_data) = it_data.
  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
      EXPORTING
        input     = <fs_data>-fictr
      IMPORTING
        output    = <fs_data>-posid
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_data>-posid ) TO lr_posnr.
  ENDLOOP.

  DATA(lv_bukrs) = is_input-bukrs[ 1 ]-low.

  SELECT SINGLE fikrs
    FROM t001
    INTO @DATA(lv_fikrs)
   WHERE bukrs = @lv_bukrs.

  SELECT r~*
    FROM zptpt_prepr AS r INNER JOIN @lt_data AS t ON r~ps_psp_pnr = t~posid
*   WHERE ps_psp_pnr    IN @lr_posnr
   WHERE controlstatus EQ 'A'
     AND preprsta      NE 'K'
     AND dprepr        EQ ''
     AND r~gjahr IN @is_input-gjahr
   INTO TABLE @DATA(lt_prepr)
     .
  IF sy-subrc = 0.
    SORT lt_prepr BY prepurreqn prebnfpo.
    DELETE ADJACENT DUPLICATES FROM lt_prepr COMPARING prepurreqn prebnfpo.
    SORT lt_prepr BY ps_psp_pnr.
  ENDIF.
  SELECT *
    FROM fmfctrt
    INTO TABLE @DATA(lt_fmfctrt)
   WHERE fictr IN @is_input-fictr
     AND spras EQ @sy-langu.
  IF sy-subrc = 0.
    SORT lt_fmfctrt BY fictr fikrs spras.
  ENDIF.
  SELECT *
    FROM fmcit
    INTO TABLE @DATA(lt_fmcit)
   WHERE fipex IN @is_input-cmmtitem
     AND spras EQ @sy-langu.
  IF sy-subrc = 0.
    SORT lt_fmcit BY fikrs fipex.
  ENDIF.
  SELECT *
    FROM prps
    INTO TABLE @DATA(lt_prps)
   WHERE pspnr IN @lr_posnr.
  IF sy-subrc = 0.
    SORT lt_prps BY pspnr.
  ENDIF.
  SELECT *
    FROM zconv_fm_pi_view
    INTO TABLE @DATA(lt_view)
   WHERE gjahr IN @is_input-gjahr.
* Begin of change CFINJSD-5091
  IF sy-subrc = 0.
    SORT lt_view  ASCENDING BY gjahr pi.
  ENDIF.
* End of change CFINJSD-5091
  IF NOT lt_prps IS INITIAL.
    SELECT p~*
      FROM prhi AS p INNER JOIN @lt_prps AS t ON p~down = t~pspnr
      INTO TABLE @DATA(lt_prhi).
    IF sy-subrc = 0.
      SELECT p~*
        FROM prps AS p INNER JOIN @lt_prhi AS i ON p~pspnr = i~posnr
        INTO TABLE @DATA(lt_first).
      IF sy-subrc = 0.
        SELECT m~*
          FROM /cpd/d_mp_item AS m INNER JOIN @lt_first AS t ON m~mp_item_okey = t~objnr
          INTO TABLE @DATA(lt_mp_item).
        IF sy-subrc = 0.
          SELECT h~*
            FROM /cpd/d_pfp_ph AS h INNER JOIN @lt_mp_item AS t ON h~mp_id_int = t~parent_key
           WHERE h~sel_structure ='E'
            INTO TABLE @DATA(lt_ph)            .
          IF sy-subrc = 0.
            SELECT v~*
              FROM /cpd/d_pfp_pv AS v INNER JOIN @lt_ph AS t ON v~parent_key = t~db_key
             WHERE v~version_type = 'PLAN'
              INTO TABLE @DATA(lt_pv)              .
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SORT lt_prhi    BY down.
  SORT lt_first   BY pspnr.
  SORT lt_mp_item BY mp_item_okey.
  SORT lt_ph      BY mp_id_int.


  LOOP AT lt_data ASSIGNING <fs_data>.
    READ TABLE lt_prhi ASSIGNING FIELD-SYMBOL(<fs_prhi>) WITH KEY down = <fs_data>-posid BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE lt_first ASSIGNING FIELD-SYMBOL(<fs_first>) WITH KEY pspnr = <fs_prhi>-posnr BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE lt_mp_item ASSIGNING FIELD-SYMBOL(<fs_item>) WITH KEY mp_item_okey = <fs_first>-objnr BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE lt_ph ASSIGNING FIELD-SYMBOL(<fs_ph>) WITH KEY mp_id_int     = <fs_item>-parent_key
                                                                    sel_structure = 'E' BINARY SEARCH.
          IF sy-subrc = 0.
            IF <fs_ph>-zzplan_option = 'BU' AND ( <fs_data>-cmmtitem = 'EXTERNAL' OR <fs_data>-cmmtitem = 'ASSET' ).
              " monthly view
              CLEAR ls_output.
              ls_output-gjahr        = <fs_data>-gjahr.
              ls_output-posid        = <fs_data>-fictr.
              READ TABLE lt_prps ASSIGNING FIELD-SYMBOL(<fs_prps>) WITH KEY pspnr = <fs_data>-posid BINARY SEARCH.
              IF sy-subrc = 0.
                ls_output-post1      = <fs_prps>-post1.
              ENDIF.
              ls_output-fictr        = <fs_data>-fictr.
              READ TABLE lt_fmfctrt ASSIGNING FIELD-SYMBOL(<fs_fmfctrt>) WITH KEY fictr = <fs_data>-fictr
                                                                                  fikrs = lv_fikrs
                                                                                  spras = sy-langu BINARY SEARCH.
              IF sy-subrc = 0.
                ls_output-bezeich    = <fs_fmfctrt>-beschr.
              ENDIF.
              ls_output-cmmtitem     = <fs_data>-cmmtitem.
              READ TABLE lt_fmcit ASSIGNING FIELD-SYMBOL(<fs_fmcit>) WITH KEY fikrs = lv_fikrs
                                                                              fipex = <fs_data>-cmmtitem BINARY SEARCH.
              IF sy-subrc = 0.
                ls_output-cmmtitem_txt = <fs_fmcit>-bezei.
              ENDIF.
              CLEAR: lv_monat.
              DO 12 TIMES.
                lv_monat = lv_monat + 1.
                APPEND INITIAL LINE TO ct_plancost ASSIGNING FIELD-SYMBOL(<fs_output>).
                MOVE-CORRESPONDING ls_output TO <fs_output>.
                <fs_output>-monat = lv_monat.
              ENDDO.
              READ TABLE lt_prepr TRANSPORTING NO FIELDS WITH KEY ps_psp_pnr = <fs_data>-posid.
              IF sy-subrc = 0.
                DATA(lv_tabix) = sy-tabix.
                LOOP AT lt_prepr ASSIGNING FIELD-SYMBOL(<fs_prepr>) FROM lv_tabix WHERE ps_psp_pnr = <fs_data>-posid.
                  IF <fs_prepr>-lfdat IS INITIAL.
                    <fs_prepr>-lfdat = <fs_prepr>-startdate.
                  ENDIF.
                  <fs_prepr>-prepurreqnitmdescription = <fs_data>-fictr.
                  APPEND <fs_prepr> TO lt_pr.
                  IF <fs_data>-cmmtitem = 'ASSET' AND <fs_prepr>-assetflag IS INITIAL.
                    CONTINUE.
                  ENDIF.
                  IF <fs_data>-cmmtitem = 'EXTERNAL' AND <fs_prepr>-assetflag IS NOT INITIAL.
                    CONTINUE.
                  ENDIF.
                  IF <fs_prepr>-lfdat(4) = <fs_data>-gjahr.
                    ls_output-monat = <fs_prepr>-lfdat(6).
                    ls_output-plancost = <fs_prepr>-localplanvalue.
                    COLLECT ls_output INTO ct_plancost.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.
              ls_range-chanm  = /cpd/cl_pfp_constants=>gc_fpoid. "Name Of Characteristic
              ls_range-sign   = 'I'.                 "Include
              ls_range-compop = 'EQ'.                "Operator
              ls_range-low    = <fs_ph>-plan_id.
              APPEND ls_range TO lt_range.
              CLEAR ls_range.
              READ TABLE lt_pv ASSIGNING FIELD-SYMBOL(<fs_pv>) WITH KEY parent_key   = <fs_ph>-db_key
                                                                        version_type = 'PLAN'.
              IF sy-subrc = 0.
                ls_range-chanm  = /cpd/cl_pfp_constants=>gc_fver. "Name Of Characteristic
                ls_range-sign   = 'I'.                 "Include
                ls_range-compop = 'EQ'.                "Operator
                ls_range-low    = <fs_pv>-version_id.
                APPEND ls_range TO lt_range.
              ENDIF.
*                "Pass filter for CALMONTH
              CLEAR ls_range.
              ls_range-chanm  = /cpd/cl_pfp_constants=>gc_calmonth. "Name Of Characteristic
              ls_range-sign   = 'I'.                 "Include
              ls_range-compop = 'BT'.
              ls_range-low    = <fs_data>-gjahr && '01'.
              ls_range-high   = <fs_data>-gjahr && '12'.
              APPEND ls_range TO lt_range.
              CLEAR ls_range.
              ls_range-chanm  = /cpd/cl_pfp_constants=>gc_frtyp. "Resource Type
              ls_range-sign   = 'I'.                 "Include
              ls_range-compop = 'EQ'.                "Operator
              IF <fs_data>-cmmtitem = 'INTERNAL'.
                ls_range-low = 'ZINT'.
              ELSEIF <fs_data>-cmmtitem = 'EXTERNAL'.
                ls_range-low = 'ZEXT'.
              ELSE.
                ls_range-low = 'ZAST'.
              ENDIF.
              APPEND ls_range TO lt_range.
              CLEAR ls_range.
              ls_range-chanm  = /cpd/cl_pfp_constants=>gc_fpid . "WBS
              ls_range-sign   = 'I'.                 "Include
              ls_range-compop = 'EQ'.                "Operator
              ls_range-low    = 'PR' && <fs_data>-posid.
              APPEND ls_range TO lt_range.
*            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  SORT lt_range BY chanm sign compop low high.
  DELETE ADJACENT DUPLICATES FROM lt_range COMPARING chanm sign compop low high.
*** Read data from infocube.
  CALL METHOD /cpd/cl_pfp_query_services=>get_infocube_data
    EXPORTING
      it_keyfigure = lt_keyfig
      it_char      = lt_char
      it_range     = lt_range
    IMPORTING
      er_t_data    = lr_data
      et_message   = lt_message.
  ASSIGN lr_data->* TO FIELD-SYMBOL(<ft_data>).
  IF <ft_data> IS ASSIGNED.
    LOOP AT <ft_data> ASSIGNING FIELD-SYMBOL(<fs_data1>).
      CLEAR: lv_plancost,
             lv_plan_qty,
             lv_fipex,
             lv_gjahr,
             lv_monat,
             lv_posnr,
             lv_fictr.
      ASSIGN COMPONENT '/CPD/FPCA' OF STRUCTURE <fs_data1> TO FIELD-SYMBOL(<fs_fpca>).
      lv_plancost = <fs_fpca>.
      ASSIGN COMPONENT '/CPD/FQTY' OF STRUCTURE <fs_data1> TO FIELD-SYMBOL(<fs_fqty>).
      lv_plan_qty  = <fs_fqty>.
      ASSIGN COMPONENT '/CPD/FRTYP' OF STRUCTURE <fs_data1> TO FIELD-SYMBOL(<fs_fipex>).
      IF <fs_fipex> = 'ZEXT'.
        lv_fipex = 'EXTERNAL'.
      ELSEIF  <fs_fipex> = 'ZINT'.
        lv_fipex = 'INTERNAL'.
      ELSE.
        lv_fipex = 'ASSET'.
      ENDIF.
      ASSIGN COMPONENT '0CALMONTH' OF STRUCTURE <fs_data1> TO FIELD-SYMBOL(<fs_month>).
      lv_gjahr = <fs_month>(4).
      lv_monat = <fs_month>+4(2).
      ASSIGN COMPONENT '/CPD/FPID' OF STRUCTURE <fs_data1> TO FIELD-SYMBOL(<fs_fpid>).
      lv_posnr = <fs_fpid>+2.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input     = lv_posnr
        IMPORTING
          output    = lv_fictr
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      ASSIGN COMPONENT zde_cl_con_cpm_pw_util=>gc_mflg OF STRUCTURE <fs_data1> TO FIELD-SYMBOL(<fs_mflg>).
      IF sy-subrc = 0.
        READ TABLE ct_plancost INTO ls_plancost_cpm WITH KEY fictr = lv_fictr
                                                       gjahr    = lv_gjahr
                                                       monat    = lv_monat
                                                       cmmtitem = lv_fipex
                                                       .
        IF sy-subrc = 0.
          IF <fs_mflg> EQ abap_true.
            ls_plancost_cpm-plancost  =  lv_plancost.
            COLLECT ls_plancost_cpm INTO lt_plancost_cpm.
          ENDIF.
          CONTINUE.
        ENDIF.
      ENDIF.
      READ TABLE lt_plancost ASSIGNING <fs_output> WITH KEY fictr    = lv_fictr
                                                             gjahr    = lv_gjahr
                                                             cmmtitem = lv_fipex
                                                             monat = lv_monat.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO lt_plancost ASSIGNING <fs_output>.
      ENDIF.

      <fs_output>-fictr    = lv_fictr.
      READ TABLE lt_fmfctrt ASSIGNING <fs_fmfctrt> WITH KEY fictr = <fs_output>-fictr
                                                            fikrs = lv_fikrs
                                                            spras = sy-langu BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_output>-bezeich = <fs_fmfctrt>-beschr.
      ENDIF.
      <fs_output>-posid    = lv_fictr.
      READ TABLE lt_prps ASSIGNING <fs_prps> WITH KEY pspnr = <fs_output>-posid BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_output>-post1      = <fs_prps>-post1.
      ENDIF.
      <fs_output>-gjahr    = lv_gjahr.
      <fs_output>-monat    = lv_monat.
      <fs_output>-cmmtitem = lv_fipex.
      READ TABLE lt_fmcit ASSIGNING <fs_fmcit> WITH KEY fikrs = lv_fikrs
                                                        fipex = <fs_output>-cmmtitem BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_output>-cmmtitem_txt = <fs_fmcit>-bezei.
      ENDIF.
      <fs_output>-plancost  = <fs_output>-plancost + lv_plancost.
      <fs_output>-zplan_qty = <fs_output>-zplan_qty + lv_plan_qty.
    ENDLOOP.
    UNASSIGN <ft_data>.
  ENDIF.

  DATA(lt_plan) = lt_plancost_cpm.


  LOOP AT lt_data ASSIGNING <fs_data>.
    READ TABLE lt_plan TRANSPORTING NO FIELDS WITH KEY fictr    = <fs_data>-fictr
                                                       gjahr    = <fs_data>-gjahr
                                                       cmmtitem = <fs_data>-cmmtitem.
    IF sy-subrc <> 0.
      READ TABLE lt_plancost TRANSPORTING NO FIELDS WITH KEY fictr    = <fs_data>-fictr
                                                             gjahr    = <fs_data>-gjahr
                                                             cmmtitem = <fs_data>-cmmtitem.
      IF sy-subrc = 0.
        CLEAR: lv_monat.
        DO 12 TIMES.
          lv_monat = lv_monat + 1.
          READ TABLE lt_plancost INTO DATA(ls_plan) WITH KEY monat    = lv_monat
                                                             fictr    = <fs_data>-fictr
                                                             gjahr    = <fs_data>-gjahr
                                                             cmmtitem = <fs_data>-cmmtitem.
          IF sy-subrc = 0.
            ls_plan-posid = <fs_data>-fictr.
            ls_plan-post1 = <fs_data>-bezeich.
            APPEND ls_plan TO lt_plancost_cpm.
          ELSE.
            MOVE-CORRESPONDING <fs_data> TO ls_plan.
            ls_plan-monat = lv_monat.
            ls_plan-posid = ls_plan-fictr.
            ls_plan-post1 = ls_plan-bezeich.
            APPEND ls_plan TO lt_plancost_cpm.
          ENDIF.
        ENDDO.
      ELSE.
        CLEAR: lv_monat.
        DO 12 TIMES.
          lv_monat = lv_monat + 1.
          MOVE-CORRESPONDING <fs_data> TO ls_plan.
          ls_plan-monat = lv_monat.
          ls_plan-posid = ls_plan-fictr.
          READ TABLE lt_fmfctrt ASSIGNING <fs_fmfctrt> WITH KEY fictr = ls_plan-fictr
                                                                fikrs = lv_fikrs
                                                                spras = sy-langu  BINARY SEARCH.
          IF sy-subrc = 0.
            ls_plan-bezeich = <fs_fmfctrt>-bezeich.
            ls_plan-post1   = <fs_fmfctrt>-bezeich.
          ENDIF.
          APPEND ls_plan TO lt_plancost_cpm.
        ENDDO.
      ENDIF.
    ENDIF.

  ENDLOOP.

  SORT lt_plancost_cpm ASCENDING BY gjahr fictr cmmtitem monat.
  " PI View
  LOOP AT lt_plancost_cpm ASSIGNING FIELD-SYMBOL(<fs_cost>) GROUP BY ( gjahr    = <fs_cost>-gjahr
                                                                   fictr    = <fs_cost>-fictr
                                                                   cmmtitem = <fs_cost>-cmmtitem ).
    LOOP AT lt_view ASSIGNING FIELD-SYMBOL(<fs_view>).
      APPEND INITIAL LINE TO ct_plancost_pi ASSIGNING FIELD-SYMBOL(<fs_pi>).
      <fs_pi>-gjahr        = <fs_cost>-gjahr.
      <fs_pi>-pi           = <fs_view>-pi.
      <fs_pi>-posid        = <fs_cost>-fictr.
      <fs_pi>-post1        = <fs_cost>-post1.
      <fs_pi>-fictr        = <fs_cost>-fictr.
      <fs_pi>-bezeich      = <fs_cost>-bezeich.
      <fs_pi>-cmmtitem     = <fs_cost>-cmmtitem.
      <fs_pi>-cmmtitem_txt = <fs_cost>-cmmtitem_txt.
*      IF <fs_cost>-cmmtitem = 'EXTERNAL'.
*        READ TABLE lt_pr TRANSPORTING NO FIELDS WITH KEY gjahr = <fs_cost>-gjahr prepurreqnitmdescription = <fs_cost>-fictr.
*        IF sy-subrc = 0.
*          CONTINUE.
*        ENDIF.
*      ENDIF.
      LOOP AT GROUP <fs_cost> INTO DATA(ls_cost).
        DATA(lv_time) = ls_cost-gjahr && ls_cost-monat.
        IF lv_time >= <fs_view>-zdatefrom(6) AND lv_time <= <fs_view>-zdateto(6).
          IF lv_time = <fs_view>-zdatefrom(6) AND lv_left IS NOT INITIAL.
            CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
              EXPORTING
                day_in            = <fs_view>-zdatefrom
              IMPORTING
                last_day_of_month = lv_last
              EXCEPTIONS
                day_in_no_date    = 1
                OTHERS            = 2.
            DATA(lv_cur) = <fs_view>-zdatefrom+6(2).
            DATA(lv_end) = lv_last+6(2).
            DATA(lv_ran) = lv_end - lv_cur.
            <fs_pi>-plancost = lv_left.
            CLEAR: lv_cur,
                   lv_end,
                   lv_ran,
                   lv_cost,
                   lv_left.
            CONTINUE.
          ENDIF.
          IF lv_time = <fs_view>-zdateto(6).
            CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
              EXPORTING
                day_in            = <fs_view>-zdateto
              IMPORTING
                last_day_of_month = lv_last
              EXCEPTIONS
                day_in_no_date    = 1
                OTHERS            = 2.
            lv_cur = <fs_view>-zdateto+6(2).
            lv_end = lv_last+6(2).
            lv_ran = lv_end - lv_cur.
            lv_cost = ls_cost-plancost * lv_cur / lv_end.
            lv_left = ls_cost-plancost - lv_cost.
            <fs_pi>-plancost = <fs_pi>-plancost + lv_cost.
            CLEAR: lv_cur,
                   lv_end,
                   lv_ran,
                   lv_cost.
            CONTINUE.
          ENDIF.
          <fs_pi>-plancost = <fs_pi>-plancost + ls_cost-plancost.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    CLEAR lv_left. " ++ CFINJSD-5091
  ENDLOOP.

  LOOP AT lt_pr ASSIGNING <fs_prepr>.
    READ TABLE ct_plancost_pi ASSIGNING <fs_pi> WITH KEY fictr    = <fs_prepr>-prepurreqnitmdescription
                                                         gjahr    = <fs_prepr>-gjahr
                                                         pi       = <fs_prepr>-pinum
                                                         cmmtitem = 'EXTERNAL'.
    IF sy-subrc = 0.
      <fs_pi>-plancost = <fs_pi>-plancost + <fs_prepr>-localplanvalue.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_plancost_cpm INTO ls_plancost_cpm.
    READ TABLE ct_plancost ASSIGNING FIELD-SYMBOL(<ls_plancost>) WITH KEY fictr    = ls_plancost_cpm-fictr
                                                       gjahr    = ls_plancost_cpm-gjahr
                                                       monat    = ls_plancost_cpm-monat
                                                       cmmtitem = ls_plancost_cpm-cmmtitem.
    IF sy-subrc = 0.
      <ls_plancost>-plancost = <ls_plancost>-plancost + ls_plancost_cpm-plancost.
    ELSE.
      APPEND ls_plancost_cpm TO ct_plancost.
    ENDIF.
  ENDLOOP.
  CLEAR: lr_data,
         lt_range,
         lt_message.


ENDFUNCTION.
