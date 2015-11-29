""! Criado por: Antonio P. Lacerda
""! Criado em:  2015.06.30
""! Objectivo: Gestao das ordens de transporte

""! Futuros enhancements:
""!   * E071 - Objectos incluidos na ordem
""!   * TMS_MGR_READ_TRANSPORT_QUEUE
""!   * TRINT_DISPLAY_LOG_OVERVIEW
""!   * TMS_UI_SHOW_TRANSPORT_LOGS
""!   * TR_READ_GLOBAL_INFO_OF_REQUEST
""!   * TRINT_TDR_USER_COMMAND -> FM com as varias funcoes das ordens
""!   * SUBMIT rddprott AND RETURN WITH pv_korr  = lv_trkorr. " Exibir o log
""!   * TR_DISPLAY_REQUEST -> FM para exibir a ordem com i_activetab = HEADER

REPORT  ztest_apl_015.

DATA: e070 TYPE e070, e070a TYPE e070a, e07t TYPE e07t, maxrc TYPE trretcode, tmscsys TYPE tmscsys.
SELECTION-SCREEN BEGIN OF BLOCK main.
SELECT-OPTIONS:
  s_trkorr FOR e070-trkorr,
  s_trfunc FOR e070-trfunction,
  s_trstat FOR e070-trstatus,
  s_as4use FOR e070-as4user DEFAULT sy-uname SIGN I OPTION EQ,
  s_as4dat FOR e070-as4date DEFAULT sy-datum SIGN I OPTION EQ,
  s_as4tex FOR e07t-as4text,
  s_projec FOR e070a-reference,
  s_sysnam FOR tmscsys-sysnam.
SELECTION-SCREEN END OF BLOCK main.

INITIALIZATION.

CLASS lcl_transport_request DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS:
      get_request
        IMPORTING iv_trkorr TYPE trkorr
        RETURNING value(ro_request) TYPE REF TO lcl_transport_request
        RAISING cx_sy_ref_is_initial,

      free_all,

      display_in_queue
        IMPORTING
          iv_system TYPE any
          it_filter TYPE tmsiqfils.

    METHODS:
      constructor IMPORTING iv_trkorr TYPE trkorr,
      free,
      get_objects,
      get_list_data
        EXPORTING
          ev_trkorr   TYPE any
          ev_user     TYPE any
          ev_function TYPE any
          ev_status   TYPE any
          ev_date     TYPE any
          ev_project  TYPE any
          ev_text     TYPE any,
      get_systems_status
        IMPORTING iv_sytem TYPE trtarsys
        RETURNING value(rs_action) TYPE ctslg_action,
      get_filter_for_queue RETURNING value(rt_filter) TYPE tmsiqfils,

      display_request,
      display_request_settings
        IMPORTING
          iv_operation TYPE sy-ucomm DEFAULT 'EDIT'
          iv_activetab TYPE trreqstate-activetab DEFAULT sreqs_activetab_header,
      display_owner,
      display_log.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF  st_request,
        trkorr  TYPE trkorr,
        request TYPE REF TO lcl_transport_request,
      END OF    st_request.

    TYPES BEGIN OF  st_main_data.
            INCLUDE STRUCTURE e070.
    TYPES   as4text TYPE as4text.
    TYPES END OF    st_main_data.

    CLASS-DATA mt_requests TYPE SORTED TABLE OF st_request WITH UNIQUE KEY trkorr.

    DATA:
      mv_trkorr TYPE trkorr,
      ms_main_data TYPE st_main_data,
      mv_description TYPE as4text,
      mv_project TYPE project_id,
      mt_attributes TYPE STANDARD TABLE OF e070a,
      mt_objects TYPE STANDARD TABLE OF e071,
      ms_cofile TYPE ctslg_cofile,
      mt_sub_requests TYPE SORTED TABLE OF trkorr WITH UNIQUE KEY table_line,
      mt_included_trkorr TYPE SORTED TABLE OF trkorr WITH UNIQUE KEY table_line.

    METHODS:
      get_main_data,
      get_attributes,
      get_file_system_data,
      get_sub_requests,
      get_included_requests.

ENDCLASS.

CLASS lcl_transport_request IMPLEMENTATION.
  METHOD display_in_queue.
    DATA:
      ls_system_config TYPE tmscsys,
      ls_filter TYPE stmsiqfil.

    ls_system_config-sysnam = iv_system.

    CALL FUNCTION 'TMS_IQ_FILTER_SAVE'
      EXPORTING
        is_system_config = ls_system_config
      TABLES
        it_filter        = it_filter
      CHANGING
        is_filter        = ls_filter.

    CALL FUNCTION 'TMS_UIQ_IMPORT_QUEUE_DISPLAY'
      EXPORTING
        iv_system                   = ls_system_config-sysnam
      EXCEPTIONS
        import_queue_display_failed = 1
        OTHERS                      = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.  " display_in_queue
  METHOD get_filter_for_queue.
    DATA ls_filter LIKE LINE OF rt_filter.
    ls_filter-field = 'TRKORR'.
    ls_filter-icon = icon_filter.
    ls_filter-name = 'Ordenzeca'.
    ls_filter-sign = 'I'.
    ls_filter-optn = 'EQ'.
    ls_filter-value = me->mv_trkorr.

    APPEND ls_filter TO rt_filter.
  ENDMETHOD.  " get_filter_for_queue
  METHOD get_request.
    DATA ls_request LIKE LINE OF mt_requests.
    READ TABLE mt_requests
      INTO ls_request
      WITH KEY trkorr = iv_trkorr.

    IF ls_request IS INITIAL.
      SELECT COUNT( * )
        FROM e070
        WHERE trkorr = iv_trkorr
        AND trfunction IN ('W','K').
      IF sy-dbcnt = 0.
        RAISE EXCEPTION TYPE cx_sy_ref_is_initial.
      ENDIF.
      CREATE OBJECT ls_request-request
        EXPORTING
          iv_trkorr = iv_trkorr.

      ls_request-trkorr = iv_trkorr.
      INSERT ls_request INTO TABLE mt_requests.
    ENDIF.

    ro_request = ls_request-request.

  ENDMETHOD.
  METHOD free_all.
    FIELD-SYMBOLS <request> LIKE LINE OF mt_requests.
    LOOP AT mt_requests ASSIGNING <request>.
      <request>-request->free( ).
    ENDLOOP.  " request
    UNASSIGN <request>.
  ENDMETHOD.
  METHOD constructor.
    me->mv_trkorr = iv_trkorr.
    me->get_main_data( ).
    me->get_attributes( ).
    me->get_file_system_data( ).
    me->get_included_requests( ).
  ENDMETHOD.
  METHOD free.
    DATA ls_request TYPE st_request.
    READ TABLE mt_requests
      INTO ls_request
      WITH TABLE KEY trkorr = me->mv_trkorr.

    DELETE mt_requests
      WHERE trkorr = me->mv_trkorr.

    FREE ls_request-request.
  ENDMETHOD.
  METHOD get_main_data.
    SELECT SINGLE *
      FROM e070
      INNER JOIN e07t
        ON e07t~trkorr = e070~trkorr
      INTO CORRESPONDING FIELDS OF me->ms_main_data
      WHERE e070~trkorr = me->mv_trkorr.

    SELECT SINGLE reference
      FROM e070a
      INTO me->mv_project
      WHERE trkorr = me->ms_main_data-trkorr
        AND attribute = 'SAP_CTS_PROJECT'.
  ENDMETHOD.
  METHOD get_attributes.
    SELECT *
      FROM e070a
      INTO TABLE me->mt_attributes
      WHERE trkorr = me->ms_main_data-trkorr
        AND pos NOT IN ('1','999999')
        AND attribute <> 'SAPCOMPONENT'.
  ENDMETHOD.
  METHOD get_objects.
    SELECT *
      FROM e071
      INTO TABLE me->mt_objects
      WHERE trkorr = me->mv_trkorr.

    CHECK me->mt_sub_requests IS NOT INITIAL.
    SELECT *
      FROM e071
      APPENDING TABLE me->mt_objects
      FOR ALL ENTRIES IN me->mt_sub_requests
      WHERE trkorr = me->mt_sub_requests-table_line.

    SORT me->mt_objects.
    DELETE ADJACENT DUPLICATES FROM me->mt_objects.
  ENDMETHOD.
  METHOD get_file_system_data.
    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr = me->ms_main_data-trkorr
      IMPORTING
        es_cofile = me->ms_cofile.
  ENDMETHOD.
  METHOD display_request.
    SET PARAMETER ID 'KOR' FIELD me->mv_trkorr.
    CALL FUNCTION 'TR_PRESENT_REQUEST'
      EXPORTING
        iv_trkorr    = me->mv_trkorr
        iv_highlight = abap_true.
  ENDMETHOD.
  METHOD display_request_settings.
    CALL FUNCTION 'TR_DISPLAY_REQUEST'
      EXPORTING
        i_trkorr    = me->mv_trkorr
        i_operation = iv_operation
        i_activetab = iv_activetab.
  ENDMETHOD.
  METHOD display_owner.
    CALL FUNCTION 'TMS_UI_SHOW_USER_DETAILS'
      EXPORTING
        iv_user                 = me->ms_main_data-as4user
      EXCEPTIONS
        cancelled_by_user       = 1
        get_user_address_failed = 2
        OTHERS                  = 3.
  ENDMETHOD.
  METHOD display_log.
    SUBMIT rddprott
      AND RETURN
      WITH pv_korr = me->mv_trkorr.
  ENDMETHOD.
  METHOD get_systems_status.
    FIELD-SYMBOLS <system> LIKE LINE OF me->ms_cofile-systems.
    READ TABLE me->ms_cofile-systems
      ASSIGNING <system>
      WITH KEY systemid = iv_sytem.

    CHECK <system> IS ASSIGNED.

    FIELD-SYMBOLS <step> LIKE LINE OF <system>-steps.
    READ TABLE <system>-steps
      ASSIGNING <step>
      INDEX lines( <system>-steps ).

    CHECK <step> IS ASSIGNED.

    READ TABLE <step>-actions
      INTO rs_action
      INDEX lines( <step>-actions ).

  ENDMETHOD.  " get_systems_status
  METHOD get_sub_requests.
    SELECT trkorr
      FROM e070
      INTO TABLE me->mt_sub_requests
      WHERE strkorr = me->mv_trkorr.
  ENDMETHOD.  " get_sub_requests
  METHOD get_list_data.
    ev_trkorr   = me->mv_trkorr.
    ev_user     = me->ms_main_data-as4user.
    ev_function = me->ms_main_data-trfunction.
    ev_status   = me->ms_main_data-trstatus.
    ev_date     = me->ms_main_data-as4date.
    ev_project  = me->mv_project.
    ev_text     = me->ms_main_data-as4text.
  ENDMETHOD.  " get_list_data
  METHOD get_included_requests.

    DATA lv_sysid TYPE string.
    lv_sysid = sy-sysid && '%'.

    DATA lt_trkorr TYPE STANDARD TABLE OF trkorr.

    SELECT DISTINCT obj_name
      FROM e071
      INTO TABLE lt_trkorr
      WHERE trkorr = me->mv_trkorr
        AND pgmid = 'CORR'
        AND object = 'MERG'
        AND obj_name LIKE lv_sysid.

    SORT lt_trkorr. DELETE ADJACENT DUPLICATES FROM lt_trkorr.

    INSERT LINES OF lt_trkorr INTO TABLE me->mt_included_trkorr.
  ENDMETHOD.  " get_included_requests
ENDCLASS.                    "lcl_transport_request IMPLEMENTATION

CLASS lcl_list_transport_requests DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      tr_trkorr TYPE RANGE OF e070-trkorr,
      tr_trfunc TYPE RANGE OF e070-trfunction,
      tr_trstat TYPE RANGE OF e070-trstatus,
      tr_as4use TYPE RANGE OF e070-as4user,
      tr_as4dat TYPE RANGE OF e070-as4date,
      tr_as4tex TYPE RANGE OF e07t-as4text,
      tr_projec TYPE RANGE OF e070a-reference,
      tr_sysnam TYPE RANGE OF tmscsys-sysnam.

    METHODS:
      constructor,

      get_data
        IMPORTING
          ir_trkorr TYPE tr_trkorr
          ir_trfunc TYPE tr_trfunc
          ir_trstat TYPE tr_trstat
          ir_as4use TYPE tr_as4use
          ir_as4dat TYPE tr_as4dat
          ir_as4tex TYPE tr_as4tex
          ir_projec TYPE tr_projec
          ir_sysnam TYPE tr_sysnam,

      get_list.

    METHODS:
      on_double_click
        FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,
      on_link_click
        FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column,
      on_added_function
        FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_trkorr      TYPE string VALUE'TRKORR'      ,
      c_trfunction  TYPE string VALUE'TRFUNCTION'  ,
      c_trstatus    TYPE string VALUE'TRSTATUS'    ,
      c_as4user     TYPE string VALUE'AS4USER'     ,
      c_as4date     TYPE string VALUE'AS4DATE'     ,
      c_project_id  TYPE string VALUE'PROJECT_ID'  ,
      c_as4text     TYPE string VALUE'AS4TEXT'     .

    CONSTANTS:
      c_dats TYPE string VALUE 'DATS',
      c_char TYPE string VALUE 'CHAR'.

    DATA mt_systems TYPE triwb_t_system.

    DATA mt_wboattrt TYPE STANDARD TABLE OF wboattrt.

    DATA mo_alv TYPE REF TO cl_salv_table.

    DATA mp_data TYPE REF TO data.

    METHODS:
      get_systems,
      get_attributes,
      build_structure,
      append_new_line IMPORTING io_tr TYPE REF TO lcl_transport_request,
      set_layout IMPORTING it_columns TYPE salv_t_column_ref,
      get_line_trkorr
        IMPORTING iv_row TYPE salv_de_row
        RETURNING value(rv_trkorr) TYPE trkorr,
      pop_up_system RETURNING value(rv_system) TYPE tmssysnam,
      get_filter_selected_reqs
        IMPORTING it_rows TYPE salv_t_row
        RETURNING value(rt_filter) TYPE tmsiqfils.

ENDCLASS.

CLASS lcl_list_transport_requests IMPLEMENTATION.
  METHOD constructor.
    me->get_systems( ).
    me->get_attributes( ).
    me->build_structure( ).
  ENDMETHOD.  " constructor
  METHOD get_attributes.
    SELECT *
      FROM wboattrt
      INTO TABLE me->mt_wboattrt
      WHERE langu = sy-langu
        AND attr NOT LIKE 'SAP%'
        AND attr <> 'EXPORT_TIMESTAMP'.
  ENDMETHOD.  " get_attributes
  METHOD build_structure.
    DEFINE m_append_field_cat.
      add 1 to lv_counter.
      append initial line to lt_field_cat assigning <field>.
      <field>-col_pos   = lv_counter.
      <field>-fieldname = &1.
      <field>-domname   = &2.
      <field>-datatype  = &3.
      <field>-intlen    = &4.
    END-OF-DEFINITION.

    DATA lt_field_cat TYPE lvc_t_fcat.

    DATA lv_counter TYPE i.

    FIELD-SYMBOLS <field> LIKE LINE OF lt_field_cat.
    m_append_field_cat:
      c_trkorr      c_trkorr      c_char 20,
      c_trfunction  c_trfunction  c_char 01,
      c_trstatus    c_trstatus    c_char 01,
      c_as4user     c_as4user     c_char 12,
      c_as4date     c_as4date     c_dats 08,
      c_project_id  c_project_id  c_char 10,
      c_as4text     c_as4text     c_char 60.

    DATA lv_colname LIKE <field>-fieldname.

    FIELD-SYMBOLS <system> LIKE LINE OF mt_systems.
    LOOP AT mt_systems ASSIGNING <system>.
      lv_colname = <system>-sysname && '_RC'.
      m_append_field_cat lv_colname '' c_char 4.

      lv_colname = <system>-sysname && '_DATE'.
      m_append_field_cat lv_colname '' c_dats 8.
    ENDLOOP.  " systems
    UNASSIGN <system>.

    FIELD-SYMBOLS <wboattrt> LIKE LINE OF mt_wboattrt.
    LOOP AT mt_wboattrt ASSIGNING <wboattrt>.
      lv_colname = <wboattrt>-attr.
      m_append_field_cat lv_colname '' c_char 32.
    ENDLOOP.  " systems
    UNASSIGN <wboattrt>.

    cl_alv_table_create=>create_dynamic_table(
      EXPORTING it_fieldcatalog = lt_field_cat
      IMPORTING ep_table        = me->mp_data
      EXCEPTIONS OTHERS         = 1
    ).

  ENDMETHOD.  " build_structure
  METHOD get_systems.
    DATA lt_release TYPE triwb_t_release.
    DATA lt_deliver TYPE triwb_t_deliver.

    DATA lt_systems LIKE mt_systems.
    CALL FUNCTION 'TMS_WBO_CONFIG_READ'
      IMPORTING
        et_system  = lt_systems
        et_release = lt_release
        et_deliver = lt_deliver.

    IF s_sysnam[] IS NOT INITIAL.
      DELETE lt_systems WHERE sysname IN s_sysnam.
    ENDIF.

    FIELD-SYMBOLS <release> LIKE LINE OF lt_release.
    READ TABLE lt_release
      ASSIGNING <release>
      WITH KEY intsys = sy-sysid.

    DATA ls_system LIKE LINE OF lt_systems.
    READ TABLE lt_systems
      INTO ls_system
      WITH KEY sysname = <release>-consys.

    APPEND ls_system TO mt_systems.

    LOOP AT mt_systems INTO ls_system.
      FIELD-SYMBOLS <deliver> LIKE LINE OF lt_deliver.
      LOOP AT lt_deliver ASSIGNING <deliver>
      WHERE fromsystem = ls_system-sysname.
        READ TABLE lt_systems
          INTO ls_system
          WITH KEY sysname = <deliver>-tosystem.
        CHECK sy-subrc = 0.
        APPEND ls_system TO mt_systems.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.  " get_systems
  METHOD get_data.
    DATA lt_trkorr TYPE SORTED TABLE OF trkorr WITH UNIQUE KEY table_line.
    SELECT  e070~trkorr
      FROM e070
      INNER JOIN e07t
        ON e07t~trkorr = e070~trkorr
      INNER JOIN e070a
        ON  e070a~trkorr = e070~trkorr
        AND e070a~attribute = 'SAP_CTS_PROJECT'
      INTO TABLE lt_trkorr
      WHERE e070~trkorr     IN ir_trkorr
        AND e070~trfunction IN ('W','K')
        AND e070~trfunction IN ir_trfunc
        AND e070~trstatus   IN ir_trstat
        AND e070~as4user    IN ir_as4use
        AND e070~as4date    IN ir_as4dat
        AND e07t~as4text    IN ir_as4tex
        AND e070a~reference IN ir_projec.

    FIELD-SYMBOLS <trkorr> LIKE LINE OF lt_trkorr.
    LOOP AT lt_trkorr ASSIGNING <trkorr>.
      TRY.
          me->append_new_line( lcl_transport_request=>get_request( <trkorr> ) ).
        CATCH cx_sy_ref_is_initial.
      ENDTRY.
    ENDLOOP.  " trkorr
    UNASSIGN <trkorr>.
  ENDMETHOD.  " get_data
  METHOD append_new_line.
    DEFINE m_new_fs.
      field-symbols <&1> type any.
      assign component &1 of <line> to <&1>.
    END-OF-DEFINITION.  " m_nem_fs

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN mp_data->* TO <table>.

    FIELD-SYMBOLS <line> TYPE any.
    APPEND INITIAL LINE TO <table> ASSIGNING <line>.

    FIELD-SYMBOLS <trkorr> TYPE any.
    ASSIGN COMPONENT c_trkorr OF STRUCTURE <line> TO <trkorr>.
    FIELD-SYMBOLS <trfunction> TYPE any.
    ASSIGN COMPONENT c_trfunction OF STRUCTURE <line> TO <trfunction>.
    FIELD-SYMBOLS <trstatus> TYPE any.
    ASSIGN COMPONENT c_trstatus OF STRUCTURE <line> TO <trstatus>.
    FIELD-SYMBOLS <as4user> TYPE any.
    ASSIGN COMPONENT c_as4user OF STRUCTURE <line> TO <as4user>.
    FIELD-SYMBOLS <as4date> TYPE any.
    ASSIGN COMPONENT c_as4date OF STRUCTURE <line> TO <as4date>.
    FIELD-SYMBOLS <project> TYPE any.
    ASSIGN COMPONENT c_project_id OF STRUCTURE <line> TO <project>.
    FIELD-SYMBOLS <as4text> TYPE any.
    ASSIGN COMPONENT c_as4text OF STRUCTURE <line> TO <as4text>.

    io_tr->get_list_data(
      IMPORTING
        ev_trkorr   = <trkorr>
        ev_user     = <as4user>
        ev_function = <trfunction>
        ev_status   = <trstatus>
        ev_date     = <as4date>
        ev_project  = <project>
        ev_text     = <as4text>
    ).

    FIELD-SYMBOLS <sys_rc> TYPE any.
    FIELD-SYMBOLS <sys_date> TYPE any.

    DATA lv_fieldname TYPE string.

    FIELD-SYMBOLS <system> LIKE LINE OF mt_systems.
    LOOP AT mt_systems ASSIGNING <system>.
      lv_fieldname = <system>-sysname && '_RC'.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <line> TO <sys_rc>.
      lv_fieldname = <system>-sysname && '_DATE'.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <line> TO <sys_date>.

      DATA ls_action TYPE ctslg_action.
      DATA lv_system TYPE trtarsys.
      lv_system = <system>-sysname.
      ls_action = io_tr->get_systems_status( lv_system ).

      IF ls_action-date <> '00000000'.
        <sys_date> = ls_action-date.

        UNPACK ls_action-rc TO <sys_rc>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.  " append_new_line
  METHOD get_list.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN mp_data->* TO <table>.

    cl_salv_table=>factory(
      IMPORTING r_salv_table  = mo_alv
      CHANGING t_table        = <table>
    ).

    mo_alv->get_functions( )->set_all( abap_true ).

    mo_alv->get_columns( )->set_optimize( ).

    mo_alv->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).

    me->set_layout( mo_alv->get_columns( )->get( ) ).

    mo_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    TRY.
        mo_alv->get_sorts( )->add_sort( 'TRKORR' ).
      CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
    ENDTRY.

    DATA lo_events TYPE REF TO cl_salv_events_table.
    lo_events = mo_alv->get_event( ).
    SET HANDLER me->on_double_click    FOR lo_events.
    SET HANDLER me->on_link_click      FOR lo_events.
    SET HANDLER me->on_added_function  FOR lo_events.

    mo_alv->display( ).

  ENDMETHOD.  " get_list
  METHOD on_double_click.
    DATA lo_tr TYPE REF TO lcl_transport_request.
    TRY.
        lo_tr = lcl_transport_request=>get_request(
                  get_line_trkorr( row )
                ).
      CATCH cx_sy_ref_is_initial.
        RETURN.
    ENDTRY.

    CASE column.
      WHEN c_trkorr     . lo_tr->display_request( ).
      WHEN c_trfunction . lo_tr->display_log( ).
      WHEN c_trstatus   . lo_tr->display_request_settings( ).
      WHEN c_as4user    . lo_tr->display_owner( ).
      WHEN c_as4date    .
      WHEN c_project_id .
      WHEN c_as4text    . lo_tr->display_request_settings( ).
      WHEN OTHERS       .
        READ TABLE me->mt_systems
          TRANSPORTING NO FIELDS
          WITH KEY sysname = column(3).
        IF sy-subrc = 0.
          lcl_transport_request=>display_in_queue(
            iv_system = column(3)
            it_filter = lo_tr->get_filter_for_queue( )
          ).
        ENDIF.
    ENDCASE.  " column
  ENDMETHOD.  " on_double_click
  METHOD on_link_click.

  ENDMETHOD.  " on_link_click
  METHOD on_added_function.

    CASE e_salv_function.
      WHEN 'TRANSP'.
        lcl_transport_request=>display_in_queue(
          iv_system = me->pop_up_system( )
          it_filter = me->get_filter_selected_reqs( mo_alv->get_selections( )->get_selected_rows( ) )
        ).
      WHEN OTHERS.
    ENDCASE.  " e_salv_function

  ENDMETHOD.  " on_added_function
  METHOD pop_up_system.
    rv_system = 'QC1'.
  ENDMETHOD.  " pop_up_system
  METHOD get_filter_selected_reqs.
    DATA lt_filter LIKE rt_filter.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN mp_data->* TO <table>.

    FIELD-SYMBOLS <line> TYPE any.

    FIELD-SYMBOLS <trkorr> TYPE trkorr.

    FIELD-SYMBOLS <row> LIKE LINE OF it_rows.
    LOOP AT it_rows ASSIGNING <row>.
      UNASSIGN <line>.
      READ TABLE <table>
        ASSIGNING <line>
        INDEX <row>.

      CHECK <line> IS ASSIGNED.

      UNASSIGN <trkorr>.
      ASSIGN COMPONENT c_trkorr OF STRUCTURE <line> TO <trkorr>.
      CHECK <trkorr> IS ASSIGNED.

      CLEAR lt_filter.
      TRY.
          lt_filter = lcl_transport_request=>get_request( <trkorr> )->get_filter_for_queue( ).

          APPEND LINES OF lt_filter TO rt_filter.

        CATCH cx_sy_ref_is_initial.
      ENDTRY.

    ENDLOOP.  " rows
    UNASSIGN <row>.
  ENDMETHOD.  " get_filter_selected_reqs
  METHOD set_layout.

    DATA lv_medium_text TYPE scrtext_m.

*    DATA lo_column TYPE REF TO cl_salv_column_table.
*    lo_column ?= mo_alv->get_columns( )->get_column( 'TRKORR' ).
*    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

    FIELD-SYMBOLS <column> LIKE LINE OF it_columns.
    LOOP AT it_columns ASSIGNING <column>.
      "check is a system column
      READ TABLE mt_systems
        TRANSPORTING NO FIELDS
        WITH KEY sysname = <column>-columnname(3).
      IF sy-subrc = 0.
        " it is
        lv_medium_text = <column>-columnname(3).
      ELSE.
        FIELD-SYMBOLS <wboattrt> LIKE LINE OF mt_wboattrt.
        READ TABLE mt_wboattrt
          ASSIGNING <wboattrt>
          WITH KEY attr = <column>-columnname.

        IF sy-subrc = 0.
          lv_medium_text = <wboattrt>-text.

        ELSE.

          IF <column>-columnname = c_as4user.
            lv_medium_text = 'User'.

          ELSE.
            DATA lo_elem TYPE REF TO cl_abap_elemdescr.
            lo_elem ?= cl_abap_elemdescr=>describe_by_name( <column>-columnname ).

            DATA ls_description TYPE dfies.
            ls_description = lo_elem->get_ddic_field( ).

            lv_medium_text = ls_description-scrtext_m.
          ENDIF.
        ENDIF.
      ENDIF.
      <column>-r_column->set_medium_text( lv_medium_text ).

    ENDLOOP.  " columns
  ENDMETHOD.  " set_layout
  METHOD get_line_trkorr.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN mp_data->* TO <table>.

    FIELD-SYMBOLS <line> TYPE any.
    READ TABLE <table>
      ASSIGNING <line>
      INDEX iv_row.

    CHECK <line> IS ASSIGNED.

    FIELD-SYMBOLS <trkorr> TYPE any.
    ASSIGN COMPONENT c_trkorr
      OF STRUCTURE <line>
      TO <trkorr>.

    CHECK <trkorr> IS ASSIGNED.

    rv_trkorr = <trkorr>.

  ENDMETHOD.  " get_line_trkorr
ENDCLASS.                    "lcl_list_transport_requests IMPLEMENTATION

START-OF-SELECTION.
  DATA lo_list_tr TYPE REF TO lcl_list_transport_requests.
  CREATE OBJECT lo_list_tr.
  lo_list_tr->get_data(
    ir_trkorr = s_trkorr[]
    ir_trfunc = s_trfunc[]
    ir_trstat = s_trstat[]
    ir_as4use = s_as4use[]
    ir_as4dat = s_as4dat[]
    ir_as4tex = s_as4tex[]
    ir_projec = s_projec[]
    ir_sysnam = s_sysnam[]
  ).
  lo_list_tr->get_list( ).