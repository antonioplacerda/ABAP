CLASS zcl_screen_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES tt_id TYPE STANDARD TABLE OF screen-group1 WITH EMPTY KEY.

    METHODS:
      constructor ,
      set_invisible         IMPORTING !it_id TYPE tt_id ,
      set_visible           IMPORTING !it_id TYPE tt_id ,
      set_visible_no_input  IMPORTING !it_id TYPE tt_id ,
      set_no_input          IMPORTING !it_id TYPE tt_id ,
      set_only_visible      IMPORTING !it_id TYPE tt_id ,
      commit_screen .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES st_value TYPE c LENGTH 1.

    CONSTANTS:
      true  TYPE st_value VALUE 1,
      false TYPE st_value VALUE 0.

    DATA:
      mt_screen TYPE SORTED TABLE OF screen
        WITH UNIQUE KEY name
        WITH NON-UNIQUE SORTED KEY group1 COMPONENTS group1.

    METHODS:
      change_this_id
        IMPORTING
          !it_id        TYPE tt_id
          !iv_input     TYPE st_value OPTIONAL
          !iv_invisible TYPE st_value OPTIONAL
          !iv_active    TYPE st_value OPTIONAL,
      change_not_this_id
        IMPORTING
          !it_id        TYPE tt_id
          !iv_input     TYPE st_value OPTIONAL
          !iv_invisible TYPE st_value OPTIONAL
          !iv_active    TYPE st_value OPTIONAL.

ENDCLASS.



CLASS zcl_screen_handler IMPLEMENTATION.


  METHOD change_not_this_id.
    DEFINE declare.
      DATA(lv_&1) = iv_&1.
      translate lv_&1 using '0110'.
    END-OF-DEFINITION.

    declare: input, invisible, active.

    LOOP AT mt_screen ASSIGNING FIELD-SYMBOL(<screen>) .

      IF line_exists( it_id[ table_line = <screen>-group1 ] ).
        change: input, invisible, active.
        
      ELSE.
        change_not: input, invisible, active.
      
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD change_this_id.
    LOOP AT mt_screen ASSIGNING FIELD-SYMBOL(<screen>) .
      CHECK line_exists( it_id[ table_line = <screen>-group1 ] ).

      change: input, invisible, active.

    ENDLOOP.
  ENDMETHOD.


  METHOD commit_screen.

    LOOP AT SCREEN.
      TRY.
          DATA(ls_screen) = mt_screen[ name = screen-name ].
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.
      ls_screen-input = SWITCH #( ls_screen-group3 WHEN 'OPU' OR 'TXT' OR 'TOT' THEN false ELSE ls_screen-input ).
      MODIFY screen FROM ls_screen.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    LOOP AT SCREEN.
      CHECK screen-group1 IS NOT INITIAL.
      INSERT screen INTO TABLE mt_screen.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_invisible.

    change_this_id( it_id = it_id
                    iv_input      = false
                    iv_invisible  = true
                    iv_active     = false
                    ).

  ENDMETHOD.


  METHOD set_no_input.

    change_this_id( it_id = it_id
                    iv_input      = false
                    iv_invisible  = false
                    iv_active     = true
                    ).

  ENDMETHOD.


  METHOD set_only_visible.

    change_not_this_id( it_id = it_id
                        iv_input      = true
                        iv_invisible  = false
                        iv_active     = true
                        ).

  ENDMETHOD.


  METHOD set_visible.

    change_this_id( it_id = it_id
                    iv_input      = true
                    iv_invisible  = false
                    iv_active     = true
                    ).

  ENDMETHOD.


  METHOD set_visible_no_input.

    change_this_id( it_id = it_id
                    iv_input      = false
                    iv_invisible  = false
                    iv_active     = true
                    ).

  ENDMETHOD.
ENDCLASS.