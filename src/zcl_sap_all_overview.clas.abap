"!<p class="shorttext synchronized">A wrapper around CL_SALV_TABLE to display the user overview</p>
CLASS zcl_sap_all_overview DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: constructor,
      add_entry IMPORTING uname          TYPE uname
                          first_name     TYPE ad_namefir
                          last_name      TYPE ad_namelas
                          sap_all_status TYPE REF TO zcl_sap_all_status,
      display,
      close.


methods: callback importing p_task TYPE clike.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF display_type,
             uname      TYPE uname,
             first_name TYPE ad_namefir,
             last_name  TYPE ad_namelas,
             status     TYPE char5,
           END OF display_type,
           BEGIN OF overview_type,
             uname      TYPE uname,
             first_name TYPE ad_namefir,
             last_name  TYPE ad_namelas,
             status     TYPE REF TO zcl_sap_all_status,
           END OF overview_type.

    DATA: display_tab  TYPE TABLE OF display_type,
          overview_tab TYPE TABLE OF overview_type,
          alv          TYPE REF TO cl_salv_table.

    METHODS: set_functions,
      handle_button_click FOR EVENT added_function OF cl_salv_events_table IMPORTING sender e_salv_function,
      fill_display_tab,
      mass_update importing unames type zcl_sap_all_status=>unames  status type char11.


      data: processes type i,
            tasks type i,
            free type i,
            max type i,
            maxt type i,
            update_done type abap_bool.

ENDCLASS.



CLASS zcl_sap_all_overview IMPLEMENTATION.
  METHOD add_entry.

*    DATA(status) = sap_all_status->get_status( ).

    APPEND VALUE #( uname = uname
                    first_name = first_name
                    last_name = last_name
                    status = sap_all_status )
*                    status = SWITCH #( status
*                                       WHEN zcl_sap_all_status=>activated THEN icon_okay
*                                       WHEN zcl_sap_all_status=>deactivated THEN icon_incomplete
*                                       WHEN zcl_sap_all_status=>hidden THEN icon_select_detail ) )
                                       TO overview_tab.

  ENDMETHOD.

  METHOD constructor.
    TRY.
        cl_salv_table=>factory( EXPORTING r_container = cl_gui_container=>default_screen
                                IMPORTING r_salv_table = alv
                                CHANGING  t_table = display_tab ).
      CATCH cx_salv_msg INTO DATA(err).
        MESSAGE err->get_text( ) TYPE 'E'.
    ENDTRY.

    alv->get_selections( )->set_selection_mode(  if_salv_c_selection_mode=>multiple ).

*alv->set_screen_status( pfstatus = 'STATUS' report = 'Z_SAP_ALL_TOOL' set_functions = alv->c_functions_all  ).

    set_functions( ).

    SET HANDLER handle_button_click FOR alv->get_event( ).
  ENDMETHOD.

  METHOD display.


    fill_display_tab( ).
*data x type ref to cl_salv_column_table.
    alv->set_data( CHANGING t_table = display_tab ).
    alv->get_display_settings( )->set_striped_pattern( abap_true ).
    CAST cl_salv_column_table( alv->get_columns( )->get_column( 'UNAME' ) )->set_key( ).

*x ?= alv->get_columns( )->get_column( 'UNAME' ).
*x->set_key( )."set_key_fixation( abap_true )."get_column( 'UNAME' ).
    alv->get_columns( )->set_optimize( abap_true ).
    alv->get_columns( )->get_column( 'STATUS' )->set_long_text( 'status' ).
    alv->get_columns( )->get_column( 'STATUS' )->set_medium_text( 'status' ).
    alv->get_columns( )->get_column( 'STATUS' )->set_short_text( 'status' ).
*    alv->get_columns( )->get_column( 'STATUS' )->set_long_text( 'status' ).
     alv->get_columns( )->get_column( 'FIRST_NAME' )->set_long_text( 'first name' ).
        alv->get_columns( )->get_column( 'FIRST_NAME' )->set_medium_text( 'first name' ).
           alv->get_columns( )->get_column( 'FIRST_NAME' )->set_short_text( 'first name' ).
      alv->get_columns( )->get_column( 'LAST_NAME' )->set_long_text( 'last name' ).
       alv->get_columns( )->get_column( 'LAST_NAME' )->set_medium_text( 'last name' ).
        alv->get_columns( )->get_column( 'LAST_NAME' )->set_short_text( 'last name' ).
       alv->get_columns( )->get_column( 'UNAME' )->set_long_text( 'username' ).
        alv->get_columns( )->get_column( 'UNAME' )->set_medium_text( 'username' ).
         alv->get_columns( )->get_column( 'UNAME' )->set_short_text( 'username' ).
    alv->display( ).
  ENDMETHOD.

  METHOD set_functions.

    DATA(functions) = alv->get_functions( ).

    functions->set_filter( abap_true ).
    functions->set_sort_asc( abap_true ).
    functions->set_sort_desc( abap_true ).
    functions->set_find( abap_true ).
*    functions->set_find_more( abap_true ).

    TRY.
        functions->add_function(
          EXPORTING
            name     = CONV #( zcl_sap_all_status=>activated )               " ALV Funktion
            icon     = CONV #( icon_okay )
            text     = 'Activate'
            tooltip  = 'activate'
            position = 2                 " Funktion Positionierung
        ).
      CATCH cx_root INTO DATA(err).
        MESSAGE err->get_text( ) TYPE 'E'.
    ENDTRY.
    TRY.
        functions->add_function(
        EXPORTING
          name     = CONV #( zcl_sap_all_status=>deactivated )                  " ALV Funktion
          icon     = CONV #( icon_cancel )
          text     = 'Deactivate'
          tooltip  = 'deactivate'
          position = 2                 " Funktion Positionierung
      ).
      CATCH cx_root INTO err.
        MESSAGE err->get_text( ) TYPE 'E'.
    ENDTRY.

    TRY.
        functions->add_function(
        EXPORTING
          name     = CONV #( zcl_sap_all_status=>hidden )                  " ALV Funktion
          icon     = CONV #( icon_select_detail )
          text     = 'Hide'
          tooltip  = 'hide'
          position = 2                 " Funktion Positionierung
      ).
      CATCH cx_root INTO err.
        MESSAGE err->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_button_click.

    DATA(selections) = alv->get_selections( )->get_selected_rows( ).
get RUN TIME FIELD data(tim1).
*    LOOP AT selections INTO DATA(line_number).
* cl_progress_indicator=>progress_indicate( i_text = |updating user | i_processed = 98 i_total = 100 i_output_immediately = abap_true ).

*    cl_progress_indicator=>progress_indicate( i_text = |updating user { overview_tab[ line_number ]-uname }| i_processed = sy-tabix i_total = lines( selections ) i_output_immediately = abap_true ).

*    IF lines( selections ) > 1.
*
      DATA(unames) = VALUE zcl_sap_all_status=>unames( FOR sel IN selections ( overview_tab[ sel ]-uname ) ).

      mass_update( unames = unames status = CONV #( e_salv_function ) ).
*      while zcl_sap_all_status=>update_done = abap_false.
*      endwhile.
*WAIT FOR ASYNCHRONOUS TASKS UNTIL zcl_sap_all_status=>update_done = abap_true.

*    ELSE.
* cl_progress_indicator=>progress_indicate( i_text = |updating user | i_processed = 100 i_total = 100 i_output_immediately = abap_true ).

*      overview_tab[ line_number ]-status->update_status( CONV #( e_salv_function ) ).
*    ENDIF.
*    ENDLOOP.
data time type p length 5 decimals 2.

    get RUN TIME FIELD data(tim2).
    time = ( tim2 - tim1 ) / 1000000.
    message |Updated { lines( unames ) } users in { time } seconds| type 'S'.
*  cl_progress_indicator=>progress_indicate( i_text = |updating user { overview_tab[ line_number ]-uname }| i_processed = 100 i_total = 100 i_output_immediately = abap_true ).


*    MESSAGE |{ lines( selections ) } entries updated| TYPE 'S'.

    fill_display_tab( ).
*    alv->set_data( changing t_table = display_tab ).
    alv->refresh( ).
  ENDMETHOD.

  METHOD fill_display_tab.
    display_tab = VALUE #( FOR entry IN overview_tab (
                               first_name = entry-first_name
                               last_name = entry-last_name
                               uname = entry-uname
                               status = SWITCH #( entry-status->get_status( )
                                   WHEN zcl_sap_all_status=>activated   THEN icon_okay
                                   WHEN zcl_sap_all_status=>deactivated THEN icon_cancel
                                   WHEN zcl_sap_all_status=>hidden      THEN icon_select_detail ) ) ).
  ENDMETHOD.

  METHOD callback.
    DATA(current_task) = CONV i( substring( val = p_task off = strlen( p_task ) - 1 len = 1 ) ).

* cl_progress_indicator=>progress_indicate( i_text = |updating user | i_processed = current_task i_total = maxt i_output_immediately = abap_true ).
    SUBTRACT 1 FROM processes.
    RECEIVE RESULTS FROM FUNCTION 'Z_SAP_ALL_MASS_UPDATE'.
    IF conv i( substring( val = p_task off = strlen( p_task ) - 1 len = 1 ) ) EQ maxt.
      update_done = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD mass_update.

clear: processes,
            tasks,
*            free ,
*            max ,
            maxt ,
            update_done .
  call function 'SPBT_INITIALIZE'
*  EXPORTING
*    group_name                     = space            " PBT-Servergruppnenname
  IMPORTING
    max_pbt_wps                    =  max                " max. Anzahl PBT-Ressourcen im System
    free_pbt_wps                   =  free                " im Moment freie PBT-Ressourcen
  EXCEPTIONS
*    invalid_group_name             = 1                " der PBT-Servergruppenname ist ungültig
*    internal_error                 = 2                " interner Fehler ist aufgetreten ( s. Syslog )
    pbt_env_already_initialized    = 3                " PBT-Umgebung ist für Gruppe schon initialisiert
*    currently_no_resources_avail   = 4                " im Moment sind alle PBT-Ressourcen belegt
*    no_pbt_resources_found         = 5                " keine PBT-Ressourcen im System konfiguriert
*    cant_init_different_pbt_groups = 6                " unterschiedl. PBT-Gruppen nicht initialisierbar
*    others                         = 7
  .
IF sy-subrc = 0.
free = free * '0.75'.
*free = 8.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

maxt = lines( unames ).

clear processes.
*clear processes.
tasks = 1.
    DATA(xxunames) = unames.
*    data counter type i.
*while xxunames is not initial.
    LOOP AT xxunames into data(unamex).
     cl_progress_indicator=>progress_indicate( i_text = |updating users in { free } threads - processed { sy-tabix } / { maxt } | i_processed = sy-tabix i_total = maxt i_output_immediately = abap_true ).

      WHILE processes >= free.
**
        WAIT UNTIL processes < free.
      ENDWHILE.
      DATA(tasknamex) = 'SAP_ALL' && tasks. "&& sy-tabix.
      DATA(xsu) = VALUE z_sap_all_unames( ( value #( uname = unamex ) ) ).
      CALL FUNCTION 'Z_SAP_ALL_MASS_UPDATE' STARTING NEW TASK tasknamex CALLING callback ON END OF TASK
        EXPORTING
          uname = unamex
          status = status.
*          TABLES unames = xsu.
*      IF sy-subrc = 0.
        ADD 1 TO processes.
        add 1 to tasks.
*        delete xxunames where table_line eq unamex.
*        delete fiunames INDEX 1.
*        DELETE xxunames where TABLE_LINE eq unamex.
*      ENDIF.
    ENDLOOP.
*endwhile.
    WAIT FOR ASYNCHRONOUS TASKS UNTIL update_done = abap_true.

    RETURN.
  ENDMETHOD.

  METHOD close.
alv->close_screen( ).
*free alv.
*leave to screen 0.
  ENDMETHOD.

ENDCLASS.
