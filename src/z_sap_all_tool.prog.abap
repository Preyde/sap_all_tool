*&---------------------------------------------------------------------*
*& Report z_add_sap_all_sap_new
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_sap_all_tool.

PARAMETERS: uname TYPE uname MATCHCODE OBJECT prem DEFAULT sy-uname.
SELECTION-SCREEN: SKIP,
BEGIN OF LINE,
 COMMENT 5(50) status,
 END OF LINE,
SKIP,
BEGIN OF LINE,
 PUSHBUTTON 5(20) button1 USER-COMMAND button1_press,
 PUSHBUTTON 30(20) button2 USER-COMMAND button2_press,
 END OF LINE.


DATA: button1_status TYPE char11,
      button2_status TYPE char11,
      sap_all_status TYPE REF TO zcl_sap_all_status.
DATA overview TYPE REF TO zcl_sap_all_overview.

INITIALIZATION.

  %_uname_%_app_%-text = 'Username'.

  SET PF-STATUS 'STATUS'.
  sap_all_status = NEW #( uname ).
  PERFORM build_ui.

AT SELECTION-SCREEN.

  sap_all_status = NEW #( uname ).

  CASE sy-ucomm.
    WHEN 'BUTTON1_PRESS'.
      sap_all_status->update_status( button1_status ).
    WHEN 'BUTTON2_PRESS'.
      sap_all_status->update_status( button2_status ).
    WHEN 'OVERVIEW'.
      IF overview IS NOT BOUND.
        PERFORM display_overview.
      ELSE.
        overview->display( ).
      ENDIF.
      RETURN.
    WHEN 'ACTIVATE'.
      sap_all_status->update_status( zcl_sap_all_status=>activated ).
    WHEN 'DEACTIVATE'.
      sap_all_status->update_status( zcl_sap_all_status=>deactivated ).
    WHEN 'HIDE'.
      sap_all_status->update_status( zcl_sap_all_status=>hidden ).
    WHEN 'BACK'.
      IF overview IS BOUND.
        overview->close( ).
        SUBMIT (sy-cprog) VIA SELECTION-SCREEN.
      ENDIF.
  ENDCASE.

  PERFORM build_ui.


*module %_back input.
*  CASE sy-ucomm.
*    WHEN 'BACK'.
*      IF overview IS BOUND.
*        overview->close( ).
*      ENDIF.
*  ENDCASE.
*endmodule.
  "! Fetches the first and last name of the given username. If the user doesn't exist sy-subrc is set to 4
*FORM fetch_name USING uname TYPE uname CHANGING first_name TYPE ad_namefir last_name TYPE ad_namelas.
*
*  cl_identity_factory=>retrieve(
*    EXPORTING
*      it_bname                = VALUE #( ( CONV #( uname ) ) )
*    IMPORTING
*      et_node_root            = DATA(node)
*  ).
*
*
*  TRY.
*      DATA(user) = node[ 1 ]-idref.
*    CATCH cx_sy_itab_line_not_found.
*      sy-subrc = 4.
*      RETURN.
*  ENDTRY.
*
*  user->if_identity_person~get_personname( IMPORTING es_personname = DATA(personname) ).
*
*  first_name = personname-name_first.
*  last_name = personname-name_last.
*
*ENDFORM.

FORM display_overview.

  overview = NEW zcl_sap_all_overview( ).


  SELECT bname AS uname, name_first, name_last FROM user_addr INTO TABLE @DATA(unames).

*  SELECT bname AS uname FROM usr02 INTO TABLE @DATA(unames).

  DATA(mod) = lines( unames ) / 5.

*  DATA: first_name TYPE ad_namefir,
*        last_name  TYPE ad_namelas.

  LOOP AT unames INTO DATA(uname).

    "shows the progress indicator in 20% steps
    IF sy-tabix MOD mod = 0.
      cl_progress_indicator=>progress_indicate( i_text = 'loading overview'
                                                i_output_immediately = abap_true
                                                i_processed = sy-tabix
                                                i_total = lines( unames ) ).
    ENDIF.

*    PERFORM fetch_name USING uname-uname CHANGING first_name last_name.

    overview->add_entry( uname = uname-uname
                         first_name = uname-name_first
                         last_name = uname-name_last
                         sap_all_status = NEW zcl_sap_all_status( uname-uname ) ).

  ENDLOOP.
  SET PF-STATUS ''.
  overview->display( ).

ENDFORM.

FORM build_ui.

  DATA(stat) = sap_all_status->get_status( ).

  DATA(activate_btn_text) = 'activate SAP_ALL'.

  IF stat = 'ACTIVATED'.
    WRITE icon_okay TO status.
    status = status && ' SAP_ALL is activated'.
    button1_status = zcl_sap_all_status=>hidden.
    button2_status = zcl_sap_all_status=>deactivated.
    button1 = 'hide SAP_ALL'.
    button2 = 'deactivate SAP_ALL'.
  ELSEIF stat = 'HIDDEN'.

    WRITE icon_select_detail TO status.
    status = status && ' SAP_ALL is hidden'.
    button1_status = zcl_sap_all_status=>activated.
    button1 = activate_btn_text.
    button2_status = zcl_sap_all_status=>deactivated.
    button2 = 'deactivate SAP_ALL'.

  ELSEIF stat = 'DEACTIVATED'.

    WRITE icon_cancel TO status.
    status = status && ' SAP_ALL is deactivated'.
    button1_status = zcl_sap_all_status=>hidden.
    button1 = 'hide SAP_ALL'.
    button2 = activate_btn_text.
    button2_status = zcl_sap_all_status=>activated.
  ELSE.
    MESSAGE 'Invalid SAP-ALL-STATUS at BUILD_UI' TYPE 'E'.
  ENDIF.
ENDFORM.
