*&---------------------------------------------------------------------*
*& Report ZSET_INDISP_MULT_REG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zset_indisp_mult_reg.

DATA:
  gt_zbv_disponivel TYPE TABLE OF zbv_disponivel,
  gs_zbv_disponivel TYPE zbv_disponivel.

DATA:
  tempo_minutos    TYPE mcwmit-be_ae,
  tempo_disponivel TYPE p DECIMALS 3.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(18) TEXT-002.
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_co_tec TYPE zbv_disponivel-consultor_tecnico AS LISTBOX VISIBLE LENGTH 20.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 21(25) btn1 USER-COMMAND btn VISIBLE LENGTH 25.
  SELECTION-SCREEN END OF LINE.
*  p_dispon TYPE zbv_disponivel-disponivel AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK blk1.

AT SELECTION-SCREEN OUTPUT.
  btn1 = TEXT-003.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'BTN'.
      PERFORM relatar_indisponibilidade.
    WHEN OTHERS.
  ENDCASE.



*&---------------------------------------------------------------------*
*& Form relatar_disponibilidade
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM relatar_indisponibilidade .

  "Validação para verificar se o usuário possui permissão de execução
  AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '16'.
  CASE sy-subrc.
    WHEN 0.

      SELECT * FROM zbv_disponivel
          INTO TABLE gt_zbv_disponivel
            WHERE consultor_tecnico = p_co_tec
            AND   data_registro = sy-datum.

      IF sy-subrc = 0.
        "Verifica a quantidade de linhas(registros) que tem na tabela
        DESCRIBE TABLE gt_zbv_disponivel LINES DATA(linha).

        "Le o ultimo registro da tabela
        READ TABLE gt_zbv_disponivel ASSIGNING FIELD-SYMBOL(<fs_zbv_disponivel>) INDEX linha.

        "Verifica o status atual do registo
        IF <fs_zbv_disponivel>-disponivel = abap_false. "p_dispon.
          MESSAGE 'Não é possível repetir o mesmo status / Você já está Indisponível' TYPE 'W'.
        ENDIF.

        "Se o status não foi indisponivel, alterar os dados e encerrar o registro.

        <fs_zbv_disponivel>-disponivel = abap_false.
        <fs_zbv_disponivel>-data_fim_disp = sy-datum. "Talvez nao precise (redundante)
        <fs_zbv_disponivel>-hora_fim_disp = sy-uzeit.

        "Fazer o calculo do tempo de disponibilidade
        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
          EXPORTING
            date_from       = <fs_zbv_disponivel>-data_ini_disp "gs_zbv_disponivel-data_ini_disp
            date_to         = <fs_zbv_disponivel>-data_fim_disp "gs_zbv_disponivel-data_fim_disp
            time_from       = <fs_zbv_disponivel>-hora_ini_disp   "gs_zbv_disponivel-hora_ini_disp
            time_to         = <fs_zbv_disponivel>-hora_fim_disp   "gs_zbv_disponivel-hora_fim_disp
          IMPORTING
            delta_time      = tempo_minutos
*           DELTA_UNIT      =
          EXCEPTIONS
            from_greater_to = 1
            OTHERS          = 2.

        "Tempo setado para teste
*        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
*          EXPORTING
*            date_from  = '20240818' "gs_zbv_disponivel-data_ini_disp
*            date_to    = '20240820' "gs_zbv_disponivel-data_fim_disp
*            time_from  = '100000'   "gs_zbv_disponivel-hora_ini_disp
*            time_to    = '110000'   "gs_zbv_disponivel-hora_fim_disp
*          IMPORTING
*            delta_time = tempo_minutos
**           DELTA_UNIT =
**   EXCEPTIONS
**           FROM_GREATER_TO       = 1
**           OTHERS     = 2
*          .

        "Verifica se a função pegou corretamento o tempo
        IF sy-subrc <> 0.
          MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          tempo_disponivel = tempo_minutos / 60.
          <fs_zbv_disponivel>-tempo_disponivel = tempo_disponivel.
*      gs_zbv_disponivel-tempo_disponivel = tempo_disponivel.

          MODIFY zbv_disponivel FROM <fs_zbv_disponivel>.
          FREE: <fs_zbv_disponivel>.
        ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE

      ENDIF. "sy-subrc = 0. Validação do Select
    WHEN OTHERS.
      MESSAGE 'Sem permissão a transação!' TYPE 'E'.
  ENDCASE.

ENDFORM.
