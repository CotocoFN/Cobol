       IDENTIFICATION DIVISION.
       PROGRAM-ID.           VIP-1001.
       AUTHOR.               BRUNO.
      ******************************************************************
      *                      PROGRAMA DE CADASTRO E VENDAS             *
      ******************************************************************
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
      ******************************************************************
      *                      WORKING-STORAGE                           *
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  OPC-MENU.
           03 PRINCIPAL-MENU           PIC 9(01).

       01  KEYSTATUS                 PIC 9(04) SPECIAL-NAMES CRT STATUS.
           88 ESC-KEY                  VALUE 27.
           88 ENTER-KEY                VALUE 13.
           88 F1-KEY                   VALUE 1.
           88 F2-KEY                   VALUE 2.
           88 F3-KEY                   VALUE 3.
           88 F4-KEY                   VALUE 4.
      ******************************************************************
      *                      SCREEN-SECTION                            *
      ******************************************************************
       SCREEN SECTION.
       01  WELCOME.
           03 LINE 10 COL 35           VALUE "BEM-VINDO!".
           03 LINE 12 COL 24           VALUE "*** PROGRAM SELL ***".
           03 LINE 20 COL 29           VALUE "AUTHOR: BRUNO NECKEL".
           03 LINE 23 COL 29           VALUE "PRESS ENTER TO CONTINUE".
       01  TELA-MENU.
           03 LINE 10 COL 20           VALUE "MENU-PRINCIPAL".
           03 LINE 12 COL 20           VALUE "(1)CLIENTE ".
           03 LINE 13 COL 20           VALUE "(2)PRODUTOS".
           03 LINE 14 COL 20           VALUE "(3)COMPRAS".
           03 LINE 15 COL 20           VALUE "(4)VENDAS".
           03 LINE 16 COL 20           VALUE "(5)CONSULTA ESTOQUE".
           03 LINE 17 COL 20           VALUE "(6)CONSULTA CADASTROS".
           03 LINE 18 COL 20           VALUE "(7)CONSULTA COMPRA".
           03 LINE 19 COL 20           VALUE "(8)CONSULTA VENDA".
           03 LINE 22 COL 20           VALUE "ESC-ENCERRAR PROGRAMA".
      ******************************************************************
      *                      PROCEDURE-DIVISION                        *
      ******************************************************************
       PROCEDURE DIVISION.
       PD-INICIO.
           DISPLAY WINDOW ERASE.
           DISPLAY WELCOME.
           ACCEPT OMITTED CONTROL KEY IN KEYSTATUS.
       MENU-INICIO.
           PERFORM PD-MENU-PRINCIPAL
           EVALUATE PRINCIPAL-MENU
              WHEN 1
                 CALL "VIP-1002"
                 CANCEL "VIP-1002"
              WHEN 2
                 CALL "VIP-1003"
                 CANCEL "VIP-1003"
              WHEN 3
                 CALL "VIP-2001"
                 CANCEL "VIP-2001"
              WHEN 4
                 CALL "VIP-2002"
                 CANCEL "VIP-2002"
              WHEN 5
                 CALL "VIP-5002"
                 CANCEL "VIP-5002"
              WHEN 6
                 CALL "VIP-5001"
                 CANCEL "VIP-5001"
              WHEN 7
                 CALL "VIP-5003"
                 CANCEL "VIP-5003"
              WHEN 8
                 CALL "VIP-5004"
                 CANCEL "VIP-5004"
              WHEN OTHER
                 IF ESC-KEY
                    GO PD-FIM
                 ELSE
                    DISPLAY MESSAGE BOX "OPCAO INVALIDA"
                END-IF
           END-EVALUATE.
           DISPLAY WINDOW ERASE.
           DISPLAY WELCOME.
           GO MENU-INICIO.
      ******************************************************************
      *                      PD-MENU-PRINCIPAL                         *
      ******************************************************************
       PD-MENU-PRINCIPAL.
           DISPLAY WINDOW ERASE.
           DISPLAY TELA-MENU.
           DISPLAY "OPCAO:"            AT 2120.
           ACCEPT PRINCIPAL-MENU       AT 2126.
       PD-FIM.
           EXIT PROGRAM.
           STOP RUN.