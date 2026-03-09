       IDENTIFICATION DIVISION.
       PROGRAM-ID.           VIP-1002.
       AUTHOR.               BRUNO.
      ******************************************************************
      *                      CADASTRO DE CLIENTE FISICO                *
      ******************************************************************
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "VIP-CLIFOR.SL".
       DATA DIVISION.
       FILE SECTION.
           COPY "VIP-CLIFOR.FD".
      ******************************************************************
      *                      WORKING-STORAGE                           *
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  STATUS-DATA.
           05 FS-VIP-CLIFOR            PIC X(02).
              88 VALID-VIP-CLIFOR      VALUE "00" THRU "09".

       01  CODIGO                      PIC ZZ.
       01  NM-CODIGO                   PIC X(10).

       01  CLIENTE.
           03 NOME                     PIC X(60).
           03 CPF-CNPJ                 PIC Z99.999.999-99.
           03 ENDERECO                 PIC X(20).
           03 BAIRRO                   PIC X(20).
           03 CIDADE                   PIC X(20).
           03 ESTADO                   PIC X(20).
           03 CEP                      PIC Z99.999.999.
       01  OPC.
           03 COD-BUSCA                PIC 9(01).
           03 OPC-CANCELAR             PIC X(01).

       01  KEYSTATUS                 PIC 9(04) SPECIAL-NAMES CRT STATUS.
           88 ESC-KEY                  VALUE 27.
           88 ENTER-KEY                VALUE 13.
           88 F1-KEY                   VALUE 1.
           88 F2-KEY                   VALUE 2.
           88 F3-KEY                   VALUE 3.
           88 F4-KEY                   VALUE 4.
       LINKAGE SECTION.
       01  LINK-PARAMETROS.
           03 LINK-CD-CODIGO           PIC 9(02).
           03 LINK-NM                  PIC X(10).

      ******************************************************************
      *                      SCREEN-SECTION                            *
      ******************************************************************
       SCREEN SECTION.
       01  CAD-CLIENTE-FIS.
           03 LINE 08 COL 20           VALUE "CLIENTE FISICO".
           03 LINE 10 COL 20           VALUE "NOME:".
           03 LINE 11 COL 20           VALUE "CPF:".
           03 LINE 12 COL 20           VALUE "ENDERECO:".
           03 LINE 13 COL 20           VALUE "BAIRRO:".
           03 LINE 14 COL 20           VALUE "CIDADE:".
           03 LINE 15 COL 20           VALUE "ESTADO:".
           03 LINE 16 COL 20           VALUE "CEP:".

       01  CAD-CONFIRMAR.
           03 LINE 20 COL 15           VALUE "CONFIRMAR <S>/<N>:".
           03 LINE 21 COL 29           VALUE "OPCAO:".
      ******************************************************************
      *                          PROCEDURE DIVISION                    *
      ******************************************************************
       PROCEDURE DIVISION USING LINK-PARAMETROS.
       PD-CAD-FISICO.
           OPEN I-O VIP-CLIFOR.
           DISPLAY WINDOW ERASE.
           DISPLAY CAD-CLIENTE-FIS.
           ACCEPT NOME                 AT 1025.
           IF ESC-KEY
              GO FIM-CAD-FISICO
           END-IF.
           ACCEPT CPF-CNPJ             AT 1124.
           ACCEPT ENDERECO             AT 1229.
           ACCEPT BAIRRO               AT 1327.
           ACCEPT CIDADE               AT 1427.
           ACCEPT ESTADO               AT 1527.
           ACCEPT CEP                  AT 1624.
           DISPLAY CAD-CONFIRMAR.
           ACCEPT OPC-CANCELAR         AT 2135
           IF OPC-CANCELAR EQUAL "S" OR
              OPC-CANCELAR EQUAL "s"
              INITIALIZE REG-CLIFOR
              MOVE HIGH-VALUE          TO CLIFOR-CD-CLIFOR
              START VIP-CLIFOR KEY < CLIFOR-CHAVE
              READ VIP-CLIFOR NEXT WITH NO LOCK
              DISPLAY MESSAGE BOX "GRAVADO COM SUCESSO!"
              IF NOT VALID-VIP-CLIFOR
                 MOVE 1                TO CLIFOR-CD-CLIFOR
              ELSE
                 ADD 1                 TO CLIFOR-CD-CLIFOR
              END-IF
              PERFORM MOVER-TELA-PARA-REGISTRO
              WRITE REG-CLIFOR
              IF NOT VALID-VIP-CLIFOR
              DISPLAY MESSAGE BOX "NAO FOI POSSIVEL GRAVAR!"
                            X"0A" "FILE STATUS:" FS-VIP-CLIFOR
              END-IF
           END-IF.
       FIM-CAD-FISICO.
           CLOSE VIP-CLIFOR.
           EXIT PROGRAM.
      ******************************************************************
      *                      FUNCTIONS                                 *
      ******************************************************************
       MOVER-TELA-PARA-REGISTRO.
           MOVE NOME                   TO NM-CODIGO.
           MOVE NM-CODIGO              TO CLIFOR-NM-CLIFOR.
           MOVE CPF-CNPJ               TO CLIFOR-CPF-CNPJ.
           MOVE ENDERECO               TO CLIFOR-ENDERECO.
           MOVE BAIRRO                 TO CLIFOR-BAIRRO.
           MOVE CEP                    TO CLIFOR-CEP.
           MOVE ESTADO                 TO CLIFOR-ESTADO.
           MOVE CIDADE                 TO CLIFOR-CIDADE.
