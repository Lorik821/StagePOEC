       identification division.
       program-id. mainMenu.

       environment division.
       configuration section.

       data division.
       working-storage section.

       procedure division.

       Menu.
           perform Menu-Init.
           perform Menu-Trt until option = 0.
           perform Menu-Fin.

       Menu-Init.
           move 1 to option.
           accept DateSysteme FROM DATE.
           MOVE FUNCTION CURRENT-DATE to WS-CURRENT-DATE-DATA.
      *    Connexion à la base de données
           MOVE
           "Trusted_Connection=yes;Database=stagePOECCobol;server=DESKTOP-G3KGIN3\SQLEXPRESS;factory=System.Data.SqlClient;"
             to cnxDb.
           exec sql
               Connect using :CnxDb
           end-exec.

      *    Choix de l'autocommit
           EXEC SQL
               SET AUTOCOMMIT ON
           End-EXEC.

       Menu-Trt.
           move 0 to Option.
           display menu-principal.
           accept option line 18 col 14.

           evaluate option
               when 1
                   perform rechercheClient
               when 2
                   perform creationClient
           end-evaluate.
       
       end program mainMenu.