      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       01 DateSysteme.
         03 Annee PIC 99.
         03 Mois PIC 99.
         03 Jour PIC 99.

       01 WS-CURRENT-DATE-DATA.
         05 WS-CURRENT-DATE.
           10 WS-CURRENT-YEAR PIC 9(4).

       01 dateSystemeJours.
         02 anneesEnJours PIC 9(9).
         02 moisEnJours PIC 9(3).
         02 dateEnJours PIC 9(9).
         02 anneeDateSystemeEnJours PIC 9(9).
         02 moisDateSystemeEnJours PIC 9(3).
         02 dateSystemeEnJours PIC 9(9).
         02 anneesContratEnJour PIC 9(9).
         02 moisContratEnJours PIC 9(3).
         02 dateContratEnJours PIC 9(9).

       77 Option pic X value "".
       77 Option1 pic X value "".
       77 Option2 pic X value "".
       77 Option3 pic X value "".
       77 Option4 pic X value "".
       77 Option5 pic X value "".
       77 Option6 pic X value "".
       77 Option7 pic X value "".
       77 Option8 pic X value "".
       77 Option9 pic X value "".

       77 optionIs PIC x(2).
       77 optionIsContrats PIC x(2).
       77 optionIsSinistre PIC x(2).

       77 NoLigne PIC 99.
       77 NoLigneVisible PIC 9.

       77 CouleurFondEcran pic 99 value 15.
       77 CouleurCaractere pic 99 value 0.

       77 optionCreationClient PIC 9(1).
       77 optionCreationContrat PIC 9(1).
       77 optionVisualisation PIC x(1).
       77 optionVisualisationDetCon PIC x(1).
       77 optionDeclaration PIC 9(1).
       77 optionSuppression PIC 9(1).

       77 optionModificationContrat PIC 9.

       77 optionDetailClient PIC 9(1).
       77 optionDetailContrat PIC 9(1).
       77 optionDetailSinistre PIC 9(1).

       77 Menu-trt-fin pic 9.
       77 Recherche-nom-trt-Fin pic 9.
       77 MenuClient-Trt-fin pic 9.
       77 VisualisationContrat-Trt-fin pic 9.
       77 VisualisationSinistre-Trt-fin pic 9.
       77 CreationClient-Trt-fin pic 9.
       77 Recherche-Client-trt-Fin pic 9.
       77 menu-Visualisation-contrat-fin pic 9.
       77 Recherche-Contrat-trt-Fin pic 9.
       77 Recherche-Sinistre-trt-Fin pic 9.

       77 optionRechercheClientNom PIC x(1).
       77 rechercheClientNom PIC x(50).

       77 optionRechercheContrat PIC x(1).

       01 clientCourant.
         03 codeClient PIC x(36).
         03 nom PIC x(30).
         03 prenom PIC x(30).
         03 dateNaissance.
           04 AAAA PIC 9(4).
           04 MM PIC 9(2).
           04 JJ PIC 9(2).
         03 adresse PIC x(50).
         03 codePostal PIC x(5).
         03 ville PIC x(30).

       01 contratCourant.
         03 codeContrat PIC x(36).
         03 refCodeClient PIC x(36).
         03 sinistresCouverts.
           04 IT PIC 9.
           04 PE PIC 9.
           04 IA PIC 9.
           04 MT PIC 9.
           04 CHM PIC 9.
         03 dateSignature.
           04 AAAA PIC 9(4).
           04 MM PIC 9(2).
           04 JJ PIC 9(2).
         03 validite PIC 9.
         03 franchise.
           04 FRIT PIC 99.
           04 FRPE PIC 99.
           04 FRIA PIC 99.
           04 FRMT PIC 99.
           04 FRCH PIC 99.

       01 sinistreCourant.
         03 codeSinistre PIC x(36).
         03 refCodeClient PIC x(36).
         03 refCodeContrat PIC x(36).
         03 typeSinistre PIC xx.
         03 dateSinistre.
           04 AAAA PIC 9(4).
           04 MM PIC 9(2).
           04 JJ PIC 9(2).

       77 fillerREQSQL PIC x(5).

       01 listeClient.
         02 indice OCCURS 50.
           03 client.
             04 codeClientL PIC x(36).
             04 nomL PIC x(30).
             04 prenomL PIC x(30).
             04 dateNaissanceL.
               05 AAAA PIC x(4).
               05 MM PIC x(2).
               05 JJ PIC x(2).
             04 adresseL PIC x(50).
             04 codePostalL PIC x(5).
             04 villeL PIC x(30).

       01 listeContrat.
         02 indice OCCURS 50.
           03 codecontrat PIC x(36).
           03 refCodeClient PIC x(36).
           03 sinistresCouverts.
             04 IT PIC 9.
             04 PE PIC 9.
             04 IA PIC 9.
             04 MT PIC 9.
             04 CHM PIC 9.
           03 dateSignature.
             04 AAAA PIC 9(4).
             04 MM PIC 9(2).
             04 JJ PIC 9(2).
           03 validite PIC 9.
           03 franchise.
             04 FRIT PIC 99.
             04 FRPE PIC 99.
             04 FRIA PIC 99.
             04 FRMT PIC 99.
             04 FRCH PIC 99.

       01 listeSinistre.
         02 sinistre OCCURS 50.
           03 codeSinistre PIC x(36).
           03 refCodeClient PIC x(36).
           03 refCodeContrat PIC x(36).
           03 typeSinistre PIC xx.
           03 dateSinistre.
             04 AAAA PIC 9(4).
             04 MM PIC 9(2).
             04 JJ PIC 9(2).

       77 indiceTab PIC 99.
       77 tailleTab PIC 99.

       77 indiceClient PIC 9.
       77 indiceContrat PIC 9.

       77 indiceTabContrats PIC 99.
       77 tailleTabContrats PIC 99.
       77 tmpCodeContrat PIC x(8).
       77 NoLigneContrat PIC 99.
       77 NoLigneVisibleContrat PIC 9.
       77 resContrats PIC x(80).
       77 optionVisualisationContrats PIC x.

       77 optionVisualisationSinistres PIC x.
       77 sinistresTmpContrats PIC x(5).
       77 indiceSinistre PIC 99.
       77 tailleTabSinistres PIC 99.
       77 NoLigneSinistre PIC 99.
       77 NoLigneVisibleSinistre PIC 9.
       77 tmpCodeSinistre PIC x(8).
       77 tmpRefCodeClientSinistre PIC x(36).
       77 resSinistres PIC x(80).
       77 indiceTabSinistre PIC 99.
       77 contratCouvreSinistre PIC 9.
       77 contratOK PIC 9.

       77 pageCourante PIC 99.
       77 pagesTotales PIC 99.
       77 tmpPageCourante PIC 99.

       77 pageCouranteContrats PIC 99.
       77 pagesTotalesContrats PIC 99.
       77 tmpPageCouranteContrats PIC 99.

       77 pageCouranteSinistre PIC 99.
       77 pagesTotalesSinistre PIC 99.
       77 tmpPageCouranteSinistres PIC 99.

       01 variablesIntermediaireAgeClient.
         02 differenceAnnee PIC 9(4).
         02 differenceMois PIC 9(2).
         02 differenceJour PIC 9(2).
         02 ageEnJour PIC 9(5).
         02 tmpAgeEnJour PIC 9(5).

       01 variablesIntermediaireContrats.
         02 IT PIC x(1) value 'n'.
         02 PE PIC x(1) value 'n'.
         02 IA PIC x(1) value 'n'.
         02 MT PIC x(1) value 'n'.
         02 CHM PIC x(1) value 'n'.

       77 tmpDateCreaClient PIC x(10).

       01 variablesIntermediairesContratsDates.
         02 totalJours PIC 9(7).
         02 annees PIC 9(3).
         02 mois PIC 9(2).
         02 jours PIC 9(2).
         02 str PIC x(3) value 'oui'.

       01 variablesIntermediaire.
         02 codeClientI PIC x(8).
         02 nomI PIC x(10).
         02 prenomI PIC x(8).
         02 adresseI PIC x(19).
         02 villeI PIC x(10).

       77 res PIC x(80).

      *Les différents sinistres :
      *    IT - Incapacité temporaire
      *    PE - Perte d'emploi
      *    IA - Invalidité
      *    MT - Maternité
      *    CH - Chômage
       01 sinistres.
         02 incapaciteTemporaire PIC x(2) value 'IT'.
         02 perteEmploi PIC x(2) value 'PE'.
         02 invalidite PIC x(2) value 'IA'.
         02 maternite PIC x(2) value 'MT'.
         02 chomage PIC x(2) value 'CH'.

      * Déclaration des variables SQL Server
       77 CNXDB STRING.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           EXEC SQL
               INCLUDE SQLDA
           END-EXEC.

       SCREEN SECTION.
       01 menu-principal background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU PRINCIPAL ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 8 col 5 value "- 1 - Client".
         10 line 9 col 5 value "- 2 - Creation Client".
         10 line 10 col 5 value "- 0 - Quitter ".
         10 line 18 col 5 value "Option : ".

      * Voir la continuite des variables options
       01 Menu-Recherche-nom background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU recherche par nom ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 8 col 5 value "Recherche par nom : ".
         10 line 8 col 25 using Nom PIC X(30).
         10 line 9 col 5 value "Recherche par prenom : ".
         10 line 9 col 28 using Prenom PIC X(30).
         10 line 19 col 5 value "- 0 - pour quitter / - 1 - pour valider ".
         10 line 18 col 5 value "Option : ".
         10 line 18 col 14 PIC 9 from Option1.

       01 menu-recherche-client background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU RECHERCHE CLIENT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 8 col 5 value "Nom : ".
         10 line 8 col 11 using Nom PIC X(30).
         10 line 9 col 5 value "Prenom : ".
         10 line 9 col 14 using Prenom PIC X(30).
         10 line 10 col 5 value "Code client : ".
         10 line 10 col 19 using codeClient PIC X(8).

       01 Menu-Recherche-contrat background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU recherche par Numero de contrat ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 8 col 5 value "Recherche par Numero de contrat : ".
         10 line 8 col 39 using codeClient PIC X(30).
         10 line 19 col 5 value "- 0 - pour quitter / - 1 - pour valider ".
         10 line 18 col 5 value "Option : ".
         10 line 18 col 14 PIC 9 from Option3.

       01 Menu-Recherche-sinistre background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU recherche par Numero de sinistre ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 8 col 5 value "Recherche par Numero de sinistre : ".
         10 line 8 col 40 using codeClient PIC X(30).
         10 line 19 col 5 value "- 0 - pour quitter / - 1 - pour valider ".
         10 line 18 col 5 value "Option : ".
         10 line 18 col 14 PIC 9 from Option4.

       01 menu-Crea-mod-client background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU CLIENT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 8 col 5 value "Nom : ".
         10 line 8 col 11 using Nom PIC X(30).
         10 line 9 col 5 value "Prenom : ".
         10 line 9 col 14 using Prenom PIC X(30).
         10 line 10 col 5 value "Adresse : ".
         10 line 10 col 15 using Adresse PIC X(60).
         10 line 11 col 5 value "Code Postal : ".
         10 line 11 col 19 using CodePostal PIC X(5).
         10 line 12 col 5 value "Ville : ".
         10 line 12 col 13 using Ville PIC X(30).
         10 line 13 col 5 value "Date de naissance : ".
         10 line 13 col 25 using JJ of dateNaissance PIC 9(2).
         10 line 13 col 27 value "/".
         10 line 13 col 28 using MM of dateNaissance PIC 9(2).
         10 line 13 col 30 value "/".
         10 line 13 col 31 using AAAA of dateNaissance PIC 9(4).
         10 line 17 col 5 value "Option : ".
         10 line 17 col 14 using OptionCreationClient PIC 9.
         10 line 19 col 5 value "--------------------------------------------------------------------".
         10 line 20 col 5 value "- 1 - Validation                                                    ".
         10 line 21 col 5 value "- 2 - Annulation                                                    ".
         10 line 22 col 5 value "- 3 - Creation                                                      ".
         10 line 23 col 5 value "- 0 - Quitter                                                       ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

       01 menu-Crea-Mod-contrat background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU CREATION/MODIFICATION CONTRAT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 5 col 2 from codeClient PIC X(8).
         10 line 5 col 11 value "/".
         10 Line 5 Col 12 from Nom PIC X(10).
         10 line 5 col 23 value "/".
         10 Line 5 Col 24 from Prenom PIC X(10).
         10 line 5 col 35 value "/".
         10 Line 5 Col 36 from Ville PIC X(15).
         10 Line 5 Col 60 from JJ of dateNaissance of clientCourant.
         10 line 5 col 62 value "/".
         10 Line 5 Col 63 from MM of dateNaissance of clientCourant.
         10 line 5 col 65 value "/".
         10 Line 5 Col 66 from AAAA of dateNaissance of clientCourant.
      *  10 line 7 col 5 value " Date du contrat : ".
      *  10 Line 7 Col 24 from JJ of contratcourant.
      *  10 line 7 col 27 value "/".
      *  10 Line 7 Col 28 from MM of contratcourant.
      *  10 line 7 col 31 value "/".
      *  10 Line 7 Col 32 from AAAA of contratcourant.
      *  10 line 8 col 5 value " Code Contrat : ".
      *  10 line 8 col 19 from codeContrat of contratCourant PIC X(36).
         10 line 10 col 5 value " Garantie couverte : ".
         10 line 12 col 5 value " IT - Incapacite Temporaire :                 Franchise de :    jours".
         10 line 12 col 35 using IT of contratCourant PIC 9.
         10 line 13 col 5 value " PE - Perte d'emploi        :                 Franchise de :    jours".
         10 line 13 col 35 using PE of contratCourant PIC 9.
         10 line 14 col 5 value " IA - Invalidite            :                 Franchise de :    jours".
         10 line 14 col 35 using IA of contratCourant PIC 9.
         10 line 15 col 5 value " MT - Maternite             :                 Franchise de :    jours".
         10 line 15 col 35 using MT of contratCourant PIC 9.
         10 line 16 col 5 value " CH - Chomage               :                 Franchise de :    jours".
         10 line 16 col 35 using CHM of contratCourant PIC 9.
         10 line 12 col 66 using FRIT of contratCourant PIC 99 value 0.
         10 line 13 col 66 using FRPE of contratCourant PIC 99 value 0.
         10 line 14 col 66 using FRIA of contratCourant PIC 99 value 0.
         10 line 15 col 66 using FRMT of contratCourant PIC 99 value 0.
         10 line 16 col 66 using FRCH of contratCourant PIC 99 value 0.
         10 line 18 col 5 value " Option : ".
         10 line 18 col 15 using optionCreationContrat.
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "- 0 - Menu Precedant                                                ".
         10 line 22 col 5 value "- 1 - Validation                                                    ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

       01 menu-Liste-client background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " LISTE DES CLIENTS ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 6 col 1 value "Nu Client  Nom        Prenom   Adresse             CP    Ville      Naissance  ".
         10 line 7 col 1 value "-------------------------------------------------------------------------------".
         10 line 18 col 1 value " Num Client : ".
         10 line 18 col 15 using optionVisualisation.
         10 line 18 col 20 value " Choix visualisation : ".
         10 line 18 col 43 using optionVisualisationDetCon.
         10 line 18 col 67 value "Page ".
         10 line 18 col 72 from pageCourante.
         10 line 18 col 75 value "de ".
         10 line 18 col 78 from pagesTotales.
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "- Num- Selection du client         - S - Pages suivantes            ".
         10 line 22 col 5 value "- 0 - Menu Principal               - C - Contrat client             ".
         10 line 23 col 5 value "                                   - D - Detail client              ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

       01 Recherche-client-L background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line NoLigne col 10 from codeClient of clientCourant.
         10 line NoLigne col 19 from Nom of clientCourant.
         10 line NoLigne col 32 from Prenom of clientCourant.
         10 line NoLigne col 47 from Ville of clientCourant.
         10 line NoLigne col 63 from JJ of dateNaissance of clientCourant.
         10 line NoLigne col 66 from MM of dateNaissance of clientCourant.
         10 line NoLigne col 69 from AAAA of dateNaissance of clientcourant.

       01 menu-visualisation-contrat background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU VISUALISATION CONTRAT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 5 col 2 using codeClient PIC X(8).
         10 line 5 col 11 value "/".
         10 Line 5 Col 12 using Nom PIC X(10).
         10 line 5 col 23 value "/".
         10 Line 5 Col 24 using Prenom PIC X(10).
         10 line 5 col 35 value "/".
         10 Line 5 Col 36 using Ville PIC X(15).
         10 Line 5 Col 60 using JJ of dateNaissance of clientCourant.
         10 line 5 col 62 value "/".
         10 Line 5 Col 63 using MM of dateNaissance of clientCourant.
         10 line 5 col 65 value "/".
         10 Line 5 Col 66 using AAAA of dateNaissance of clientCourant.
         10 line 7 col 1 value "Nu    contrat         IT      PE      IA      MT      CH      Signature  Valide".
         10 line 17 col 5 value " Num Contrat : ".
         10 line 17 col 67 value "Page ".
         10 line 17 col 72 from pageCourante.
         10 line 17 col 75 value "de ".
         10 line 17 col 78 from pagesTotales.
         10 line 19 col 5 value "--------------------------------------------------------------------".
         10 line 20 col 5 value "- Num- Selection du contrat dans la liste                           ".
         10 line 21 col 5 value "- + - Creation d'un nouveau contrat                                 ".
         10 line 22 col 5 value "- - - Suppression d'un nouveau contrat                              ".
         10 line 23 col 5 value "- 0 - Menu Contrat             - s - Pages suivantes                ".
         10 line 24 col 5 value "--------------------------------------------------------------------".
      * Prévoir une alerte si le client a plus de 65 ans.

       01 Recherche-contrat-L background-color is CouleurFondEcran foreground-color is CouleurCaractere.
      *  10 line NoLigne col 10 from Contrat.
      *  10 line NoLigne col 19 from Garantie.

       01 menu-Creation-contrat background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU CREATION CONTRAT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 5 col 2 using codeClient PIC X(8).
         10 line 5 col 11 value "/".
         10 Line 5 Col 12 using Nom PIC X(10).
         10 line 5 col 23 value "/".
         10 Line 5 Col 24 using Prenom PIC X(10).
         10 line 5 col 35 value "/".
         10 Line 5 Col 36 using Ville PIC X(15).
         10 Line 5 Col 60 using JJ of dateNaissance of clientCourant.
         10 line 5 col 62 value "/".
         10 Line 5 Col 63 using MM of dateNaissance of clientCourant.
         10 line 5 col 65 value "/".
         10 Line 5 Col 66 using AAAA of dateNaissance of clientCourant.
         10 line 7 col 5 value " Date du contrat : ".
         10 line 7 col 40 value " Garantie couverte : ".
         10 line 9 col 5 value "Garantie à couvrir".
         10 line 11 col 5 value " 1 - IT - Incapacité temporaire".
         10 line 12 col 5 value " 2 - PE - Perte d'emploi".
         10 line 13 col 5 value " 3 - IA - Invalidité".
         10 line 14 col 5 value " 4 - MT - Maternité".
         10 line 15 col 5 value " 5 - CH - Chômage".
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "- 12345 - Selection des garanties dans la liste                     ".
         10 line 22 col 5 value "- 0 - Menu Principal                                                ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

       01 menu-detail-client background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU DETAIL CLIENT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 7 col 5 value "Code client       : ".
         10 line 7 col 25 from codeClient PIC X(8).
         10 line 8 col 5 value "Nom               : ".
         10 line 8 col 25 from Nom PIC X(30).
         10 line 9 col 5 value "Prenom            : ".
         10 line 9 col 25 from Prenom PIC X(30).
         10 line 10 col 5 value "Adresse           : ".
         10 line 10 col 25 from Adresse PIC X(60).
         10 line 11 col 5 value "Code Postal       : ".
         10 line 11 col 25 from CodePostal PIC X(5).
         10 line 12 col 5 value "Ville             : ".
         10 line 12 col 25 from Ville PIC X(30).
         10 line 13 col 5 value "Date de naissance : ".
         10 line 13 col 25 from JJ of dateNaissance PIC 9(2).
         10 line 13 col 27 value "/".
         10 line 13 col 28 from MM of dateNaissance PIC 9(2).
         10 line 13 col 30 value "/".
         10 line 13 col 31 from AAAA of dateNaissance PIC 9(4).
         10 line 17 col 5 value "Option : ".
         10 line 17 col 14 PIC 9 from Option5.
         10 line 19 col 5 value "--------------------------------------------------------------------".
         10 line 20 col 5 value "- 1 - Modification                                                  ".
         10 line 21 col 5 value "- 0 - Quitter                                                       ".
         10 line 22 col 5 value "                                                                    ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

       01 menu-detail-contrat background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU DETAIL CONTRAT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 5 col 2 from codeClient PIC X(8).
         10 line 5 col 11 value "/".
         10 Line 5 Col 12 from Nom PIC X(10).
         10 line 5 col 23 value "/".
         10 Line 5 Col 24 from Prenom PIC X(10).
         10 line 5 col 35 value "/".
         10 Line 5 Col 36 from Ville PIC X(15).
         10 Line 5 Col 60 from JJ of dateNaissance of clientCourant.
         10 line 5 col 62 value "/".
         10 Line 5 Col 63 from MM of dateNaissance of clientCourant.
         10 line 5 col 65 value "/".
         10 Line 5 Col 66 from AAAA of dateNaissance of clientCourant.
         10 line 7 col 5 value " Date du contrat : ".
         10 Line 7 Col 24 from JJ of contratcourant.
         10 line 7 col 27 value "/".
         10 Line 7 Col 28 from MM of contratcourant.
         10 line 7 col 31 value "/".
         10 Line 7 Col 32 from AAAA of contratcourant.
         10 line 8 col 5 value " Code Contrat : ".
         10 line 8 col 19 from codeContrat of contratCourant PIC X(36).
         10 line 10 col 5 value " Garantie couverte : ".
         10 line 12 col 5 value " IT - Incapacite Temporaire :                 Franchise de :    jours".
         10 line 12 col 35 from IT of contratCourant PIC 9.
         10 line 12 col 66 from FRIT of contratCourant PIC 99.
         10 line 13 col 5 value " PE - Perte d'emploi        :                 Franchise de :    jours".
         10 line 13 col 35 from PE of contratCourant PIC 9.
         10 line 13 col 66 from FRPE of contratCourant PIC 99.
         10 line 14 col 5 value " IA - Invalidite            :                 Franchise de :    jours".
         10 line 14 col 35 from IA of contratCourant PIC 9.
         10 line 14 col 66 from FRIA of contratCourant PIC 99.
         10 line 15 col 5 value " MT - Maternite             :                 Franchise de :    jours".
         10 line 15 col 35 from MT of contratCourant PIC 9.
         10 line 15 col 66 from FRMT of contratCourant PIC 99.
         10 line 16 col 5 value " CH - Chomage               :                 Franchise de :    jours".
         10 line 16 col 35 from CHM of contratCourant PIC 9.
         10 line 16 col 66 from FRCH of contratCourant PIC 99.
         10 line 18 col 5 value " Option : ".
      *  10 line 18 col 15 using option.
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "- 0 - Menu Precedant                                                ".
         10 line 22 col 5 value "- 1 - Modification                                                  ".
         10 line 23 col 5 value "- 2 - Visualisation sinistres                                       ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

       01 menu-detail-sinistre background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU DETAIL SINISTRE ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 5 col 2 from codeClient PIC X(8).
         10 line 5 col 11 value "/".
         10 Line 5 Col 12 from Nom PIC X(10).
         10 line 5 col 23 value "/".
         10 Line 5 Col 24 from Prenom PIC X(10).
         10 line 5 col 35 value "/".
         10 Line 5 Col 36 from Ville PIC X(15).
         10 Line 5 Col 60 from JJ of dateNaissance of clientCourant.
         10 line 5 col 62 value "/".
         10 Line 5 Col 63 from MM of dateNaissance of clientCourant.
         10 line 5 col 65 value "/".
         10 Line 5 Col 66 from AAAA of dateNaissance of clientCourant.
         10 line 7 col 5 value " Date du sinistre : ".
         10 Line 7 Col 24 from JJ of sinistrecourant.
         10 line 7 col 27 value "/".
         10 Line 7 Col 28 from MM of sinistrecourant.
         10 line 7 col 31 value "/".
         10 Line 7 Col 32 from AAAA of sinistrecourant.
         10 line 8 col 5 value " Code Contrat : ".
         10 line 8 col 21 from refCodeContrat of sinistreCourant PIC X(36).
         10 line 9 col 5 value " Code Client  : ".
         10 line 9 col 21 from refcodeClient of sinistreCourant PIC X(36).
         10 line 10 col 5 value " Code Sinistre : ".
         10 line 10 col 21 from codeSinistre of sinistreCourant PIC X(36).
         10 line 12 col 5 value " Type de sinistre : ".
         10 line 12 col 25 from typeSinistre of sinistreCourant PIC XX.
         10 line 18 col 5 value " Option : ".
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "- 0 - Menu Precedant                                                ".
         10 line 22 col 5 value "                                                                    ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

       01 menu-visualisation-sinistre background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU VISUALISATION SINISTRE ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 5 col 2 using codeClient PIC X(8).
         10 line 5 col 11 value "/".
         10 Line 5 Col 12 using Nom PIC X(10).
         10 line 5 col 23 value "/".
         10 Line 5 Col 24 using Prenom PIC X(10).
         10 line 5 col 35 value "/".
         10 Line 5 Col 36 using Ville PIC X(15).
         10 Line 5 Col 60 using JJ of dateNaissance of clientCourant.
         10 line 5 col 62 value "/".
         10 Line 5 Col 63 using MM of dateNaissance of clientCourant.
         10 line 5 col 65 value "/".
         10 Line 5 Col 66 using AAAA of dateNaissance of clientCourant.
         10 line 7 col 5 value " Num/ type / date  / date sinistre                                   ".
         10 line 16 col 5 value " Options : ".
         10 line 16 col 67 value "Page ".
         10 line 16 col 72 from pageCourante.
         10 line 16 col 75 value "de ".
         10 line 16 col 78 from pagesTotales.
         10 line 18 col 5 value "--------------------------------------------------------------------".
         10 line 19 col 5 value "- Num- Visualisation sinistre             Incapacite temporaire (IT)".
         10 line 20 col 5 value "- s - Pages suivantes                     Perte d'emploi (PE)       ".
         10 line 21 col 5 value "- + - Creation sinistre                   Invalidite (IA)           ".
         10 line 22 col 5 value "- - - Suppression sinistre                Maternite (MA)            ".
         10 line 23 col 5 value "- 0 - Menu Contrat                        Chomage (CH)              ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

       01 Recherche-Sinistre-L background-color is CouleurFondEcran foreground-color is CouleurCaractere.
      *  10 line NoLigne col 10 from Garantie.
         10 line NoLigne col 17 from DateSysteme.
      *  10 line NoLigne col 25 from Date-sinistre.

       01 menu-Declaration-sinistre background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU DECLARATION SINISTRE ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from jour of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from mois of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from annee of DateSysteme.
         10 line 5 col 5 from codeClient PIC X(8).
         10 line 5 col 15 value "/".
         10 Line 5 Col 40 from Nom PIC X(15).
         10 line 5 col 57 value "/".
         10 Line 5 Col 59 from Prenom PIC X(15).
         10 line 8 col 5 value "Date du sinistre : ".
         10 Line 8 Col 24 using JJ of Datesinistre of sinistreCourant.
         10 Line 8 Col 27 using MM of Datesinistre of sinistreCourant.
         10 Line 8 Col 30 using AAAA of Datesinistre of sinistreCourant.
         10 line 12 col 5 value " Type : ".
         10 line 12 col 13 using typeSinistre of sinistreCourant.
         10 line 18 col 5 value " Options : ".
         10 line 18 col 23 using OptionDeclaration.
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "- 1 - Validation du sinistre                                        ".
         10 line 22 col 5 value "- 0 - Menu Contrat                                                  ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      **
      * The main procedure of the program
      **
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

       Menu-Fin.
           STOP RUN.
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Recherche de clients
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       rechercheClient.
           perform rechercheClient-init.
           perform rechercheClient-trt until optionRechercheClientNom = 1.
           perform rechercheClient-fin.

       rechercheClient-init.
           initialize rechercheClientNom.
           move 0 to optionRechercheClientNom.

       rechercheClient-trt.
           move 0 to optionRechercheClientNom.
           initialize clientCourant.
           display menu-recherche-client.
           accept menu-recherche-client.
           if codeClient of clientCourant = '' AND nom of clientCourant = '' AND prenom of clientCourant = '' THEN
               move 1 to optionRechercheClientNom
           else
               STRING codeClient of clientcourant '%' DELIMITED ' ' INTO fillerREQSQL
               STRING fillerREQSQL DELIMITED ' ' INTO fillerREQSQL

               EXEC sql
              declare CursorClient cursor for
              select codeClient, nom, prenom, DAY(dateNaissance), MONTH(dateNaissance), YEAR(dateNaissance), adresse, codePostal, ville
              from clients
              where nom = :clientCourant.nom OR codeClient like :fillerREQSQL OR prenom = :clientCourant.prenom
              order by nom
               END-EXEC

               EXEC sql
             open CursorClient
               END-EXEC
               move 1 to indiceTab
               move 0 to tailleTab
               perform until SQLCODE <> 0
                   EXEC sql
                     fetch CursorClient into :clientCourant.codeClient,
                     :clientCourant.nom, :clientCourant.prenom,
                     :clientCourant.dateNaissance.JJ, :clientCourant.dateNaissance.MM, :clientCourant.dateNaissance.AAAA,
                     :clientCourant.adresse, :clientCourant.codePostal, :clientCourant.ville
                   END-EXEC
                   IF SQLCODE >= 0 THEN
                       move codeClient of clientCourant to codeClientL of client(indiceTab)
                       move nom of clientCourant to nomL of client(indiceTab)
                       move prenom of clientCourant to prenomL of client(indiceTab)
                       move JJ of dateNaissance to JJ of dateNaissanceL(indiceTab)
                       move MM of dateNaissance to MM of dateNaissanceL(indiceTab)
                       move AAAA of dateNaissance to AAAA of dateNaissanceL(indiceTab)
                       move adresse of clientCourant to adresseL of client(indiceTab)
                       move codePostal of clientCourant to codePostalL of client(indiceTab)
                       move ville of clientCourant to villeL of client(indiceTab)
                       add 1 to indiceTab
                       add 1 to tailleTab
                   end-if
               END-PERFORM
      *        EXEC SQL
      *          select codeClient, nom, prenom, DAY(dateNaissance), MONTH(dateNaissance), YEAR(dateNaissance) INTO :clientCourant.codeClient, :clientCourant.nom, :clientCourant.prenom, :clientCourant.dateNaissance.JJ,
      *          :clientCourant.dateNaissance.MM, :clientCourant.dateNaissance.AAAA
      *          from clients where codeClient like :fillerREQSQL order by prenom
      *        end-exec

      *        IF SQLCODE < 0 then
      *            DISPLAY "aucune occurence touvee pour : " rechercheClientNom
      *            ACCEPT option
      *        ELSE
      *            perform menuVisualisationContrats
      *        end-if
               perform visualisationClients
           END-IF.

       rechercheClient-fin.
           EXEC sql
             close CursorClient
           END-EXEC.
           move 1 to option.

      *** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Recherche de sinistres
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       rechercheSinistre.
           perform rechercheSinistre-init.
           perform rechercheSinistre-trt.
           perform rechercheSinistre-fin.

       rechercheSinistre-init.
           continue.

       rechercheSinistre-trt.
           continue.

       rechercheSinistre-fin.
           continue.

      *** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Recherche contrats
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       rechercheContrat.
           perform rechercheContrat-init.
           perform rechercheContrat-trt.
           perform rechercheContrat-fin.

       rechercheContrat-init.
           continue.

       rechercheContrat-trt.
           display Menu-Recherche-contrat.

       rechercheContrat-fin.
           continue.

      ** * ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Visualisation des contrats du client courant
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       menuVisualisationSinistres.
           perform menuVisualisationSinistres-init.
           perform menuVisualisationSinistres-trt until optionVisualisationSinistres = 0.
           perform menuVisualisationSinistres-fin.

       menuVisualisationSinistres-init.
           move 1 to indiceSinistre.
           move 0 to tailleTabSinistres.
           move 1 to optionVisualisationSinistres.

       menuVisualisationSinistres-trt.
           move 0 to optionVisualisationSinistres.
           Display menu-visualisation-sinistre.
      *    Vérification si l'utilisateur souhaite accéder aux sinistres à partir d'un contrat ou s'il fait une recherche par codeSinistre
      *    if codeClient of clientCourant <> '' then
           if tailleTabSinistres = 0 then
               EXEC sql
                 declare CursorSinistres cursor for
                 select codeSinistre, codeClient, codeContrat, typeSinistre, DAY(dateDuSinistre), MONTH(dateDuSinistre), YEAR(dateDuSinistre)
                 from sinistres
                 where codeClient = :clientCourant.codeClient
                 order by dateDuSinistre
               END-EXEC
               EXEC sql
                 open CursorSinistres
               END-EXEC

      *        Traitement pour récupérer les sinistres à partir de la base de données
               perform until SQLCODE <> 0
                   EXEC sql
                     fetch CursorSinistres into :sinistreCourant.codeSinistre, :sinistreCourant.refCodeClient, :sinistreCourant.refCodeContrat, :sinistreCourant.typeSinistre,
                          :sinistreCourant.dateSinistre.JJ, :sinistreCourant.dateSinistre.MM, :sinistreCourant.dateSinistre.AAAA
                   END-EXEC
                   if SQLCODE >= 0 then
                       move corresponding sinistreCourant to sinistre of listeSinistre(indiceSinistre)
                       add 1 to indiceSinistre
                       add 1 to tailleTabSinistres
                   end-if
               end-perform
               EXEC sql
                 close CursorSinistres
               END-EXEC
           end-if.
      *    else

      *    end-if.

      *    Plus qu 'à effectuer l' affichage des données dans le menu-visualisation-sinistres
           move 1 to indiceTabSinistre.
           move 8 to NoLigneSinistre.
           perform until indiceTabSinistre >= tailleTabSinistres
               perform until indiceTabSinistre >= tailleTabSinistres or NoLigneSinistre = 17

                   move codeSinistre of sinistre of listeSinistre(indiceTabSinistre) to tmpCodeSinistre
                   subtract 7 FROM NoLigneSinistre GIVING NoLigneVisibleSinistre

      *            Pour chaque sinistre, il faut vérifier et recharger depuis la base de données les données du client auquel le sinistre est affilié. Dans le cas où l'utilisateur passe par un client pour y afficher la liste qui lui est
      *            dédié, ça ne pose pas de problème et on pourrait se passer de cette vérification ; mais dans le cas d'un simple numéro de sinistre entré, il faut pouvoir afficher à qui ce sinistre est affilié
      *            Au vu de la difficulté de ceci, le code est en commentaire ; si on a le temps, on reviendra dessus
      *            if codeClient of clientCourant <> '' then
      *                move refCodeClient of sinistreCourant(indiceSinistre) to tmpRefCodeClientSinistre
      *
      *                EXEC sql
      *                  select codeClient, nom, prenom, dateNaissance, adresse, codePostal, ville into :clientCourant.codeClient, :clientCourant.nom, :clientCourant.prenom, :clientCourant:dateNaissance, :clientCourant.adresse,
      *                  :clientCourant.codePostal, :clientCourant.ville from clients where codeClient = :tmpRefCodeClientSinistre order by dateSinistre
      *                END-EXEC
      *
      *            end-if

      *            Création et affichage de la ligne du contrat à partir des variables
                   STRING NoLigneVisibleSinistre "              " tmpCodeSinistre "                  " typeSinistre of sinistre(indiceTabSinistre) "               " JJ of sinistre(indiceTabSinistre) "/" MM of sinistre(indiceTabSinistre) "/" AAAA of
                     sinistre(
                       indiceTabSinistre)
                     INTO
                     resSinistres
                   DISPLAY resSinistres line NoLigneSinistre col 1

                   add 1 to indiceTabSinistre
                   add 1 to NoLigneSinistre
               end-perform
           end-perform.
           initialize optionIsSinistre.

      *    Analyse de l'option de l'utilisateur et analyse de la situation en conséquence
           perform until optionIsSinistre = 'ok'
               accept optionVisualisationSinistres line 16 col 17
               if optionVisualisationSinistres = 's' AND pageCouranteSinistre < pagesTotalesSinistre
                   move 'ok' to optionIsSinistre
                   add 1 to pageCouranteSinistre
               else
                   if optionVisualisationSinistres > 0 AND optionVisualisationSinistres <= NoLigneVisibleSinistre
                       move 'ok' to optionIsSinistre
                       subtract 1 from pageCouranteSinistre GIVING tmpPageCouranteSinistres
                       multiply 9 by tmpPageCouranteSinistres GIVING tmpPageCouranteSinistres
                       move FUNCTION NUMVAL (optionVisualisationSinistres) to indiceTabSinistre
                       add tmpPageCouranteSinistres to indiceSinistre
                       move corresponding sinistre of listeSinistre(indiceTabSinistre) to sinistreCourant
                       perform menuVisualisationSinistres-dtl
                       move tailleTabSinistres to indiceTabSinistre
                   else if optionVisualisationSinistres = '+'
                       move 'ok' to optionIsSinistre
                       perform declarationSinistre
                   else if optionVisualisationSinistres = 0
                       move 'ok' to optionIsSinistre
                       move tailleTabSinistres to indiceTabSinistre
                   end-if
           end-perform.

       menuVisualisationSinistres-fin.
           continue.

       menuVisualisationSinistres-dtl.
           display menu-detail-sinistre.
           accept optionDetailSinistre line 18 col 16.

      ** * ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Visualisation des contrats du client courant
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       menuVisualisationContrats.
           perform menuVisualisationContrats-init.
           perform menuVisualisationContrats-trt until optionVisualisationContrats = 0.
           perform menuVisualisationContrats-fin.

       menuVisualisationContrats-init.
           move 1 to indiceTabContrats.
           move 0 to tailleTabContrats.

           move 1 to optionVisualisationContrats.
           move 1 to pageCouranteContrats.

      *    Calcul du nombre de page totale
           divide tailleTabContrats BY 9 GIVING pagesTotalesContrats.
           add 1 to pagesTotalesContrats.
           move 1 to indiceTabContrats.

      *    Déclaration du curseur pour récupérer les contrats du client selectionné
           EXEC sql
              declare CursorContrats cursor for
              select codeContrat, DAY(dateSignature), MONTH(dateSignature), YEAR(dateSignature), IT, PE, IA, MT, CH, FRIT,FRPE,FRIA,FRMT,FRCH
              from contrats
              where codeClient = :clientCourant.codeClient
              order by dateSignature
           END-EXEC.

       menuVisualisationContrats-trt.
           move 0 to optionVisualisationContrats.
           DISPLAY menu-visualisation-contrat.

      *    Ouverture du curseur
           EXEC sql
             open CursorContrats
           END-EXEC.

      *    Execution du traitement ; recuperation des contrats pour le client donné puis rangement dans la liste des contrats (le tableau)
           if tailleTabContrats = 0 then
               perform until SQLCODE <> 0
                   EXEC sql
                     fetch CursorContrats into :contratCourant.codeContrat,
                     :contratCourant.dateSignature.JJ, :contratCourant.dateSignature.MM, :contratCourant.dateSignature.AAAA,
                     :contratCourant.sinistresCouverts.IT, :contratCourant.sinistresCouverts.PE, :contratCourant.sinistresCouverts.IA,
                     :contratCourant.sinistresCouverts.MT, :contratCourant.sinistresCouverts.CHM, :contratCourant.franchise.FRIT,
                     :contratCourant.franchise.FRPE, :contratCourant.franchise.FRIA, :contratCourant.franchise.FRMT,
                     :contratCourant.franchise.FRCH
                   END-EXEC
                   IF SQLCODE >= 0 THEN
                       move codeContrat of contratCourant to codecontrat of indice(indiceTabContrats)
                       move sinistresCouverts of contratCourant to sinistresCouverts of indice(indiceTabContrats)
                       move corresponding dateSignature of contratCourant to dateSignature of indice(indiceTabContrats)
                       move corresponding franchise of contratCourant to franchise of indice(indiceTabContrats)
                       add 1 to indiceTabContrats
                       add 1 to tailleTabContrats
                   end-if
               end-perform
           end-if.

      *    Plus qu 'à effectuer l' affichage ; pour ce faire, il faudra calculer la franchiose du contrat et l 'afficher en sachant que les contrats ont une validité d'un an.
           move 1 to indiceTabContrats.
           move 8 to NoLigneContrat.
           perform until indiceTabContrats >= tailleTabContrats
               perform until indiceTabContrats >= tailleTabContrats or NoLigneContrat = 17

                   move codecontrat of indice(indiceTabContrats) to tmpCodeContrat
                   subtract 7 FROM NoLigneContrat GIVING NoLigneVisibleContrat

      *            Vérification des sinistres couverts, la valeur par défaut des variables temporaires pour écrire la ligne est 'n'
                   IF IT of sinistresCouverts of indice(indiceTabContrats) = 1 THEN
                       move 'o' to IT of variablesIntermediaireContrats
                   else
                       move 'n' to IT of variablesIntermediaireContrats
                   END-IF
                   IF PE of sinistresCouverts of indice(indiceTabContrats) = 1 THEN
                       move 'o' to PE of variablesIntermediaireContrats
                   else
                       move 'n' to PE of variablesIntermediaireContrats
                   END-IF
                   IF IA of sinistresCouverts of indice(indiceTabContrats) = 1 THEN
                       move 'o' to IA of variablesIntermediaireContrats
                   else
                       move 'n' to IA of variablesIntermediaireContrats
                   END-IF
                   IF MT of sinistresCouverts of indice(indiceTabContrats) = 1 THEN
                       move 'o' to MT of variablesIntermediaireContrats
                   else
                       move 'n' to MT of variablesIntermediaireContrats
                   END-IF
                   IF CHM of sinistresCouverts of indice(indiceTabContrats) = 1 THEN
                       move 'o' to CHM of variablesIntermediaireContrats
                   else
                       move 'n' to CHM of variablesIntermediaireContrats
                   END-IF

      *            Calcul pour savoir si le contrat est toujours valide (ne prend pas en compte les années bisectiles) et modifie la variable str qui permet d'afficher si oui ou non le contrat est valide
                   subtract annee of DateSysteme from AAAA of dateSignature of indice(indiceTabContrats) GIVING annees of variablesIntermediairesContratsDates
                   subtract Mois of DateSysteme from MM of dateSignature of indice(indiceTabContrats) GIVING mois of variablesIntermediairesContratsDates
                   subtract jour of DateSysteme from JJ of dateSignature of indice(indiceTabContrats) GIVING jours of variablesIntermediairesContratsDates
                   multiply 365 by annees GIVING totalJours
                   multiply 30.58 by mois of variablesIntermediairesContratsDates GIVING mois of variablesIntermediairesContratsDates
                   add mois of variablesIntermediairesContratsDates jours to totalJours
                   IF totalJours > 365
                       move 0 to validite of indice(indiceTabContrats)
                       move 'non' to str of variablesIntermediairesContratsDates
                   END-IF

      *            Création et affichage de la ligne du contrat
                   STRING NoLigneVisibleContrat "     " tmpCodeContrat "        " IT of variablesIntermediaireContrats "       " PE of variablesIntermediaireContrats "       " IA of variablesIntermediaireContrats "       " MT of
                     variablesIntermediaireContrats "       " CHM of variablesIntermediaireContrats "       " JJ of dateSignature of indice(indiceTabContrats) "/" MM of dateSignature of indice(indiceTabContrats) "/" AAAA of dateSignature of indice(
                       indiceTabContrats) "    " str INTO resContrats
                   DISPLAY resContrats line NoLigneContrat col 1

                   add 1 to indiceTabContrats
                   add 1 to NoLigneContrat
               end-perform
           end-perform.

           initialize optionIsContrats.
           perform until optionIsContrats = 'ok'
               accept optionVisualisationContrats line 17 col 20
               if optionVisualisationContrats = 's' AND pageCouranteContrats < pagesTotalesContrats
                   move 'ok' to optionIsContrats
                   add 1 to pageCouranteContrats
               else
                   if optionVisualisationContrats = '+'
      *                Calcul pour déterminer si le client a le droit de signer un nouveau contrat ; l'âge limite étant de 65 ans
                       subtract AAAA of dateNaissance of clientCourant from WS-CURRENT-YEAR GIVING differenceAnnee of variablesIntermediaireAgeClient
                       subtract MM of dateNaissance of clientCourant from Mois of DateSysteme GIVING differenceMois of variablesIntermediaireAgeClient
                       subtract JJ of dateNaissance of clientCourant from jour of DateSysteme GIVING differenceJour of variablesIntermediaireAgeClient
                       multiply 365 by differenceAnnee GIVING ageEnJour
                       multiply 30.58 by differenceMois GIVING tmpAgeEnJour
                       add tmpAgeEnJour to ageEnJour
                       add differenceJour to ageEnJour

                       if ageEnJour < 23725 then
                           perform creationContrat
                           perform menuVisualisationContrats
                           move tailleTabContrats to indiceTabContrats
                           move 0 to optionVisualisationContrats
                       else
                           display 'Le client est trop age pour souscrire a un contrat. APPUYEZ SUR ENTREE.' line 18 col 5
                           accept optionVisualisationContrats
                           move 'ok' to optionIsContrats
                   else
                       if optionVisualisationContrats > 0 AND optionVisualisationContrats <= NoLigneVisibleContrat AND tailleTabContrats > 0
                           move 'ok' to optionIsContrats
                           subtract 1 from pageCouranteContrats GIVING tmpPageCouranteContrats
                           multiply 9 by tmpPageCouranteContrats GIVING tmpPageCouranteContrats
                           move FUNCTION NUMVAL (optionVisualisationContrats) to indiceContrat
                           add tmpPageCouranteContrats to indiceContrat
      *                move to contratCourant depuis le tableau avec l'indice en question
                           move corresponding indice of listeContrat(indiceContrat) to contratCourant
                           perform menuVisualisationContrats-dtl
                       else
                           if optionVisualisationContrats = 0
                               move 'ok' to optionIsContrats
                               move tailleTabContrats to indiceTabContrats
                           end-if
           end-perform.

       menuVisualisationContrats-dtl.
           display menu-detail-contrat.
           move 0 to optionDetailContrat.
           accept OptionDetailContrat line 18 col 15.
           if optionDetailContrat = 1 then
               perform modificationContrat
               initialize contratCourant
               initialize listeContrat

               move 0 to tailleTabContrats

               move 1 to optionVisualisationContrats
               move 1 to pageCouranteContrats
      *        Calcul du nombre de page totale
               divide tailleTabContrats BY 9 GIVING pagesTotalesContrats
               add 1 to pagesTotalesContrats
               move 1 to indiceTabContrats

           else
               if optionDetailContrat = 2 then
                   perform menuVisualisationSinistres
                   move tailleTabContrats to indiceTabContrats
               end-if.

       menuVisualisationContrats-fin.
           EXEC sql
             close CursorContrats
           END-EXEC.
           initialize contratCourant.
           initialize listeContrat.
           move 1 to optionVisualisationContrats.

      *** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Déclaration sinistre
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       declarationSinistre.
           perform declarationSinistre-init.
           perform declarationSinistre-trt until optionDeclaration = 0.
           perform declarationSinistre-fin.

       declarationSinistre-init.
           move 1 to optionDeclaration.
           initialize sinistreCourant.

       declarationSinistre-trt.
           move 0 to optionDeclaration.
           display menu-Declaration-sinistre.
           accept menu-Declaration-sinistre.
           if optionDeclaration = 1 AND (typeSinistre of sinistreCourant = 'IT' OR typeSinistre of sinistreCourant = 'PE' OR typeSinistre of sinistreCourant = 'IA' OR typeSinistre of sinistreCourant = 'MT' OR typeSinistre of sinistreCourant = 'CH')

      *        Calcul des différentes dates pour contrôle
               multiply AAAA of sinistreCourant by 365 GIVING anneesEnJours
               multiply MM of sinistreCourant by 30.58 GIVING moisEnJours
               add anneesEnJours moisEnJours JJ of sinistreCourant to dateEnJours

               multiply WS-CURRENT-YEAR by 365 GIVING anneeDateSystemeEnJours
               multiply mois of DateSysteme by 30.58 GIVING moisDateSystemeEnJours
               add anneeDateSystemeEnJours moisDateSystemeEnJours jour of DateSysteme to dateSystemeEnJours

               multiply AAAA of contratCourant by 365 GIVING anneesContratEnJour
               multiply MM of contratCourant by 30.58 giving moisContratEnJours
               add anneesContratEnJour moisEnJours JJ of contratCourant to dateContratEnJours

      *        Calcul pour obtenir les sinistres couverts par le contrat du client
      *        if IT of sinistresCouverts of contratCourant = 1 AND typeSinistre of sinistreCourant = 'IT' then
      *            move 1 to contratCouvreSinistre
      *        else if PE of sinistresCouverts of contratCourant = 1 AND typeSinistre of sinistreCourant = 'PE' then
      *            move 1 to contratCouvreSinistre
      *        else if IA of sinistresCouverts of contratCourant = 1 AND typeSinistre of sinistreCourant = 'IA' then
      *            move 1 to contratCouvreSinistre
      *        else if MT of sinistresCouverts of contratCourant = 1 AND typeSinistre of sinistreCourant = 'MT' then
      *            move 1 to contratCouvreSinistre
      *        else if CHM of sinistresCouverts of contratCourant = 1 AND typeSinistre of sinistreCourant = 'CH' then
      *            move 1 to contratCouvreSinistre
      *        end-if
               move 1 to contratCouvreSinistre

      *        Contrôle du fait que la date que l'utilisateur entre soit inférieure à la date du contrat et à la date du jour même si c'est inutile et Vérification si le contrat du client couvre le sinistre déclaré
               if (dateEnJours <= dateSystemeEnJours AND dateEnJours >= dateContratEnJours AND contratCouvreSinistre = 1) then
      *            On utilise ici une variable qui n'a pas vraiement sa place (tmpDateCreaClient), mais cela n'a aucune incidence de l'utiliser ici
                   STRING JJ of sinistreCourant "-" MM of sinistreCourant "-" AAAA of sinistreCourant into tmpDateCreaClient
                   EXEC sql
                     select NEWID() INTO :sinistreCourant.codeSinistre
                   END-EXEC
                   EXEC sql
                       INSERT INTO sinistres (codeSinistre, codeClient, codeContrat, typeSinistre, dateDuSinistre)
      *                VALUE ('FA49C503-1FE4-49BE-90E3-E419FD031C0A', '52215E3A-46B8-4796-AD07-168A03F2F478', 'ACF55041-6AD0-4F5E-AB89-199F16D84904', 'IT', '12-12-2020')
                       VALUE (:sinistreCourant.codeSinistre, :clientCourant.codeClient, :contratCourant.codeContrat, :sinistreCourant.typeSinistre, :tmpDateCreaClient)
                   END-EXEC
                   if SQLCODE >= 0
                       DISPLAY "Declaration du sinistre effectue. APPUYER SUR ENTREE" line 18 col 5
                       accept optionDeclaration
                       move 0 to optionDeclaration
                   else
                       DISPLAY "Decalration du sinistre non effective. APPYUER SUR ENTREE" line 18 col 5
                       accept optionDeclaration
                       move 1 to optionDeclaration
                   end-if
               end-if
           else if optionDeclaration = 0
               continue
           else
               move 1 to optionDeclaration
           end-if.

       declarationSinistre-fin.
           continue.

      *** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Création de contrat
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       creationContrat.
           perform creationContrat-init.
           perform creationContrat-trt until optionCreationContrat = 0.
           perform creationContrat-fin.

       creationContrat-init.
           move 1 to optionCreationContrat.
           move 0 to contratCourant.

      *    On réinitialisation le contrat courant pour un créer un nouveau
           initialize contratCourant.

       creationContrat-trt.
           move 0 to optionCreationContrat.

      *    On récupère les données de l'utilisateur pour le nouveau contrat
           display menu-Crea-mod-contrat.
           accept menu-Crea-mod-contrat.

      *    Ici on contrôle que le nouveau contrat poss_de au moins un sinistre ; cela n'a aucun sens de créer un contrat qui ne couvre aucun sinistre 
           if IT of contratCourant = 1
               move 1 to contratOK
           else if PE of contratCourant = 1
               move 1 to contratOK
           else if IA of contratCourant = 1
               move 1 to contratOK
           else if MT of contratCourant = 1
               move 1 to contratOK
           else if CHM of contratCourant = 1
               move 1 to contratOK
           end-if.

      *    Condition principale qui contrôle les champs saisis par l'utilisateur
           if optionCreationContrat = 1 AND (IT of sinistresCouverts of contratCourant = 0 OR IT of sinistresCouverts of contratCourant = 1) AND (PE of sinistresCouverts of contratCourant = 0 OR PE of sinistresCouverts of contratCourant = 1) AND
             (IA of sinistresCouverts of contratCourant = 0 OR IA of sinistresCouverts of contratCourant = 1) AND (MT of sinistresCouverts of contratCourant = 0 OR MT of sinistresCouverts of contratCourant = 1) AND (CHM of sinistresCouverts of
             contratCourant = 0 OR CHM of sinistresCouverts of contratCourant = 1) AND contratOK = 1

      *        Formatage de la date du jour
               STRING jour of DateSysteme "-" Mois of DateSysteme "-" Annee of DateSysteme INTO tmpDateCreaClient

      *        Execution du code SQL pour insérer le nouveau contrat
               EXEC sql
                 select newid() into :contratCourant.codeContrat
               END-EXEC
               EXEC sql
                   INSERT INTO Contrats (codeContrat, codeClient, IT, PE, IA, MT, CH, FRIT, FRPE, FRIA, FRMT, FRCH, dateSignature)
                   VALUES (:contratCourant.codeContrat, :clientCourant.codeClient, :contratCourant.sinistresCouverts.IT, :contratCourant.sinistresCouverts.PE, :contratCourant.sinistresCouverts.IA, :contratCourant.sinistresCouverts.MT,
                   :contratCourant.sinistresCouverts.CHM, :contratCourant.franchise.FRIT, :contratCourant.franchise.FRPE, :contratCourant.franchise.FRIA, :contratCourant.franchise.FRMT, :contratCourant.franchise.FRCH, :tmpDateCreaClient)
               END-EXEC

      *        Affichage du résultat, pour savoir si le contrat a bien été créé
               if SQLCODE >= 0
                   DISPLAY "Creation du contrat reussie." line 19 col 5
                   accept optionCreationContrat
                   move 0 to optionCreationContrat
               else
                   DISPLAY "Creation du contrat echouee." line 19 col 5
                   accept optionCreationContrat
                   move 1 to optionCreationContrat
               end-if.

       creationContrat-fin.
           continue.

      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Modifcation des contrats
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       modificationContrat.
           perform modificationContrat-init.
           perform modificationContrat-trt until optionModificationContrat = 0.
           perform modificationContrat-fin.

       modificationContrat-init.
           move 1 to optionModificationContrat.

       modificationContrat-trt.
           move 0 to optionModificationContrat.
           display menu-Crea-Mod-contrat.
           accept menu-Crea-Mod-contrat.
           if optionCreationContrat = 1 AND (IT of sinistresCouverts of contratCourant = 0 OR IT of sinistresCouverts of contratCourant = 1) AND (PE of sinistresCouverts of contratCourant = 0 OR PE of sinistresCouverts of contratCourant = 1) AND
             (IA of sinistresCouverts of contratCourant = 0 OR IA of sinistresCouverts of contratCourant = 1) AND (MT of sinistresCouverts of contratCourant = 0 OR MT of sinistresCouverts of contratCourant = 1) AND (CHM of sinistresCouverts of
             contratCourant = 0 OR CHM of sinistresCouverts of contratCourant = 1)
               STRING jour of DateSysteme "-" Mois of DateSysteme "-" Annee of DateSysteme INTO tmpDateCreaClient
               EXEC sql
                 UPDATE contrats
                 set   IT = :contratCourant.IT,
                       PE = :contratCourant.PE,
                       IA = :contratCourant.IA,
                       MT = :contratCourant.MT,
                       CH = :contratCourant.CHM,
                       FRIT = :contratCourant.FRIT,
                       FRPE = :contratCourant.FRPE,
                       FRIA = :contratCourant.FRIA,
                       FRMT = :contratCourant.FRMT,
                       FRCH = :contratCourant.FRCH
               where codeContrat = :contratCourant.codeContrat
               END-EXEC
               if SQLCODE >= 0
                   DISPLAY "Modification du contrat reussie. APPUYEZ SUR ENTREE" line 18 col 5
                   accept optionModificationContrat
                   move 0 to optionModificationContrat

               else
                   DISPLAY "Modification du contrat echouee. APPUYEZ SUR ENTREE" line 18 col 5
                   accept optionModificationContrat
                   move 1 to optionModificationContrat
               end-if
           else
               if optionModificationContrat = 2 OR optionModificationContrat = 0
                   move 0 to optionModificationContrat
               else
                   move 1 to optionModificationContrat
               end-if.

       modificationContrat-fin.
           continue.

      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Modification de sinistre
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *



      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Création de client
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       creationClient.
           perform creationClient-init.
           perform creationClient-trt until optionCreationClient = 0.
           perform creationClient-fin.

       creationClient-init.
           move 1 to optionCreationClient.
           initialize clientCourant.
           continue.

       creationClient-trt.
           move 0 to optionCreationClient.
           display menu-Crea-mod-client.
           accept menu-Crea-mod-client.
           if optionCreationClient = 1 AND nom of clientCourant <> '' AND prenom of clientCourant <> '' AND adresse of clientCourant <> '' AND codePostal of clientCourant <> '' AND ville of clientCourant <> '' AND (JJ of dateNaissance of clientCourant >=
             1 AND JJ of dateNaissance of clientCourant <= 31) AND (MM of dateNaissance of clientCourant >= 1 AND MM of dateNaissance of clientCourant <= 12) AND (AAAA of dateNaissance of clientCourant >= 1900 AND AAAA of dateNaissance of clientCourant <=
             WS-CURRENT-YEAR) then
               STRING JJ of dateNaissance of clientCourant "-" MM of dateNaissance of clientCourant "-" AAAA of dateNaissance of clientCourant INTO tmpDateCreaClient
               EXEC SQL
                   select newid() into :clientCourant.codeClient
               END-EXEC
               EXEC sql
                 INSERT INTO Clients (CodeClient, nom, prenom, dateNaissance, adresse, codePostal, ville)
                 VALUES (:clientCourant.codeClient, :clientCourant.nom, :clientCourant.prenom, :tmpDateCreaClient, :clientCourant.adresse, :clientCourant.codePostal, :clientCourant.ville)
               END-EXEC
               if SQLCODE >= 0
                   Display "Creation du client reussie." line 18 col 5
                   accept optionCreationClient
                   move 0 to optionCreationClient
               else
                   Display "Creation du client echouee." line 18 col 5
                   accept optionCreationClient
                   move 1 to optionCreationClient
               end-if

           else
               if optionCreationClient = 0
                   move 0 to optionCreationClient
               else
                   move 1 to optionCreationClient
               end-if.

       creationClient-fin.
           continue.
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Modification des clients
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       modificationClient.
           perform modificationClient-init.
           perform modificationClient-trt until optionCreationClient = 0.
           perform modificationClient-fin.

       modificationClient-init.
           move 1 to optionCreationClient.
           continue.

       modificationClient-trt.
           move 0 to optionCreationClient.
           display menu-Crea-mod-client.
           accept menu-Crea-mod-client.
           if optionCreationClient = 1 AND nom of clientCourant <> '' AND prenom of clientCourant <> '' AND adresse of clientCourant <> '' AND codePostal of clientCourant <> '' AND ville of clientCourant <> '' AND (JJ of dateNaissance of clientCourant >=
             1 AND JJ of dateNaissance of clientCourant <= 31) AND (MM of dateNaissance of clientCourant >= 1 AND MM of dateNaissance of clientCourant <= 12) AND (AAAA of dateNaissance of clientCourant >= 1900 AND AAAA of dateNaissance of clientCourant <=
             annee of DateSysteme) then
               STRING JJ of dateNaissance of clientCourant "-" MM of dateNaissance of clientCourant "-" AAAA of dateNaissance of clientCourant INTO tmpDateCreaClient
               EXEC sql
                   UPDATE clients
                   set nom = :clientCourant.nom,
                       prenom = :clientCourant.prenom,
                       dateNaissance = :tmpDateCreaClient,
                       adresse = :clientCourant.adresse,
                       codePostal = :clientCourant.codePostal,
                       ville = :clientCourant.ville
                   where codeClient = :clientCourant.codeClient
               END-EXEC
               if SQLCODE >= 0
                   Display "Modification du client reussie." line 18 col 5
                   accept optionCreationClient
                   move 0 to optionCreationClient
               else
                   Display "Modification du client echouee." line 18 col 5
                   accept optionCreationClient
                   move 1 to optionCreationClient
               end-if
           else
               if optionCreationClient = 2 OR optionCreationClient = 0
                   move 0 to optionCreationClient
               else
                   move 1 to optionCreationClient
               end-if.

       modificationClient-fin.
           continue.
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Visualiation liste des clients
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       visualisationClients.
           perform visualisationClients-init.
           perform visualisationClients-trt until optionVisualisation = 0.
           perform visualisationClients-fin.

       visualisationClients-init.
           move 1 to optionVisualisation.
           move 1 to pageCourante.
           move 'c' to optionVisualisationDetCon.

      *    Calcul du nombre de page totale
           divide tailleTab BY 9 GIVING pagesTotales.
           add 1 to pagesTotales.

           move 1 to indiceTab.

       visualisationClients-trt.
           move 0 to optionVisualisation.
           perform until indiceTab = tailleTab
               DISPLAY menu-Liste-client
               move 8 to NoLigne
               initialize optionIs
               perform until NoLigne = 17 OR indiceTab = tailleTab
                   move codeClientL(indiceTab) to codeClientI
                   move nomL(indiceTab) to nomI
                   move prenomL(indiceTab) to prenomI
                   move adresseL(indiceTab) to adresseI
                   move villeL(indiceTab) to villeI
                   subtract 7 FROM NoLigne GIVING NoLigneVisible
                   STRING NoLigneVisible " " codeClientI " " nomI " " prenomI " " adresseI " " codePostalL(indiceTab) " " villeI " " JJ of dateNaissanceL(indiceTab) "/" MM of dateNaissanceL(indiceTab) "/" AAAA of dateNaissanceL(indiceTab) INTO res
                   DISPLAY res line NoLigne col 1
                   ADD 1 TO indiceTab
                   ADD 1 TO NoLigne
               end-perform

               perform until optionIs = 'ok'
      *            accept optionVisualisation line 18 col 14 //optionVisualisationDetCon
                   accept menu-Liste-client
                   if optionVisualisation = 's' AND pageCourante < pagesTotales
                       move 'ok' to optionIs
                       ADD 1 to pageCourante
                   else
                       if optionVisualisation > 0 AND optionVisualisation <= NoLigneVisible AND (optionVisualisationDetCon = 'c' OR optionVisualisationDetCon = 'd')
      *                Ici il va falloir s'arranger pour afficher le détail d'un client ou d'afficher la liste de ses contrats
                           move 'ok' to optionIs
                           subtract 1 FROM pageCourante GIVING tmpPageCourante
                           multiply 9 by tmpPageCourante GIVING tmpPageCourante
                           move FUNCTION NUMVAL (optionVisualisation) to indiceClient
                           add tmpPageCourante to indiceClient
      *                move corresponding client(indiceClient) to clientCourant
                           move codeClientL of client(indiceClient) to codeClient of clientCourant
                           move nomL of client(indiceClient) to nom of clientCourant
                           move prenomL of client(indiceClient) to prenom of clientCourant
                           move adresseL of client(indiceClient) to adresse of clientCourant
                           move codePostalL of client(indiceClient) to codePostal of clientcourant
                           move villeL of client(indiceClient) to ville of clientCourant
                           move corresponding dateNaissanceL(indiceClient) to dateNaissance
      *
      *                Appel la visualisation des contrats pour le client sélectionné si l'utilisateur a choisi l'option c ; appel le détail du client sélectionné sur l'utilisateur a choisi l'option d
      *
                           if optionVisualisationDetCon = 'c' then
                               perform menuVisualisationContrats
                               move tailleTab to indiceTab
                           else
                               if optionVisualisationDetCon = 'd' then
                                   perform menuDetailClient
                                   move tailleTab to indiceTab
                               end-if
                           end-if
                       else
                           if optionVisualisation = 0
                               move 'ok' to optionIs
                               move tailleTab to indiceTab
               end-perform
           end-perform.

       visualisationClients-fin.
           move 1 to optionVisualisation.
           continue.

       menuDetailClient.
           display menu-detail-client.
           accept optionDetailClient line 17 col 14.
           if optionDetailClient = 1
               perform modificationClient
           else
               continue
           end-if.

      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Déclaration de sinistres
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       declaration.
           perform declaration-init.
           perform declaration-trt until optionDeclaration = 0.
           perform declaration-fin.

       declaration-init.
           move 1 to optionDeclaration.
           continue.

       declaration-trt.
           move 0 to optionDeclaration.
           continue.

       declaration-fin.
           continue.

      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * Suppression de clients, contrats ou sinistres
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       suppression.
           perform suppression-init.
           perform suppression-trt until optionSuppression = 0.
           perform suppression-fin.

       suppression-init.
           move 1 to optionSuppression.
           continue.

       suppression-trt.
           move 0 to optionSuppression.
           continue.

       suppression-fin.
           continue.

      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.
