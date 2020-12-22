       program-id. MoveCorr as "StagePOEC.MoveCorr".

       data division.
       working-storage section.

       linkage section.
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
         03 somme PIC 9(9)v9(2).

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

       77 indiceTab PIC 99.
       77 tailleTab PIC 99.


       procedure division using clientCourant listeClient indiceTab tailleTab.
           
           move codeClient of clientCourant to codeClientL of client(indiceTab).
           move nom of clientCourant to nomL of client(indiceTab).
           move prenom of clientCourant to prenomL of client(indiceTab).
           move JJ of dateNaissance to JJ of dateNaissanceL(indiceTab).
           move MM of dateNaissance to MM of dateNaissanceL(indiceTab).
           move AAAA of dateNaissance to AAAA of dateNaissanceL(indiceTab).
           move adresse of clientCourant to adresseL of client(indiceTab).
           move codePostal of clientCourant to codePostalL of client(indiceTab).
           move ville of clientCourant to villeL of client(indiceTab).
           add 1 to indiceTab.
           add 1 to tailleTab.
           EXIT program.
       end program MoveCorr.