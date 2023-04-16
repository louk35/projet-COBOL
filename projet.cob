       IDENTIFICATION DIVISION.
      
       PROGRAM-ID. projet.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           select fmembre assign to "membre.dat"
           organization indexed
           access mode is dynamic
           record key is fm_idmembre
           alternate record key is fm_statut  WITH DUPLICATES
           file status is cr_fmembre.

           select finscription assign to "inscriptionActivite.dat"
           organization indexed
           access mode is dynamic
           record key is fi_idinscription
           alternate record key is fi_idmembre  WITH DUPLICATES
           alternate record key is fi_idactivite  WITH DUPLICATES
           file status is cr_finscription.

           select factivite assign to "activite.dat"
           organization indexed
           access mode is dynamic
           record key is fa_idactivite
           alternate record key is fa_type  WITH DUPLICATES
           alternate record key is fa_encadrant  WITH DUPLICATES
           alternate record key is fa_semaine  WITH DUPLICATES
           alternate record key is fa_jour WITH DUPLICATES
           file status is cr_factivite.

           select ftype assign to "type.dat"
           organization indexed
           access mode is dynamic
           record key is ft_type
           file status is cr_ftype.
     
       DATA DIVISION.
  
       FILE SECTION.
       FD fmembre.
       01 tamp_fmembre.
           02 fm_idmembre PIC 9(3).
           02 fm_nom PIC A(20).
           02 fm_prenom PIC A(20).
           02 fm_telephone PIC X(20).
           02 fm_adresse PIC X(30).
           02 fm_statut PIC A(20).

       FD finscription.
       01 tamp_finscription.
           02 fi_idinscription.
	       03 fi_idmembre PIC 9(2).
               03 fi_idactivite PIC 9(2).
       
       FD factivite.
       01 tamp_factivite.
           02 fa_idactivite PIC 9(3).
           02 fa_nomActivite PIC A(20).
           02 fa_encadrant PIC A(20).
           02 fa_nbparticipant PIC 9(3).
           02 fa_jour PIC 9(1).
           02 fa_semaine PIC 9(2).
           02 fa_heureD PIC 9(2).
           02 fa_heureF PIC 9(2).
           02 fa_type PIC A(20).

       FD ftype.
       01 tamp_ftype.
           02 ft_type PIC A(20).
           02 ft_lieu PIC A(20).
           02 ft_materiel PIC X(20).
           02 ft_montantParticipation PIC 9(2).
           02 ft_gratuit PIC 9.
   
       WORKING-STORAGE SECTION.
           77 cr_fmembre PIC 9(2).
           77 cr_finscription PIC 9(2).
           77 cr_ftype PIC 9(2).
           77 cr_factivite PIC 9(2).

           77 Widmembre PIC 9(3).
           77 Wnom PIC A(20).
           77 Wprenom PIC A(20).
           77 Wtel PIC X(20).
           77 Wadresse PIC X(30).
           77 Wstatut PIC A(20).
           77 Wcheck PIC 9.
           77 Wstop PIC 9.
           77 Wstopp PIC 9.
           77 Wfin PIC 9.
           77 Wfin1 PIC 9.
           77 Wfin2 PIC 9.
           77 Wtrouve PIC 9(1).
           77 Wtype PIC A(20).
           77 Widactivite PIC 9(3).
           77 Wcompteur PIC 9.
           77 Winscrit PIC 9.
           77 Wnb PIC 9.
           77 Wchoixmenu PIC 9(2).
           77 WstopMenu PIC 9(2).
           77 Wsemaine PIC 9(2).
           77 Wjour PIC 9.
           77 WstopMenuMembre PIC 9(2).
           77 WchoixMenuMembre PIC 9(2).
           77 WstopMenuEncadrant PIC 9(2).
           77 WstopMenuPresident PIC 9(2).
  
       PROCEDURE DIVISION.
    	      
    	      open I-O fmembre
    	 
	          if cr_fmembre = 35  then
	              open output fmembre
	          end-if
	          
	          close fmembre

              open I-O ftype
    	 
	          if cr_ftype = 35  then
	              open output ftype
	          end-if
	          
	          close ftype
	          
	      open I-O factivite
    	 
	          if cr_factivite = 35  then
	              open output factivite
	          end-if
	          
	          close factivite
	          
	      open I-O finscription
    	 
	          if cr_finscription = 35  then
	              open output finscription
	          end-if
	          
	          close finscription
	         
	           perform with test after until WstopMenu = 1
	      
		       Display " Logiciel de gestion des activites !"

			      Display " "
			      Display " 1 - Membre "
			      Display " 2 - Encadrant "
			      display " 3 - Président : "
			      display " "
			      
			      perform with test after until Wchoixmenu = 1 
			      				or WchoixMenu = 2 
			      				or Wchoixmenu = 3 
				      display "Choix du role entre [1-3]: ?"
				      accept Wchoixmenu
			      
			      end-perform

			         if Wchoixmenu=1 then
			            perform menuMembre
			         end-if   
				         
		                 if Wchoixmenu=2 then
		                   perform menuEncadrant
		                 end-if
				                 
		                 if Wchoixmenu = 3 
		              	    perform menuPresident
		              	  end-if
				              	   
		             Display "Sortir ? 0(non) / 1(oui) "
			     Accept WstopMenu
	           end-perform
	           
	           display"Projet Cinematique des fichiers"
	           display"Alphadjo barry, Louka place, Samuel Ravard"
	           display"Annee Scolaire : 2022 - 2023"
	           display "Au revoir, madame, monsieur"
	          
       STOP RUN.
       
       
       menuMembre.
       
               Perform with test after until WstopMenuMembre = 1
         
		         Display "Menu membre, votre choix : ?"
		         display " "
		         Display "1  - Ajouter un membre"
		         Display "2  - Afficher les membres"
		         Display "3  - Afficher des activites"
		         Display "4  - Afficher les activites semaine"
		         Display "5  - Afficher les activites jour "
		         Display "6  - Afficher des types d'activites"
		         display "7  - S'inscrire a une activite"
		         Display "8  - Afficher les inscriptions"
		         display "9  - Membre le plus actif"
		         display "10 - Membre le moins actif"
		         display "11 - Activite la plus suivie"
		         display "12 - Activite la moins suivie"
		         display "13 - Distanciel"
		         
		         
		         perform with test after until WstopMenuMembre = 1 
			      				or WstopMenuMembre = 2 
			      				or WstopMenuMembre = 3
			      				or WstopMenuMembre = 4
			      				or WstopMenuMembre = 5
			      				or WstopMenuMembre = 6
			      				or WstopMenuMembre = 7
			      				or WstopMenuMembre = 8
			      				or WstopMenuMembre = 9
			      				or WstopMenuMembre = 10
			      				or WstopMenuMembre = 11
			      				or WstopMenuMembre = 12
			      				or WstopMenuMembre = 13
		                     display "Votre choix possible de [1-13] : ?"
				      ACCEPT WstopMenuMembre
			      
	                end-perform

		                if WstopMenuMembre=1 then
		                 Perform ajoutMembre
		                end-if

		                if WstopMenuMembre=2 then
		                 Perform affichageMembre
		                end-if

		                if WstopMenuMembre=3 then
		                  Perform affichageActivite
		                end-if

		                if WstopMenuMembre=4 then
		                   Perform  ActiviteSemaine
		                end-if

		                if WstopMenuMembre=5 then
		                    Perform ActiviteJour
		                end-if

		                if WstopMenuMembre=6 then
		                   Perform affichageType
		                end-if

		                if WstopMenuMembre=7 then
		                  Perform inscriptionMembreActivite
		                end-if
		                
		                
		                if WstopMenuMembre=8 then
		                  Perform affichageInscription
		                end-if
		                
		                if WstopMenuMembre=9 then
		                  Perform MembreLePlusActif
		                end-if
		                
		                if WstopMenuMembre=10 then
		                  Perform MembreLeMoinsActif
		                end-if
		                
		                 if WstopMenuMembre=11 then
		                  Perform ActiviteLaPlusSuivie
		                end-if
		                
		                if WstopMenuMembre=12 then
		                  Perform ActiviteLaMoinsSuivie
		                end-if
		                
		                 if WstopMenuMembre=13 then
		                  Perform Distanciel
		                end-if
		               
		               
		         
		         Display "Voulez-vous sortir (0 pour non , 1 pour oui)?"
		         Accept WstopMenuMembre
		       End-perform
              STOP RUN.
              
        menuEncadrant.
       
               Perform with test after until WstopMenuEncadrant = 1
         
		         Display "Menu Encadrant, votre choix : ?"
		         display " "
		         Display "1  - Ajouter un membre"
		         display "2  - Modifier un membre "
		         Display "3  - Afficher les membres"
		         display "4  - Ajout d'une activite"
		         Display "5  - Afficher des activites"
		         Display "6  - Modifier une activite"
		         Display "7  - Afficher les activites semaine"
		         Display "8  - Afficher les activites jour "
		         display "9  - Ajouter un type"
		         display "10 - Modifier un type"
		         Display "11 - Afficher des types d'activites"
		         display "12 - Inscription a une activite"
		         Display "13 - Afficher les inscriptions"
		         display "14 - Membre le plus actif"
		         display "15 - Membre le moins actif"
		         display "16 - Activite la plus suivie"
		         display "17 - Activite la moins suivie"
		         display "18 - Distanciel"
		         
		         perform with test after until WstopMenuEncadrant = 1 
			      				or WstopMenuEncadrant = 2 
			      				or WstopMenuEncadrant = 3
			      				or WstopMenuEncadrant = 4
			      				or WstopMenuEncadrant = 5
			      				or WstopMenuEncadrant = 6
			      				or WstopMenuEncadrant = 7
			      				or WstopMenuEncadrant = 8 
			      				or WstopMenuEncadrant = 9 
			      				or WstopMenuEncadrant = 10
			      				or WstopMenuEncadrant = 11
			      				or WstopMenuEncadrant = 12
			      				or WstopMenuEncadrant = 13
			      				or WstopMenuEncadrant = 14
			      				or WstopMenuEncadrant = 15
			      				or WstopMenuEncadrant = 16
			      				or WstopMenuEncadrant = 17
			      				or WstopMenuEncadrant = 18
		                     display "Votre choix possible de [1-18] : ?"
				      accept WstopMenuEncadrant
			      
	                end-perform

		                if WstopMenuEncadrant=1 then
		                 Perform ajoutMembre
		                end-if

		                if WstopMenuEncadrant=2 then
		                 Perform UpdateMembre
		                end-if

		                if WstopMenuEncadrant=3 then
		                  Perform affichageMembre
		                end-if

		                if WstopMenuEncadrant=4 then
		                   Perform ajoutActivite
		                end-if

		                if WstopMenuEncadrant=5 then
		                    Perform UpdateActivite
		                end-if
		                

		                if WstopMenuEncadrant=6 then
		                   Perform affichageActivite
		                end-if

		                if WstopMenuEncadrant=7 then 
		                  Perform ActiviteSemaine
		                end-if
		                
		                if WstopMenuEncadrant=8 then
		                  Perform ActiviteJour
		                end-if
		                
		                if WstopMenuEncadrant=9 then
		                  Perform ajoutType
		                end-if
		                
		                if WstopMenuEncadrant=10 then
		                  Perform UpdateType
		                end-if
		                
		                if WstopMenuEncadrant=11 then
		                  Perform affichageType
		                end-if
		                
		                if WstopMenuEncadrant=12 then
		                  Perform inscriptionMembreActivite
		                end-if
		                
		                 if WstopMenuEncadrant=13 then
		                  Perform affichageInscription
		                end-if
		                
		                if WstopMenuEncadrant=14 then
		                  Perform MembreLePlusActif
		                end-if
		                
		                 if WstopMenuEncadrant=15 then
		                  Perform MembreLeMoinsActif
		                end-if
		                
		                 if WstopMenuEncadrant=16 then
		                  Perform ActiviteLaPLusSuivie
		                end-if
		                
		                 if WstopMenuEncadrant=17 then
		                  Perform ActiviteLaMoinsSuivie
		                end-if
		                
		                 if WstopMenuEncadrant=18 then
		                  Perform Distanciel
		                end-if
		                
		                
		         
		         Display "Voulez-vous sortir (0 pour non , 1 pour oui)?"
		         Accept WstopMenuEncadrant
		       End-perform
              STOP RUN.
              
        menuPresident.
       
               Perform with test after until WstopMenuPresident = 1
         
		         Display "Menu President, votre choix : ?"
		         display " "
		         Display "1  - Ajouter un membre"
		         display "2  - Modifier un membre "
		         Display "3  - Afficher les membres"
		         display "4  - Supprimer un membre"
		         display "5  - Ajout d'une activite"
		         Display "6  - Afficher des activites"
		         Display "7  - Modifier une activite"
		         display "8  - Supprimer une activite"
		         Display "9  - Afficher les activites semaine"
		         Display "10 - Afficher les activites jour "
		         display "11 - Ajouter un type"
		         display "12 - Modifier un type"
		         display "13 - Supprimer un type d'activites"
		         Display "14 - Afficher des types d'activites"
		         Display "15 - Inscription a une activite"
		         Display "16 - Afficher les inscriptions"
		         display "17 - Supprimer un inscription"
		         display "18 - Membre le plus actif"
		         display "19 - Membre le moins actif"
		         display "20 - Activite la plus suivie"
		         display "21 - Activite la moins suivie"
		         display "22 - Distanciel"
		         
		         perform with test after until WstopMenuPresident = 1 
			      				or WstopMenuPresident = 2 
			      				or WstopMenuPresident = 3
			      				or WstopMenuPresident = 4
			      				or WstopMenuPresident = 5
			      				or WstopMenuPresident = 6
			      				or WstopMenuPresident = 7
			      				or WstopMenuPresident = 8 
			      				or WstopMenuPresident = 9 
			      				or WstopMenuPresident = 10
			      				or WstopMenuPresident = 11
			      				or WstopMenuPresident = 12
			      				or WstopMenuPresident = 13
			      				or WstopMenuPresident = 14
			      				or WstopMenuPresident = 15
			      				or WstopMenuPresident = 16
			      				or WstopMenuPresident = 17
			      				or WstopMenuPresident = 18
			      				or WstopMenuPresident = 19
			      				or WstopMenuPresident = 20 
			      				or WstopMenuPresident = 21
			      				or WstopMenuPresident = 22
			      				
		                     display "Votre choix possible de [1-22] : ?"
				      accept WstopMenuPresident
			      
	                end-perform

		                if WstopMenuPresident=1 then
		                 Perform ajoutMembre
		                end-if

		                if WstopMenuPresident=2 then
		                 Perform UpdateMembre
		                end-if

		                if WstopMenuPresident=3 then
		                  Perform affichageMembre
		                end-if

		                if WstopMenuPresident=4 then
		                   Perform DeleteMembre
		                end-if
		                
		                 if WstopMenuPresident=5 then
		                   Perform ajoutActivite
		                end-if

		                if WstopMenuPresident=6 then
		                    Perform affichageActivite
		                end-if
		                

		                if WstopMenuPresident=7 then
		                   Perform UpdateActivite
		                end-if

		                if WstopMenuPresident=8 then
		                  Perform DeleteActivite
		                end-if
		                
		                if WstopMenuPresident=9 then
		                  Perform ActiviteSemaine
		                end-if
		                
		                if WstopMenuPresident=10 then
		                  Perform ActiviteJour
		                end-if
		                
		                if WstopMenuPresident=11 then
		                  Perform ajoutType
		                end-if
		                
		                if WstopMenuPresident=12 then
		                  Perform UpdateType
		                end-if
		                
		                 if WstopMenuPresident=13 then
		                  Perform DeleteType
		                end-if
		                
		                if WstopMenuPresident=14 then
		                  Perform affichageType
		                end-if
		                
		                if WstopMenuPresident=15 then
		                  Perform inscriptionMembreActivite
		                end-if
		                
		                if WstopMenuPresident=16 then
		                  Perform affichageInscription
		                end-if
		                
		                if WstopMenuPresident=17 then
		                  Perform DeleteInscription
		                end-if
		                
		                if WstopMenuPresident=18 then
		                  Perform MembreLePlusActif
		                end-if
		                
		                 if WstopMenuPresident=19 then
		                  Perform MembreLeMoinsActif
		                end-if
		                
		                 if WstopMenuPresident=20 then
		                  Perform ActiviteLaPLusSuivie
		                end-if
		                
		                 if WstopMenuPresident=21 then
		                  Perform ActiviteLaMoinsSuivie
		                end-if
		                
		                
		         
		         Display "Voulez-vous sortir (0 pour non , 1 pour oui)?"
		         Accept WstopMenuPresident
		       End-perform
              STOP RUN.


       
       existeMembre.
           open input fmembre
           move 0 to Wtrouve
	    	
           move Widmembre to fm_idmembre
           read fmembre
	       not invalid key
		       move 1 to Wtrouve
    	   end-read
    	  
    	  close fmembre.
    	  
    	  AffichMembre.
           open input fmembre
           move 0 to Wtrouve
	    	
           move Widmembre to fm_idmembre
           read fmembre
	       not invalid key
		       
		     display "Identifiant : "fm_idmembre
		     display "Nom : "fm_nom
                    display "Prenom : "fm_prenom 
                    display "adresse : "fm_adresse
                    display "Telephone : "fm_telephone
                    display "statut : "fm_statut
		       
    	   end-read
    	  
    	  close fmembre.
    	  
    	  ajoutMembre.
    	  	
    	  	move 0 to Wstop
    	  	move 0 to Wtrouve
    	  	
    	  	perform with test after until Wstop = 0 
    	  		
    	  		perform with test after until Wtrouve = 0 
    	  			
    	  			display"Donner l'identifiant d'un membre inexistant : "
    	  			accept Widmembre
    	  			
    	  			perform existeMembre
    	  		end-perform
    	  		
    	  		display "Donner le nom du membre : "
    	  		accept Wnom
    	  		
    	  		display "Donner le prenom du membre : "
    	  		accept Wprenom
    	  		
    	  		display "Donner le numero de telephone du membre : "
    	  		accept Wtel
    	  		
    	  		display "Donner nom l'adresse du membre : "
    	  		accept Wadresse
    	  		
    	  		perform with test after until 
    	  					Wstatut = 'membre' OR Wstatut = 'encadrant' 
       	   	                       				   OR Wstatut = 'president'
	    	                   display "statut : Membre, encadrant, President "
	    	  		   accept Wstatut
	    	  	end-perform
	    	  	
	    	  	  open I-O fmembre
		   	            
		   	 	    move Widmembre to fm_idmembre
		   	 	    move Wnom to fm_nom
		   	 	    move Wprenom to fm_prenom
		   	 	    move Wtel to fm_telephone
		   	 	    move Wadresse to fm_adresse
		   	 	    move Wstatut to fm_statut
		   	 	    write tamp_fmembre
		   	  end-write
		   	
               	   	     if cr_fmembre = 00 then
		   	 	    display "Un nouveau membre ajoute avec succes"
		   	     else
		   	 	    display "Membre non ajoute dans le fichier des membre"
		   	     end-if
		   	     
		   	     close fmembre
		   	 
		   	     perform with test after until Wstop = 0  OR Wstop = 1
		                display "Autre membre ? 1(Oui) ou 0(Non)"
		                accept Wstop 
		             end-perform
	    	  		
    	  	end-perform. 
    	  	
    	  	
    	   affichageMembre.
                 open input fmembre

                 move 0 to Wfin 

                 perform with test after until Wfin = 1 
                       
                       read fmembre
                          
                          at end 
                                move 1 to Wfin
                          not at end
			    
			     display "Identifiant : "fm_idmembre
			     display "Nom : "fm_nom
                            display "Prenom : "fm_prenom 
                            display "adresse : "fm_adresse
                            display "Telephone : "fm_telephone
                            display "statut : "fm_statut
                            display "-----------------------------------"
                       end-read
                            
                 end-perform.
                 
                 close fmembre.  
                 
           UpdateMembre.
           	
           	open input fmembre
	        DISPLAY "Donnez l'identifiant du membre e modifier : "
	        ACCEPT Widmembre
	        PERFORM existeMembre
		
		   
                 MOVE Widmembre TO fm_idmembre
                 READ fmembre
		   
                 IF Wtrouve = 1 THEN
		       DISPLAY "Donnez le nom du membre : "
		       ACCEPT Wnom
		       
		       move Wnom to fm_nom
		       
		       DISPLAY "Donnez le prenom du membre : "
		       ACCEPT Wprenom
		       
		       move Wprenom to fm_prenom
		       
		       DISPLAY "Donnez le numero de telephone du membre : "
		       ACCEPT Wtel
		       MOVE Wtel TO fm_telephone
		       
		       DISPLAY "Donnez l'adresse du membre : "
		       ACCEPT Wadresse
		       
		       move Wadresse to fm_adresse
		       
		       PERFORM UNTIL Wstatut = 'membre' OR 
		       				Wstatut = 'encadrant' OR 
		       				Wstatut = 'president'
		         DISPLAY "Statut : membre, encadrant, president"
		         ACCEPT Wstatut
		       END-PERFORM
		       
		       move Wstatut to fm_statut
		       
		       OPEN I-O fmembre
		       REWRITE tamp_fmembre
		       end-rewrite
		       
		       if cr_fmembre = 00
			      DISPLAY "Modificatiion effectuee avec succes"
		       ELSE
		          DISPLAY "Aucune modification apportee"
		       END-IF
	      
		       CLOSE fmembre
                   ELSE
		       DISPLAY "Membre inexistant dans notre logiciel"
                   END-IF
		   
                   
                   STOP RUN.
                   
                   
             MembreLePlusActif.
             	   
             	   open input fmembre
             	   
             	   move 0 to Wfin
             	   move 0 to Widmembre
             	   move 0 to Wnb
             	   
             	   perform with test after until Wfin = 1 
             	   	
             	   	 read fmembre
             	   	 
             	   	   at end
             	   	 	move 1 to Wfin
             	   	   not at end
             	   	       
             	   	       open input finscription
             	   	 	
             	   	 	move fm_idmembre to fi_idmembre
             	   	 	
             	   	 	start finscription key is = fi_idmembre
             	   	 		
             	   	 		not invalid key
             	   	 		
             	   	 		      move 0 to Wfin1
             	   	 		      move 0 to Wstop
             	   	 		      move 0 to Wcompteur
             	   	 		      
             	   	 		      perform with test after until Wfin1 = 1 
             	   	 		      					or Wstop = 1
             	   	 		      		
             	   	 		      		read finscription next
             	   	 		      	        
             	   	 		      	        at end 
             	   	 		      	        	move 1 to Wfin1
             	   	 		      	        not at end
             	   	 		      	        	
             	   	 		      	        	if fm_idmembre = fi_idmembre then
             	   	 		      	        		
             	   	 		      	        		add 1 to Wcompteur
             	   	 		      	        	end-if
             	   	 		      end-perform
             	   	 		      
             	   	 		      if Wcompteur > Wnb then
             	   	 		      	    move Wcompteur to Wnb
             	   	 		      	    move fm_idmembre to Widmembre
             	   	 		      end-if
             	   	        end-start
             	   	        
             	   	        close finscription
             	   	        
             	   	    end-read
             	  end-perform
             	  
             	  display "Membre le plus actif : "Wnb" activites"
             	  
             	  display " "
             	  display " "
             	  
             	  perform AffichMembre
             	  
             	  close fmembre.
             	  
             	  
             	  
            MembreLeMoinsActif.
             	   
             	   open input fmembre
             	   
             	   move 0 to Wfin
             	   move 0 to Widmembre
             	   move 26000 to Wnb
             	   
             	   perform with test after until Wfin = 1 
             	   	
             	   	 read fmembre
             	   	 
             	   	   at end
             	   	 	move 1 to Wfin
             	   	   not at end
             	   	       
             	   	       open input finscription
             	   	 	
             	   	 	move fm_idmembre to fi_idmembre
             	   	 	
             	   	 	start finscription key is = fi_idmembre
             	   	 	
             	   	 		not invalid key
             	   	 		
             	   	 		      move 0 to Wfin1
             	   	 		      move 0 to Wstop
             	   	 		      move 0 to Wcompteur
             	   	 		      
             	   	 		      perform with test after until Wfin1 = 1 
             	   	 		      					or Wstop = 1
             	   	 		      		
             	   	 		      		read finscription next
             	   	 		      	        
             	   	 		      	        at end 
             	   	 		      	        	move 1 to Wfin1
             	   	 		      	        not at end
             	   	 		      	        	
             	   	 		      	        	if fm_idmembre = fi_idmembre then
             	   	 		      	        		
             	   	 		      	        		add 1 to Wcompteur
             	   	 		      	        	end-if
             	   	 		      	        	
             	   	 		      end-perform
             	   	 		      
             	   	 		      if Wcompteur < Wnb then
             	   	 		      	    move Wcompteur to Wnb
             	   	 		      	    move fm_idmembre to Widmembre
             	   	 		      end-if
             	   	        end-start
             	   	        
             	   	        close finscription
             	   	        
             	   	    end-read
             	  end-perform
             	  
             	  display "Membre le moins actif: "Wnb" activites"
             	  
             	  display " "
             	  display " "
             	  
             	  perform AffichMembre
             	  
             	  close fmembre.

                   
             inscriptionMembre.
    	           
    	           open input finscription
    	           
    	           move 0 to Wfin
    	           move 0 to Wcompteur
    	           
    	           perform with test after until wfin = 1 
    	           	
    	           	read finscription
    	           	
    	           	at end
    	           		move 1 to Wfin
    	           	not at end
    	           		
    	           	      if Widmembre = fi_idmembre then
    	           	      	   add 1 to Wcompteur
    	           	      end-if 
    	           
    	           end-perform.
    	           
    	           close finscription.
    
             DeleteMembre.
           	
           	open input fmembre
	        DISPLAY "Donnez l'identifiant du membre e supprimer: "
	        ACCEPT Widmembre
	        PERFORM existeMembre
	        perform inscriptionMembre
		
		   
                 MOVE Widmembre TO fm_idmembre
                 READ fmembre
		   
                 IF Wtrouve = 1 THEN
	 		
	 	      if Wcompteur = 0 then
	 	      
	 	      	       OPEN I-O fmembre
			         delete fmembre
			       end-delete
			       
			       if cr_fmembre = 00
			          DISPLAY "Suppression effectuee avec succes"
			       ELSE
		                display "Aucune suppression effectuee"
			       END-IF
		
			       CLOSE fmembre
	 	      else
	 	      	  display "Membre ayant des inscriptions"
	 	      end-if
		       
                   ELSE
		       DISPLAY "Membre inexistant dans notre logiciel"
                   END-IF
		   
                   
                   STOP RUN.
                   
     
           existeType.
               open input ftype
               move 0 to Wtrouve
	    	 
               move Wtype to ft_type
               
               read ftype
	          not invalid key
		          move 1 to Wtrouve
    	       end-read
    	      
    	      close ftype.

           ajoutType.
    	  	
    	  	move 0 to Wstop
    	  	move 0 to Wtrouve
    	  	
    	  	perform with test after until Wstop = 0 
    	  		
    	  		perform with test after until Wtrouve = 0 
    	  			
    	  			display"Donner le type inexistant d'activité : "
    	  			accept Wtype
    	  			
    	  			perform existeType
    	  		end-perform
    	  		
    	  		display "Donner le lieu de l'activité : "
    	  		accept ft_lieu
    	  		
    	  		display "Donner le matériel nécessaire : "
    	  		accept ft_materiel
    	  		
    	  		perform with test after until ft_montantParticipation > 0
		                display "Prix de participation > 0 : "
	    	  		 accept ft_montantParticipation
		        end-perform
    	  		
    	  		perform with test after until ft_gratuit = 0 OR ft_gratuit = 1
    	  		         display "Gratuit ? Oui(1) / Non(0) "
	    	  		 accept ft_gratuit
		        end-perform
	    	  	
	    	  	  open I-O ftype
		   	            
		   	 	    move Wtype to ft_type
		   	 	    write tamp_ftype
		   	  end-write
		   	
               	   	     if cr_ftype = 00 then
		   	 	    display "Un nouveau type ajouté avec succès"
		   	     else
		   	 	    display "Type non ajouté dans le fichier des types"
		   	     end-if
		   	     
		   	     close ftype
		   	 
		   	     perform with test after until Wstop = 0  OR Wstop = 1
		                display "Autre type ? 1(Oui) ou 0(Non)"
		                accept Wstop 
		             end-perform
	    	  		
    	  	end-perform. 
                 
           UpdateType.
           	
           	      open input ftype
	              DISPLAY "Donnez le type de l'activité à modifier : "
	              ACCEPT Wtype
	              PERFORM existeType
		
		   
                 MOVE Wtype TO ft_type
                 READ ftype
		   
                 IF Wtrouve = 1 THEN
                           
	              	    
    	  		    display "Donner le lieu de l'activité : "
    	  		    accept ft_lieu
    	  		      
    	  		    display "Donner le matériel nécessaire : "
    	  		    accept ft_materiel
    	  		    
    	  		    perform with test after until ft_montantParticipation > 0
		                display "Prix de participation > 0 : "
	    	  		 accept ft_montantParticipation
		            end-perform 
	    	  		
	    	            perform with test after until ft_gratuit = 0 
	    	     			OR ft_gratuit = 1
	    	  		         display "Gratuit ? Oui(1) / Non(0) "
		    	  		 accept ft_gratuit
			    end-perform
	    	  	
    	  		
		             OPEN I-O ftype
		             REWRITE tamp_ftype
		             end-rewrite
		             
		             if cr_ftype = 00
			    DISPLAY "Modificatiion effectuee avec succes"
		             ELSE
		                  DISPLAY "Aucune modification apportee"
		             END-IF
		            
		             CLOSE ftype
                  ELSE
		        DISPLAY "Type inexistant dans notre logiciel"
                  END-IF
		   
                   
           STOP RUN.


           DeleteType.
           	
           	open input ftype
	        DISPLAY "Donnez le nom du  type à supprimer: "
	        ACCEPT Wtype
	        PERFORM existeType
	        perform activiteType
		
		   
                    MOVE Wtype TO ft_type
                    READ ftype
		   
                    IF Wtrouve = 1 THEN
			
			    if Wcompteur = 0 then 
			       OPEN I-O ftype
			       delete ftype
			       end-delete
			       
			       if cr_ftype = 00
			            DISPLAY "Suppression effectuee avec succes"
			       ELSE
				    DISPLAY "Aucune suppression effectuee"
			       END-IF
		      
			       CLOSE ftype
			    else
			    	display "Type ayant deja des activites"
			    end-if
                   ELSE
		       DISPLAY "Type inexistant dans notre logiciel"
                   END-IF
		   
                   
               STOP RUN.
                   
                   
       activiteType.
    	           
    	           open input factivite
    	           
    	           move 0 to Wfin
    	           move 0 to Wcompteur
    	           
    	           perform with test after until wfin = 1 
    	           	
    	           	read factivite
    	           	
    	           	at end
    	           		move 1 to Wfin
    	           	not at end
    	           		
    	           	      if Wtype = fa_type then
    	           	      	   add 1 to Wcompteur
    	           	      end-if 
    	           
    	           end-perform.
    	           
    	           close factivite.
    	           
    	           
       affichageActiviteType.
    	           
    	           open input factivite
    	           
    	           move 0 to Wfin
    	          
    	            DISPLAY "Activité de quel type : ? "
	            ACCEPT Wtype
	            PERFORM existeType
 		    
 		    
 		    if Wtrouve = 1 
 		    	
 		    	move Wtype to fa_type
 		    	
 		    	start factivite key is = fa_type
 		    	
 		    	   invalid key
 		    		display "Aucune activité de ce type"
 		    	
 		    	   not invalid key
 		    	
 		    		move 0 to Wfin
 		    		move 0 to Wstop
 		    		
 		    		perform with test after until Wfin = 1 or Wstop = 1
 		    			
 		    			read factivite next
 		    			
 		    			at end
 		    				move 1 to Wfin
 		    			not at end
 		    				
 		    				if fa_type = Wtype then
 		    					
 		    				
 		    					display "nom : "fa_nomActivite
							display "participants : "fa_nbparticipant
							display "jour : "fa_jour
							display "semaine : "fa_semaine
							display "Heure début : "fa_heureD
							display "Heure fin : "fa_heureF
							display "type : "fa_type
							
							display "------------------"
 		    				else
 		    					move 1 to Wtrouve
 		    				end-if
 		    				
 		    		end-perform
 		    		
 		          end-start
	    	           
	    	     else
	    	     	display "Type inexistant dans le logiciel"
	    	     end-if
    	           
    	           close factivite.

       affichageType.
                 open input ftype

                 move 0 to Wfin 

                 perform with test after until Wfin = 1 
                       
                       read ftype
                          
                          at end 
                                move 1 to Wfin
                          not at end
			    
		             display "Nom de l'activité : "ft_type
		             display "lieu : "ft_lieu
                            display "matériel : "ft_materiel
                            display "Graduit : "ft_gratuit
                           
                            display "Participation  : "
                            			ft_montantParticipation
                            display "-----------------------------------"
                       end-read
                            
                 end-perform.
                 
                 close ftype. 
                 
     
                 
       existeActivite.
           open input factivite
           move 0 to Wtrouve
	    	
           move Widactivite to fa_idactivite
           read factivite
	       not invalid key
		       move 1 to Wtrouve
    	   end-read
    	  
    	  close factivite.
    	  
    	  
        AffichActivite.
           open input factivite
           move 0 to Wtrouve
	    	
           move Widactivite to fa_idactivite
           read factivite
	       not invalid key
		       
		        display "identifiant : "fa_idactivite
	                display "nom : "fa_nomActivite
		        display "participants : "fa_nbparticipant
		        display "jour : "fa_jour
		        display "semaine : "fa_semaine
		        display "Heure début : "fa_heureD
		        display "Heure fin : "fa_heureF
		        display "type : "fa_type
		        display "-----------------------------------"
		       
    	   end-read
    	  
    	  close fmembre.

       ajoutActivite.
    	  	
    	  	move 0 to Wstop
    	  	move 0 to Wtrouve

    	  	perform with test after until Wstop = 0 
    	  		
    	  		perform with test after until Wtrouve = 0 
    	  			
    	  			display"Donner l'ID d'une activité inexistante : "
    	  			accept Widactivite
    	  			
    	  			perform existeActivite
    	  		end-perform
    	  		
    	        display "Donner le nom de l'activité : "
    	  	accept fa_nomActivite

                perform with test after until Wtrouve = 1
    	  		      display "Donner le prenom de l'encadrant : "
    	  		      accept Wprenom

                      PERFORM existeEncadrant
                END-PERFORM

                move Wprenom to fa_encadrant
                       
                perform with test after until fa_nbparticipant > 0
                        display "Donner le nombre de participant : "
    	  		 accept fa_nbparticipant
                END-PERFORM
               
                perform with test after until fa_jour > 0 
                				AND fa_jour <= 7
                        display "Quel jour : "
    	  		 accept fa_jour
                END-PERFORM


                perform with test after until fa_semaine > 0 
                				AND fa_semaine <= 52
                        display "Quel semaine: "
    	  		 accept fa_semaine
                END-PERFORM

                perform with test after until Wtrouve = 1 
    	  			
    	  			 display"Quel type "
    	  			 accept Wtype    
    	  			 
    	  			 perform existeType
    	  	 end-perform
    	  	 
    	  	 
    	  	 
                 perform with test after until fa_heureD > 8
				   AND fa_heureD<= 20 
		     display "Donner heure debut : entre 9h-20h"
		     accept fa_heureD
	         end-perform 
		
                perform with test after until fa_heureF > 8
				   AND fa_heureF <= 20 
		     display "Donner heure de fin : entre 9h-20h"
		     accept fa_heureF
                end-perform 
    	  		
	    	  	
	    	  	  open I-O factivite
		   	        move Wtype to fa_type
		   	 	    move Widactivite to fa_idactivite
		   	 	    write tamp_factivite
		   	  end-write
		   	
               	  if cr_factivite = 00 then    
		   	     display "Une nouvelle activité ajouté avec succès"
		   	  else
		   	     display "Activité non ajouté dans le fichier des activités"
		   	  end-if

		   	  perform with test after until Wstop = 0  OR Wstop = 1
		                display "Autre activité ? 1(Oui) ou 0(Non)"
		                accept Wstop 
		          end-perform
	    	  		
    	  	end-perform. 
    	  	
    	  	close factivite.
          
          
           UpdateActivite.
           	
	        DISPLAY "Donnez l'id activite modifier : "
	        ACCEPT Widactivite
	        PERFORM existeActivite
		
		   
                 MOVE Widactivite TO fa_idactivite
                 READ factivite
		   
                 IF Wtrouve = 1 THEN
                 	
                 	display "Donner le nom de l'activité : "
	    	  	accept fa_nomActivite

		        perform with test after until Wtrouve = 1
	    	  		      display "Donner le prenom de l'encadrant : "
	    	  		      accept Wprenom

		              PERFORM existeEncadrant
		        END-PERFORM

		        move Wprenom to fa_encadrant
		               
		        perform with test after until fa_nbparticipant > 0
		                display "Donner le nombre de participant : "
	    	  		 accept fa_nbparticipant
		        END-PERFORM
		       
		        perform with test after until fa_jour > 0 
		        				AND fa_jour <= 7
		                display "Quel jour : "
	    	  		 accept fa_jour
		        END-PERFORM


		        perform with test after until fa_semaine > 0 
		        				AND fa_semaine <= 52
		                display "Quel semaine: "
	    	  		 accept fa_semaine
		        END-PERFORM

		        perform with test after until Wtrouve = 1 
	    	  			
	    	  			 display"Quel type "
	    	  			 accept Wtype    
	    	  			 
	    	  			 perform existeType
	    	  	 end-perform
	    	  	 
	    	  	  perform with test after until fa_heureD > 8
				   AND fa_heureD<= 20 
		            display "Donner heure debut : entre 9h-20h"
		            accept fa_heureD
	                 end-perform 
		
                        perform with test after until fa_heureF > 8
				   AND fa_heureF <= 20 
		             display "Donner heure de fin : entre 9h-20h"
		             accept fa_heureF
		         end-perform 
		       
		       OPEN I-O factivite
	       	    move Wtype to fa_type
	   	 	    move Widactivite to fa_idactivite
	   	 	    rewrite tamp_factivite
		       end-rewrite
		       
		       if cr_factivite = 00
			      DISPLAY "Modificatiion effectuee avec succes"
		       ELSE
		          DISPLAY "Activite non modifiee. Oups !!!"
		       END-IF
		       
		       CLOSE factivite
               ELSE
	               DISPLAY "Activite inexistant dans notre logiciel"
               END-IF
		   
                    close factivite
                   
                   STOP RUN.
                   
           DeleteActivite.
           	
           	open input factivite
	        DISPLAY "Donnez l'activite a supprimer : "
	        ACCEPT Widactivite
	        PERFORM existeActivite
	        perform inscriptionActivite
		
		   
                    MOVE Widactivite TO fa_idactivite
                    READ factivite
		   
                    IF Wtrouve = 1 THEN
			
			    if Wcompteur = 0 then 
			       OPEN I-O factivite
			       delete factivite
			       end-delete
			       
			       if cr_factivite = 00
			            DISPLAY "Suppression activite effectuee avec succes"
			       ELSE
				    DISPLAY "Oups ! Oups ! Aucune suppression effectuee"
			       END-IF
		      
			       CLOSE factivite
			    else
			    	display "Activite ayant deja des inscriptions"
			    end-if
                   ELSE
		       DISPLAY "Activite inexistant dans notre logiciel"
                   END-IF
                   
               STOP RUN.         
          
                   
          inscriptionActivite.
    	           
    	           open input finscription
    	           
    	           move 0 to Wfin
    	           move 0 to Wcompteur
    	           
    	           perform with test after until wfin = 1 
    	           	
    	           	read finscription
    	           	
    	           	at end
    	           		move 1 to Wfin
    	           	not at end
    	           		
    	           	      if Widactivite = fi_idactivite then
    	           	      	   add 1 to Wcompteur
    	           	      end-if 
    	           
    	           end-perform.
    	           
    	           close finscription.

          existeEncadrant.
                   open input fmembre
                   
                   move 0 to Wfin
	    	     
                   move 0 to Wtrouve
                   
                   perform with test after until Wfin = 1 OR Wtrouve = 1
                   read fmembre
                      at end 
                            move 1 to Wfin
                      not at end 
                        
                      if Wprenom = fm_prenom and 
                      			fm_statut = 'encadrant' then
                           move 1 to Wtrouve
                      end-if	
                   end-read
                   
                   end-perform.
    	      close fmembre.
    	      
    	      
    	  affichageActivite.
                 open input factivite

                 move 0 to Wfin 

                 perform with test after until Wfin = 1 
                       
                       read factivite
                          
                          at end 
                                move 1 to Wfin
                          not at end
			                display "identifiant : "fa_idactivite
			                display "nom : "fa_nomActivite
				        display "participants : "fa_nbparticipant
				        display "jour : "fa_jour
				        display "semaine : "fa_semaine
				        display "Heure début : "fa_heureD
				        display "Heure fin : "fa_heureF
				        display "type : "fa_type
				        display "-----------------------------------"
                       end-read
                            
                 end-perform.
                 
                 close factivite. 
                 
           inscriptionMembreActivite.
                  
                  
                  move 0 to Wstopp
                  
                  perform with test after until Wstopp = 0
                  
           		perform with test after until Wtrouve = 1 
           			display "Quel membre (existant) inscrire : ?"
           			accept Widmembre
           			
           			perform existeMembre
           		end-perform
           		
           		if Wtrouve = 1 then
           			
           			move 0 to Wtrouve
           			
           			perform with test after until Wtrouve = 1 
           				display "Quelle activite (existante) :  ? "
           				accept Widactivite
           				
           				perform existeActivite
           			end-perform
           			
           			perform MembreInscrit
           			
           			if Winscrit = 0 then
           			
           			    if Wtrouve = 1 then
           				
           				open I-O finscription
           					
           					move Widmembre to fi_idmembre
           					move Widactivite to fi_idactivite
           					
           					write tamp_finscription
           					
           				end-write
           				
           				close finscription
           				
           				if cr_finscription = 00 then
           					display "Membre inscrit avec succès"
           				else
           					display "Inscription non reussie"
           				end-if
           						  
           				
           			    else
           				display "Activite inexistante dans le logiciel"
           			    end-if
           			    
           			  else
           			  	display "Membre deja inscrit a cette activite"
           			  end-if
           		else
           			display "Membre inexistant dans le logiciel"
           		end-if
           		
           		
           		perform with test after until Wstopp = 0  OR Wstopp = 1
		             display "Autre inscription ? 1(Oui) ou 0(Non)"
		             accept Wstopp 
		        end-perform
           		
           	     end-perform.
      
           
           DeleteInscription.
           	
           	      open input finscription
	              DISPLAY "Supprimer inscription de quel membre : ? "
	              ACCEPT Widmembre
	              PERFORM existeMembre
		
		   
                 MOVE Wtype TO ft_type
                 READ ftype
		   
                 IF Wtrouve = 1 THEN
                           
                   move 0 to Wtrouve 
                   
                   display "Supprimer inscription a quelle activite : ?"
                   accept Widactivite
                   perform existeActivite                         
              	    
              	    if Wtrouve = 1 then
  
  		        perform MembreInscrit
  		           
  		           if Winscrit = 1 then
			        OPEN I-O finscription
			        delete finscription
			        end-delete
			     
			        if cr_finscription = 00
		                  DISPLAY "Suppression effectuee avec succes"
			        ELSE
			          DISPLAY "Aucune suppression apportee"
			        END-IF
			    
			        CLOSE finscription
			        
	                   else
			   	 display "Aucune inscription de membre a cette activite"
		           end-if
			     
	             else
	             	 display "Activite inexistante dans le logiciel"
	             end-if
	             
                  ELSE
		        DISPLAY "Membre inexistant dans notre logiciel"
                  END-IF
		   
                   
           	STOP RUN.
           	
           	CLOSE finscription.

           	     
            ActiviteSemaine.
    	           
    	           open input factivite
    	           
    	           move 0 to Wfin
    	           
    	           perform with test after until Wsemaine > 0 and
    	           				         Wsemaine <= 52
    	            DISPLAY "Activité de quel semaine : ? "
	            ACCEPT Wsemaine
	           end-perform
	           
	             	move Wsemaine to fa_semaine
	             	
 		    	start factivite key is = fa_semaine
 		    	
 		    	   invalid key
 		    		display "Aucune activité pour cette semaine"
 		    	
 		    	   not invalid key
 		    	
 		    		move 0 to Wfin
 		    		move 0 to Wstop
 		    		
 		    		perform with test after until Wfin = 1 or Wstop = 1
 		    			
 		    			read factivite next
 		    			
 		    			at end
 		    				move 1 to Wfin
 		    			not at end
 		    				
 		    				if fa_semaine = Wsemaine then
 		    				
		 		    			display "identifiant : "fa_idactivite
							display "nom : "fa_nomActivite
							display "participants : "fa_nbparticipant
							display "jour : "fa_jour
							display "semaine : "fa_semaine
							display "Heure début : "fa_heureD
							display "Heure fin : "fa_heureF
							display "type : "fa_type
							display "-----------------------------------"
 		    				else
 		    					move 1 to Wstop
 		    				end-if
 		    				
 		    		end-perform
 		    		
 		       end-start
	    	           
    	           close factivite.
    	           
    	           
    	       
    	        ActiviteJour.
    	           
    	           open input factivite
    	           
    	           move 0 to Wfin
    	           
    	           perform with test after until Wjour > 0 and
    	           				         Wjour <= 52
    	            DISPLAY "Activité de quel jour : ? "
	            ACCEPT Wjour
	           end-perform
	           
	             	move Wjour to fa_jour
	             	
 		    	start factivite key is = fa_jour
 		    	
 		    	   invalid key
 		    		display "Aucune activité pour ce jour"
 		    	
 		    	   not invalid key
 		    	
 		    		move 0 to Wfin
 		    		move 0 to Wstop
 		    		
 		    		perform with test after until Wfin = 1 or Wstop = 1
 		    			
 		    			read factivite next
 		    			
 		    			at end
 		    				move 1 to Wfin
 		    			not at end
 		    				
 		    				if fa_jour = Wjour then
 		    				
		 		    			display "identifiant : "fa_idactivite
							display "nom : "fa_nomActivite
							display "participants : "fa_nbparticipant
							display "jour : "fa_jour
							display "semaine : "fa_semaine
							display "Heure début : "fa_heureD
							display "Heure fin : "fa_heureF
							display "type : "fa_type
							display "-----------------------------------"
 		    				else
 		    					move 1 to Wstop
 		    				end-if
 		    				
 		    		end-perform
 		    		
 		       end-start
	    	           
    	           close factivite.

           		
           		
              affichageInscription.
                 open input finscription

                 move 0 to Wfin 

                 perform with test after until Wfin = 1 
                       
                       read finscription
                          
                          at end 
                                move 1 to Wfin
                          not at end
			                display "Membre : "fi_idmembre
			                display "Activite : "fi_idactivite
				        display "---------------"
                       end-read
                            
                 end-perform.
              
                 close finscription. 
                 
              MembreInscrit.
                 open input finscription

                 move 0 to Winscrit
                 
                 move Widmembre to fi_idmembre
                 move Widactivite to fi_idactivite
                 
                 read finscription
                    
                    key is fi_idinscription
                    
                    invalid key
                    	move 0 to Winscrit
                    not invalid key
                 	move 1 to Winscrit
                 	
                 end-read

                 close finscription.
                 
                 
               nbInscritActivite.
               	open input finscription
               	
               	move 0 to Wfin
               	move 0 to Wnb
               	
               	perform with test after until Wfin = 1
               		
               		read finscription
               		
               		    at end
               			move 1 to Wfin
               		    not at end
               			
               			add 1 to Wnb
               			
               		end-read
               		       
               	end-perform
           		
           		close finscription.
           		
           		
               ActiviteLaPlusSuivie.
             	   
             	   open input factivite
             	   
             	   move 0 to Wfin
             	   move 0 to Widactivite
             	   move 0 to Wnb
             	   
             	   perform with test after until Wfin = 1 
             	   	
             	   	 read factivite
             	   	 
             	   	   at end
             	   	 	move 1 to Wfin
             	   	   not at end
             	   	       
             	   	       open input finscription
             	   	 	
             	   	 	move fa_idactivite to fi_idactivite
             	   	 	
             	   	 	start finscription key is = fi_idactivite
             	   	 	
             	   	 		invalid key
             	   	 			display "Activite ayant pas d'inscriptions"
             	   	 		not invalid key
             	   	 		
             	   	 		      move 0 to Wfin1
             	   	 		      move 0 to Wstop
             	   	 		      move 0 to Wcompteur
             	   	 		      
             	   	 		      perform with test after until Wfin1 = 1 
             	   	 		      					or Wstop = 1
             	   	 		      		
             	   	 		      		read finscription next
             	   	 		      	        
             	   	 		      	        at end 
             	   	 		      	        	move 1 to Wfin1
             	   	 		      	        not at end
             	   	 		      	        	
             	   	 		      	               if fa_idactivite 
             	   	 		      	               		= fi_idactivite then
             	   	 		      	        		
             	   	 		      	        		add 1 to Wcompteur
             	   	 		      	        	else
             	   	 		      	        		move 1 to Wstop
             	   	 		      	        	end-if
             	   	 		      end-perform
             	   	 		      
             	   	 		      if Wcompteur > Wnb then
             	   	 		      	    move Wcompteur to Wnb
             	   	 		      	    move fa_idactivite to Widactivite
             	   	 		      end-if
             	   	        end-start
             	   	        
             	   	        close finscription
             	   	        
             	   	    end-read
             	  end-perform
             	  
             	  display "Activité la plus suivie : "Wnb" inscrits"
             	  
             	  display " "
             	  display " "
             	  
             	  perform AffichActivite
             	  
             	  close factivite.
             	  
             	  
            ActiviteLaMoinsSuivie.
             	   
             	   open input factivite
             	   
             	   move 0 to Wfin
             	   move 0 to Widactivite
             	   move 26000 to Wnb
             	   
             	   perform with test after until Wfin = 1 
             	   	
             	   	 read factivite
             	   	 
             	   	   at end
             	   	 	move 1 to Wfin
             	   	   not at end
             	   	       
             	   	       open input finscription
             	   	 	
             	   	 	move fa_idactivite to fi_idactivite
             	   	 	
             	   	 	start finscription key is = fi_idactivite
             	   	 	
             	   	 		invalid key
             	   	 			display "Activite ayant pas d'inscriptions"
             	   	 		not invalid key
             	   	 		
             	   	 		      move 0 to Wfin1
             	   	 		      move 0 to Wstop
             	   	 		      move 0 to Wcompteur
             	   	 		      
             	   	 		      perform with test after until Wfin1 = 1 
             	   	 		      					or Wstop = 1
             	   	 		      		
             	   	 		      		read finscription next
             	   	 		      	        
             	   	 		      	        at end 
             	   	 		      	        	move 1 to Wfin1
             	   	 		      	        not at end
             	   	 		      	        	
             	   	 		      	               if fa_idactivite 
             	   	 		      	               		= fi_idactivite then
             	   	 		      	        		
             	   	 		      	        		add 1 to Wcompteur
             	   	 		      	        	else
             	   	 		      	        		move 1 to Wstop
             	   	 		      	        	end-if
             	   	 		      end-perform
             	   	 		      
             	   	 		      if Wcompteur < Wnb then
             	   	 		      	    move Wcompteur to Wnb
             	   	 		      	    move fa_idactivite to Widactivite
             	   	 		      end-if
             	   	        end-start
             	   	        
             	   	        close finscription
             	   	        
             	   	    end-read
             	    end-perform
             	  
             	  display "Activité la moins suivie : "Wnb" inscrits"
             	  
             	  display " "
             	  display " "
             	  
             	  perform AffichActivite
             	  
             	  close factivite.
             	  
        Distanciel.
           
           perform with test after until Wsemaine > 0 
                        and Wsemaine <= 52
                  display "Quelle semaine : ? "
                  accept Wsemaine
           end-perform
           
           open input factivite
           
           move Wsemaine to fa_semaine

           start factivite key is = fa_semaine
	   
           invalid key
	       display "Aucune activite cette semaine"
           not invalid key
           
           move 0 to Wstop
           move 0 to Wfin
       
                  perform with test after until Wfin = 1 or Wstop = 1
       
                      read factivite next
           
                         at end 
           
                           move 1 to Wfin
               
                         not at end 
           
                            if Wsemaine = fa_semaine then
               
                              open input ftype
                   
                                 move fa_type to Wtype
                   
                                  perform existeType
                   
                                   if Wtrouve = 1 then
                   
                                     if ft_gratuit = 1 
                       
                                      open input finscription
                           
                                        move fa_idactivite to Widactivite
                                        move Widactivite to fi_idactivite
                           
                                        start finscription 
                                        			key is = fi_idactivite
                               
                                           invalid key
                                            display "Aucune inscription "
                                           not invalid key
                                           
		                           move 0 to Wstopp
		                           move 0 to Wfin1 
                                   
		                           perform with test after until 
		                               Wfin1 = 1 or Wstopp = 1
		                                   
		                                   read finscription next
		                                   
		                                   at end
		                                   	  move 1 to Wfin1 
		                                   not at end
		                                   
		                                     if Widactivite = fi_idactivite
				                           move 0 to Wfin2
				                           
		                                          open input fmembre
		                                   
				                           perform with test after until 
				                               Wfin2 = 1
				                                   
				                                   read fmembre
				                                       
				                                       at end
				                                          move 1 to Wfin2
				                                       not at end
				                               
						                     if fm_idmembre = fi_idmembre then
						                          move fi_idmembre to Widmembre
						                          perform AffichMembre
						                     end-if
				                             
				                                   end-read
				                    
				                           end-perform
				                           
				                           close fmembre
				                           
		                                      else
		                                   	 move 1 to Wstopp 
		                                      end-if
		                                      
		                                    end-read
		                            
		                           end-perform
                                   
                                        end-start
                               
                                     close finscription
                               
		                  else
		                     display "Activite payante"
		                  end-if
		             else
		               display "Type d'activite non trouve"
		             end-if
		           
                         close ftype
                   
		       else
                        move 1 to Wstop 
		       end-if
                  
                  end-read
                  
       	end-perform
end-start

close factivite.

           	
    	  	

