with Ada.Strings;                use Ada.Strings;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;   use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;           use Ada.Command_Line;
with cache;                      use cache;
with exploitation_table_routage; use exploitation_table_routage;
with Routeur_Exceptions;         use Routeur_Exceptions;
with utilitaire;                 use utilitaire;

procedure routeur_LL is
    type Politique_de_tri is (FIFO, LRU, LFU);  -- Politiques du Cache.

    type Arguments_Routeur is record
        Taille : Integer;               -- Taille du cache.
        Politique : Politique_de_tri;   -- Politique de tri du cache.
        Statistiques : Boolean;         -- Choix de l'affichage des statistiques.
        Routage : Unbounded_String;     -- Nom du fichier de la table de routage.
        Paquets : Unbounded_String;     -- Nom du fichier des paquets.
        Resultats : Unbounded_String;   -- Nom du fichier des résultats.
    end record;

    i : Integer;
    i_ligne_paquet : Integer := 1;
    Route_Enregistree_Dans_Cache : Boolean;
    Destination_A_Enregistrer_Dans_Cache : T_Donnee;

    -- Statistiques du cache.
    Nb_Demande_Route : Integer := 0;
    Nb_Defaut_Cache : Integer := 0;
    Taux_Defaut_Cache : Float := 0.0;

    -- Affiche les resultats pour l'utilisateur.
    procedure Ecrire (destination : T_Adresse_IP; masque : T_Adresse_IP; eth : Unbounded_String) is
        ligne : Unbounded_String;   -- Ligne à afficher à la demande de l'utilisateur.
        longueur : Integer;         -- Longueur de la destination ou du masque.
    begin
        for i in 1..3 loop -- De 1 à 3 car on ne souhaite pas avoir de points apres le dernier octet.
            longueur := Length (To_Unbounded_String (Integer'Image (Ieme_Octet (destination, i))));
            ligne := ligne & To_Unbounded_String (Integer'Image (Ieme_Octet (destination, i))(2..longueur)) & "."; -- On commence par le deuxième car à cause de "Integer'Image", il y'a la création d'un espace au début de la chaine de caractère
        end loop;
        longueur := Length (To_Unbounded_String (Integer'Image (Ieme_Octet (destination, 4))));
        ligne := ligne & To_Unbounded_String (Integer'Image (Ieme_Octet (destination, 4))(2..longueur));
        ligne := ligne & " ";

        for i in 1..3 loop -- De 1 à 3 car on ne souhaite pas avoir de points apres le dernier octet.
            longueur := Length (To_Unbounded_String (Integer'Image (Ieme_Octet (masque, i))));
            ligne := ligne & To_Unbounded_String (Integer'Image (Ieme_Octet (masque, i))(2..longueur)) & "."; -- On commence par le deuxième car à cause de "Integer'Image", il y'a la création d'un espace au début de la chaine de caractère
        end loop;
        longueur := Length (To_Unbounded_String (Integer'Image (Ieme_Octet (masque, 4))));
        ligne := ligne & To_Unbounded_String (Integer'Image (Ieme_Octet (masque, 4))(2..longueur));
        ligne := ligne & " " & eth; -- Rajoute l'interface à la ligne.

        Put (ligne);
        New_Line;
    end Ecrire;

    procedure Pour_Chaque_Ecrire is new Pour_Chaque (Traiter => Ecrire);

    Options : Arguments_Routeur;                -- Énumération des options.
    Sortie : File_Type;                         -- Fichier des resultats.
    Paquet : File_Type;                         -- Fichier des paquets.
    Table : T_LCA;                            -- Table de routage sous forme d'une liste chainée.
    Meilleure_route : T_Donnee;                 -- La meilleure route correspondant au paquet.
    ligne : Unbounded_String;                   -- Ligne à écrire dans le fichier des resultats.
    Arret_Lecture_Paquets : Boolean := False;   -- Sortie de la boucle de lecture si "Fin".
    Ligne_Paquets : Unbounded_String;           -- Une ligne du fichier des paquets.
    Cache : T_LCA;
begin
    -- Initialiser les paramètres du routeur.

    -- Initialiser les paramètres du routeur par défaut.
    Options.Taille := 10;
    Options.Politique := LRU;
    Options.Statistiques := True;
    Options.Routage := To_Unbounded_String ("table.txt");
    Options.Paquets := To_Unbounded_String ("paquets.txt");
    Options.Resultats := To_Unbounded_String ("resultats.txt");

    -- Lire les arguments de l'utilisateur.
    i := 1;
    while i <= Argument_Count loop      -- Parcourt les options spécifiées par l'utilisateur.
        if Argument(i) = "-c" then      -- L'utilisateur définit la taille du cache.
            Options.Taille := Integer'Value(Argument(i+1));
            i := i+2;
        elsif Argument(i) = "-P" then   -- L'utilisateur définit la politique de tri du cache.
            Options.Politique := Politique_de_tri'Value(Argument(i+1));
            i := i+2;
        elsif Argument(i) = "-s" then   -- L'utilisateur demande d'afficher les statistiques.
            Options.Statistiques := True;
            i := i+1;
        elsif Argument(i) = "-S" then   -- L'utilisateur demande de ne pas afficher les statistiques.
            Options.Statistiques := False;
            i := i+1;
        elsif Argument(i) = "-t" then   -- L'utilisateur donne le nom du fichier de la table de routage.
            Options.Routage := To_Unbounded_String(Argument(i+1));
            i := i+2;
        elsif Argument(i) = "-p" then   -- L'utilisateur donne le nom du fichier des paquets.
            Options.paquets := To_Unbounded_String(Argument(i+1));
            i := i+2;
        elsif Argument(i) = "-r" then   -- L'utilisateur donne le nom qu'il souhaite pour le fichier des resultats.
            Options.Resultats := To_Unbounded_String(Argument(i+1));
            i := i+2;
        else
            Put_Line ("L'option entree: " & Argument(i) & " n'est pas reconnue mais ignoree.");
            i := i+1;
        end if;
    end loop;

    -- Lire la table de routage.

    Create (Sortie, Out_File, To_String (Options.Resultats));   -- Crée le fichier des résultats et l'ouvre.
    Open (Paquet, in_File, To_String(Options.Paquets));         -- Ouvre le fichier des paquets choisi par l'utilisateur.
    Initialiser (Table);                                        -- Initialise la LCA contenant la table de routage.
    Table_Routage (Table, Options.Routage);

    while not End_Of_File (Paquet) and not Arret_Lecture_Paquets loop  -- Parcourt ligne par ligne le fichier des paquets jusqu'à sa fin.
        Ligne_Paquets := Get_Line (Paquet); -- Récupère une nouvelle ligne du fichier des paquets.

        -- Interpréter une ligne du fichier paquets.
        if Ligne_Paquets = "table" then     -- Si "table", afficher le contenu de la table de routage.
            Put ("table (ligne");
            Put (Integer'Image(i_ligne_paquet));
            Put (")");
            New_Line;
            Pour_Chaque_Ecrire (Table);
            New_Line;
        elsif Ligne_Paquets = "cache" then  -- Si "cache", on affiche le contenu du cache.
            Put ("cache (ligne");
            Put (Integer'Image(i_ligne_paquet));
            Put (")");
            New_Line;
            Pour_Chaque_Ecrire (Cache);
            New_Line;
        elsif Ligne_Paquets = "fin" then    -- Si "fin", on arrête le traitement des paquets.
            Put ("fin (ligne");
            Put (Integer'Image(i_ligne_paquet));
            Put (")");
            Arret_Lecture_Paquets := True;
        elsif Ligne_Paquets = "stat" then   -- Si "stat", on affiche les statistiques.
             if Options.Statistiques then
                Put("Nombre de défauts de cache : ");
                Put (Integer'Image (Nb_Defaut_Cache));
                New_Line;
                Put("Nombre de demandes de routes : ");
                Put (Integer'Image (Nb_Demande_Route));
                New_Line;
                Put("Taux de défaut de cache : ");
                Put (Float'Image (Taux_Defaut_Cache));
                New_Line;
            else 
                null;
            end if;
        elsif Ligne_Paquets = "" then       -- Ignorer les lignes vides.
            null;
        else                                -- Trouver la route correspondant au paquet.
            -- Choisir la meilleure route pour ce paquet.
            begin
                -- Trouver la meilleure route pour ce paquet dans le cache.
                Route_Enregistree_Dans_Cache := true;
                Meilleure_route := Meilleure_Route_Paquet_LCA (Cache, convertir_adresse_entier(Ligne_Paquets));

                exception
                    -- Trouver la meilleure route pour ce paquet dans la table de routage.
                    -- Cela se produit lorsqu'on ne trouve pas de route correspondante dans le cache: on a un défaut de cache.
                    when Aucune_Route_Valide => Meilleure_route := Meilleure_Route_Paquet_LCA (Table, convertir_adresse_entier(Ligne_Paquets));
                                                Route_Enregistree_Dans_Cache := false;
                                                Nb_Defaut_Cache := Nb_Defaut_Cache + 1;                                        
            end;
            Nb_Demande_Route := Nb_Demande_Route + 1;
            Taux_Defaut_Cache := float(Nb_Defaut_Cache) / float(Nb_Demande_Route);

            -- Écrire dans le fichier résultats.
            ligne := creer_ligne (convertir_adresse_entier(Ligne_Paquets), Meilleure_route.eth);
            Put (Sortie, ligne);
            New_Line (Sortie);

            -- Mettre à jour le cache.

            if Taille (Cache) = Options.Taille and not Route_Enregistree_Dans_Cache then
                -- Trier le cache.
                case Options.Politique is
                    when FIFO => Trier_FIFO (Cache);
                    when LRU => Trier_LRU (Cache);
                    when LFU => Trier_LFU (Cache);
                end case;
            else
                null;
            end if;

            -- Enregistrer éventuellement la route dans le cache.
            if not Route_Enregistree_Dans_Cache then
                Destination_A_Enregistrer_Dans_Cache := Coherence (Table, convertir_adresse_entier(Ligne_Paquets), Meilleure_route.eth);
                
                Enregistrer (Cache, Destination_A_Enregistrer_Dans_Cache.Destination, Destination_A_Enregistrer_Dans_Cache.Masque, Destination_A_Enregistrer_Dans_Cache.eth);
            else
                Destination_A_Enregistrer_Dans_Cache := Meilleure_route;
            end if;

            -- Actualiser les statistiques d’utilisation de la route. (utiles pour les politiques LFU et LRU)
            Actualiser_Stats (Cache, Destination_A_Enregistrer_Dans_Cache.Destination, i_ligne_paquet);
        end if;

        i_ligne_paquet := i_ligne_paquet + 1;
    end loop;

    Close (Sortie); -- Ferme le fichier des resultats.
    close (Paquet); -- Ferme le fichier des paquets.
end Routeur_LL;