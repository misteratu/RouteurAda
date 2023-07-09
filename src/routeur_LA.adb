with Ada.Strings;                use Ada.Strings;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;   use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;           use Ada.Command_Line;
with exploitation_table_routage; use exploitation_table_routage;
with arbre;                      use arbre;
with cache;                      use cache;
with utilitaire;                use utilitaire;


procedure routeur_LA is
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
    j : Integer;
    -- Affiche les resultats pour l'utilisateur.

    -- Statistiques du cache.
    Nb_Demande_Route : Integer := 0;
    Nb_Defaut_Cache : Integer := 0;
    Taux_Defaut_Cache : Float := 0.0;

    procedure Ecrire (destination : T_Adresse_IP; masque : T_Adresse_IP; eth : Unbounded_String) is
        ligne : Unbounded_String;   -- Ligne à afficher à la demande de l'utilisateur.
        longueur : Integer;         -- Longueur de la destination ou du masque.
    begin
        for i in 1..3 loop -- De 1 à 3 car on ne souhaite pas avoir de points apres le dernier octet.
            longueur := Length(To_Unbounded_String(Integer'Image(ieme_octet (destination, i))));
            ligne := ligne & To_Unbounded_String(Integer'Image(ieme_octet (destination, i))(2..longueur)) & "."; -- On commence par le deuxième car à cause de "Integer'Image", il y'a la création d'un espace au début de la chaine de caractère
        end loop;
        longueur := Length(To_Unbounded_String(Integer'Image(ieme_octet (destination, 4))));
        ligne := ligne & To_Unbounded_String(Integer'Image(ieme_octet (destination, 4))(2..longueur));
        ligne := ligne & " ";

        for i in 1..3 loop -- De 1 à 3 car on ne souhaite pas avoir de points apres le dernier octet.
            longueur := Length(To_Unbounded_String(Integer'Image(ieme_octet (masque, i))));
            ligne := ligne & To_Unbounded_String(Integer'Image(ieme_octet (masque, i))(2..longueur)) & "."; -- On commence par le deuxième car à cause de "Integer'Image", il y'a la création d'un espace au début de la chaine de caractère
        end loop;
        longueur := Length(To_Unbounded_String(Integer'Image(ieme_octet (masque, 4))));
        ligne := ligne & To_Unbounded_String(Integer'Image(ieme_octet (masque, 4))(2..longueur));
        ligne := ligne & " " & eth; -- Rajoute l'interface à la ligne.

        Put (ligne);
        New_Line;
    end Ecrire;

    procedure Pour_Chaque_Ecrire is new Pour_Chaque (Traiter => Ecrire);
    procedure Pour_Chaque_Ecrire_arbre is new Pour_Chaque_arbre (Traiter => Ecrire);

    Options : Arguments_Routeur;                -- Enumération des options.
    Sortie : File_Type;                         -- Fichier des resultats.
    Paquet : File_Type;                         -- Fichier des paquets.
    table : T_LCA;                            -- Table de routage sous forme d'une liste chainée.
    Meilleure_route : T_Donnee;                 -- La meilleure route correspondant au paquet.
    ligne : Unbounded_String;                   -- Ligne à écrire dans le fichier des resultats.
    Arret_Lecture_Paquets : Boolean := False;   -- Sortie de la boucle de lecture si "Fin".
    Ligne_Paquets : Unbounded_String;           -- Une ligne du fichier des paquets.
    Liste_Cache : T_LCA;
    Cache_Arbre : T_Arbre;
    Route_A_supprimer : T_Adresse_IP;           -- La route que l'on devra supprimer si l'on dépasse la taille du cache.
    Meilleure_route_precoherence : T_Donnee;
begin
    -- Initialiser les options du routeurs avec les parametres par defaut.
    Options.Taille := 10;
    Options.Politique := FIFO;
    Options.Statistiques := True;
    Options.Routage := To_Unbounded_String ("table.txt");
    Options.Paquets := To_Unbounded_String ("paquets.txt");
    Options.Resultats := To_Unbounded_String ("resultats.txt");

    i := 1;
    while i <= Argument_Count loop  -- Parcourt les options rajoutées par l'utilisateur.
        if Argument(i) = "-c" then -- L'utilisateur définit la taille du cache.
            Options.Taille := Integer'Value(Argument(i+1));
            i := i+2;
        elsif Argument(i) = "-P" then -- L'utilisateur définit la politique de tri du cache.
            Options.Politique := Politique_de_tri'Value(Argument(i+1));
            i := i+2;
        elsif Argument(i) = "-s" then -- L'utilisateur demande d'afficher les statistiques.
            Options.Statistiques := True;
            i := i+1;
        elsif Argument(i) = "-S" then -- L'utilisateur demande de ne pas afficher les statistiques.
            Options.Statistiques := False;
            i := i+1;
        elsif Argument(i) = "-t" then -- L'utilisateur donne le nom du fichier de la table de routage.
            Options.Routage := To_Unbounded_String(Argument(i+1));
            i := i+2;
        elsif Argument(i) = "-p" then -- L'utilisateur donne le nom du fichier des paquets.
            Options.paquets := To_Unbounded_String(Argument(i+1));
            i := i+2;
        elsif Argument(i) = "-r" then -- L'utilisateur donne le nom qu'il souhaite pour le fichier des resultats.
            Options.Resultats := To_Unbounded_String(Argument(i+1));
            i := i+2;
        else
            Put_Line ("L'option entree: " & Argument(i) & " n'est pas reconnue mais ignoree.");
            i := i+1;
        end if;
    end loop;

    Create (Sortie, Out_File, To_String (Options.Resultats)); -- Crée le fichier resultat.txt et l'ouvre.
    Open (Paquet, in_File, To_string(Options.Paquets)); -- Ouvre le fichier des paquets choisi par l'utilisateur.
    Initialiser(Liste_Cache);
    Initialiser(table); -- Initialise la liste chainée.
    Initialiser(Cache_Arbre); -- Initialise le cache sous forme d'arbre.
    Table_Routage(table, Options.Routage); -- Crée une liste chainée contenant la table de routage.
    j := 1;
    while not End_Of_File(Paquet) and not Arret_Lecture_Paquets loop -- Parcourt le fichier paquet jusqu'à sa fin.
        Ligne_Paquets := Get_Line (Paquet); -- Récupère une ligne du fichier des paquets

        if Ligne_Paquets = "table" then -- Si "table" dans les paquets, afficher la table.
            Put("table (ligne");
            Put(Integer'Image(j));
            Put(")");
            New_Line;
            Pour_Chaque_Ecrire(table);
            New_Line;
        elsif Ligne_Paquets = "fin" then -- Si "Fin", on arrete la lecture.
            Put("fin (ligne");
            Put(Integer'Image(j));
            Put(")");
            Arret_Lecture_Paquets := True;
        elsif Ligne_Paquets = "cache" then
            Put ("cache (ligne");
            Put (Integer'Image(j));
            Put (")");
            New_Line;
            Pour_Chaque_Ecrire_arbre (Cache_Arbre);
            New_Line;
        elsif Ligne_Paquets = "stat" then
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
        elsif Ligne_Paquets = "" then
            null;
        else
            -- Trouver la route correspondant aux paquets 
            Nb_Demande_Route := Nb_Demande_Route + 1;
            Meilleure_route_precoherence := meilleure_route_paquet_routeur_arbre (table, Cache_Arbre, convertir_adresse_entier(Ligne_Paquets), Nb_Defaut_Cache); -- Récupère la meilleure route correspondant au paquet dans la table de routage.
            Taux_Defaut_Cache := float(Nb_Defaut_Cache) / float(Nb_Demande_Route);
            -- Gérer le cache en fonction de sa taille.
            Meilleure_route := Coherence(table, convertir_adresse_entier(Ligne_Paquets), Meilleure_route_precoherence.eth);
            if Dans_Cache(Liste_Cache, Meilleure_route) then
                Mise_a_jour_LRU(Liste_Cache, Meilleure_route); -- Remet le route; au début de liste_cache
            elsif Taille(Liste_Cache) < Options.Taille then
                Enregistrer(Liste_Cache, Meilleure_route.Destination, Meilleure_route.Masque, Meilleure_route.eth); -- Enregistrer l'ordre des routes.
                Enregistrer_Cache_Arbre(Cache_Arbre, Meilleure_route.Destination, Meilleure_route.Masque, Meilleure_route.eth); -- Enregistrer la route dans un arbre (le cache)
            else
                --Supprimer le dernier élément
                Route_A_supprimer := Trier_LRU_Arbre (Liste_Cache);
                -- Ajouter l'élément souhaité
                Enregistrer(Liste_Cache, Meilleure_route.Destination, Meilleure_route.Masque, Meilleure_route.eth);   -- Enregistrer l'ordre des routes.
                Supprimer_Arbre(Cache_Arbre, Route_A_supprimer); -- Supprimer une destination dans un arbre
                Enregistrer_Cache_Arbre(Cache_Arbre, Meilleure_route.Destination, Meilleure_route.Masque, Meilleure_route.eth); -- Enregistrer la route dans un arbre (le cache)
            end if;
            ligne := creer_ligne(convertir_adresse_entier(Ligne_Paquets), Meilleure_route.eth); -- Crée la ligne contenant la destination et l'interface de la meilleure route.
            Put (Sortie, ligne); -- Ecrit dans le fichier des resultats la ligne des donées.
            New_Line (Sortie);
        end if;
        j := j + 1;
    end loop;

    Close (Sortie); -- Ferme le fichier des resultats.
    close (Paquet); -- Ferme le fichier des paquets.
end Routeur_LA;







