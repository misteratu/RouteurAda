with Ada.Unchecked_Deallocation; 
with SDA_Exceptions;         use SDA_Exceptions;
with Routeur_Exceptions;         use Routeur_Exceptions;
with utilitaire;                 use utilitaire;

package body cache is
    procedure Free is
            new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_LCA);

    -- Créer une LCA vide.
    procedure Initialiser (Sda: out T_LCA) is
    begin
        Sda := null;
    end Initialiser;

    -- Vérifier si la LCA est vide. 
    function Est_Vide (Sda : T_LCA) return Boolean is
    begin
        return Sda = null;
    end Est_Vide;

    -- Déterminer la taille de la liste
    -- On parcours la liste en incrementant jusqu'au moment ou l'on rencontre null
    function Taille (Sda : in T_LCA) return Integer is
    begin
        if Est_Vide(Sda) then
            return 0;
        else
            return 1 + Taille(Sda.all.Suivant);
        end if;
    end Taille;

    -- Enregistrer permet d'enregistrer une cle et une donnee dans une liste chainee
    -- Sinon on parcours la liste juqu'à trouver notre cle et si la cle est presente alors on affecte la donnee souhaitee à la cle correspondant à la donnee 
    -- Si la liste est vide ou que la liste ne contien pas la cle alors on ajoute une cellule dans la liste pour enregistrer Cle et donnee
    procedure Enregistrer (Sda : in out T_LCA ; Destination : in T_Adresse_IP ; Masque : in T_Adresse_IP ; eth : in Unbounded_String) is
    begin
        if Est_Vide(Sda) then	
            Sda := new T_Cellule'(Destination, Masque, eth, 0, 0, null);
        else
            Enregistrer (Sda.all.suivant, Destination, Masque, eth);
        end if;
    end Enregistrer;

    -- Cle_presente permet de verifier si une cle est presente ou non dans une liste chainee 
    -- Pour cela, on parcours la liste et si on trouve la cle souhaitee alors on renvoie True sinon False
    function Destination_Presente (Sda : in T_LCA ; Destination : in T_Adresse_IP) return Boolean is
    begin
        if Est_Vide(Sda) then 
            return False;
        else
            if Sda.all.Destination = Destination then 
                return True;
            else 
                return Destination_Presente(Sda.all.Suivant, Destination);
            end if;
        end if;		
    end Destination_Presente;

    -- Supprimer permet de supprimer la cle et sa donnee dans une liste
    -- On verifie que la liste contient bien la cle sinon on renvoie une exeption
    -- Si la cle est presente alors on parcours la liste jusqu'à arriver à la cellule contenant la cle voulue 
    -- On supprime ce terme en affectant à la sda son terme suivant 
    -- On free la Sda pour eviter les pertes de paquets
    -- On affecte à Sda la valeur de la sda suivant stockee precedement
    procedure Supprimer (Sda : in out T_LCA ; Destination : in T_Adresse_IP) is
        Sdaprime : T_LCA;
    begin
        if Est_Vide(Sda) then
            raise Cle_Absente_Exception;
        elsif Sda.all.Destination = Destination then
            Sdaprime := Sda.all.Suivant;
            Free(Sda);
            Sda := Sdaprime;
        else
            Supprimer(Sda.all.Suivant, Destination);
        end if;
    end Supprimer;

    procedure Vider (Sda : in out T_LCA) is
        I : Integer;
        Index : Integer;
        A_Detruire : T_LCA;
        Curseur : T_LCA;
    begin
        I := Taille(Sda);
        for Indice in reverse 0..I loop
            if Indice = 0 then -- suppression au debut ?
                if Sda = null then
                    null;
                else
                    A_Detruire := Sda;
                    Sda := Sda.all.Suivant;
                    Free (A_Detruire);
                end if;
            else
                -- Trouver le (I-1)eme element
                Index := 0;
                Curseur := Sda;
                while Curseur /= null and then Index < Indice - 1 loop
                    Curseur := Curseur.all.Suivant;
                    Index := Index + 1;
                end loop;

                -- Supprimer l"element
                if Curseur = null or else Curseur.all.Suivant = null then
                    null;
                else
                    A_Detruire := Curseur.all.Suivant;
                    Curseur.all.Suivant := A_Detruire.all.Suivant;
                    Free (A_Detruire);
                end if;

            end if;
        end loop;
    end Vider;

    procedure Pour_Chaque (Sda : in T_LCA) is
    begin
        if Est_Vide(Sda) then
            null;
        else
            begin
                Traiter(Sda.all.Destination, Sda.all.Masque, Sda.all.eth);
            exception
                when others => null;
            end;
            Pour_Chaque(Sda.all.Suivant);
        end if;
    end Pour_Chaque;


    -- Cherche la meilleure route correspondant à un paquet dans la table de routage.
    function Meilleure_Route_Paquet_LCA (Table : in T_LCA; Paquet : in T_Adresse_IP) return T_Donnee is 
        Nombre_de_bits_du_masque_differents_de_0 : Integer;             -- Compte le nombre de bits différents de 0.
        Meilleur_Nombre_de_bits_du_masque_differents_de_0 : Integer;    -- Compte le plus grand nombre de bits différents de 0 entre les differents masques de la table de routage (Pour obtenir le masque maximal).
        Parcours_table : T_LCA;                                         -- Liste qui permet de parcourir la liste en la gardant en mémoire.        
        Meilleur_route : T_Donnee ;                                     -- La route qui a le masque le plus long et qui correspond au paquet.
        A_trouve_une_route : Boolean := False;                          -- Vérifie que le paquet correspond bien à une route dans la table de routage.   
    begin
        Parcours_table := Table;
        Meilleur_Nombre_de_bits_du_masque_differents_de_0 := 0;
        for i in 1..Taille(Table) loop
            -- Traiter la correspondance entre le paquet et la destination de la table.
            Nombre_de_bits_du_masque_differents_de_0 := 0;
            if (Paquet and Parcours_table.all.Masque) = Parcours_table.all.Destination then -- Vérifie que la paquet correspond à la route (Destination, Masque).
                Nombre_de_bits_du_masque_differents_de_0 := Longueur_Masque (Parcours_table.all.Masque);
                A_trouve_une_route := True; -- Vérifie que le paquet correspond bien à une route dans la table de routage.

                -- Déterminer si cette route est meilleure que les précédentes.
                if Nombre_de_bits_du_masque_differents_de_0 >= Meilleur_Nombre_de_bits_du_masque_differents_de_0 then 
                    Meilleur_Nombre_de_bits_du_masque_differents_de_0 := Nombre_de_bits_du_masque_differents_de_0;
                    Meilleur_route := (Parcours_table.all.Destination, Parcours_table.all.Masque, Parcours_table.all.eth);
                else 
                    null;
                end if;
            else 
                null;
            end if;		
            
            Parcours_table := Parcours_table.all.Suivant ;
        end loop;

        if not A_trouve_une_route then  --Exception si aucune route valide pour un paquet
            raise Aucune_Route_Valide;
        end if;
        return Meilleur_route;
    end Meilleure_Route_Paquet_LCA;

    -- Cherche la meilleure route correspondant à un paquet dans la table de routage.
    -- et assure la cohérence du cache.
    function Coherence (Table_routage : in T_LCA; Route : in T_Adresse_IP; Eth : in Unbounded_String) return T_Donnee is 
        Longueur : Integer;             -- Compte le nombre de bits différents de 0.
        Meilleure_Longueur : Integer;    -- Compte le plus grand nombre de bits différents de 0 entre les differents masques de la table de routage (Pour obtenir le masque maximal).
        Parcours_table_routage : T_LCA;    -- Liste qui permet de parcourir la liste en la gardant en mémoire.        
        Route_Meilleure_Coherence : T_Donnee;
        Masque_temp : T_Adresse_IP;
    begin
        Parcours_table_routage := Table_Routage;
        Meilleure_Longueur := 0;

        for i in 1..Taille(Table_Routage) loop -- Parcourt la liste Table_Routage.
            Masque_temp := (Route and Parcours_table_routage.Destination) or ((not Route) and (not Parcours_table_routage.Destination));
            Longueur := Longueur_Masque (Masque_temp);
            Masque_temp := Creer_Masque (Longueur + 1);

			-- Determine la route qui a le masque le plus long et qui correspond au paquet (Recherche d'un maximum).
            if Longueur >= Meilleure_Longueur then 
                Meilleure_Longueur := Longueur;
                Route_Meilleure_Coherence := (Route and Masque_temp, Masque_temp, Eth);
            else 
                null;
            end if;

            Parcours_table_routage := Parcours_table_routage.all.Suivant ;
        end loop;

        return Route_Meilleure_Coherence;
    end Coherence;


    procedure Mise_a_jour_LRU(liste : in out T_LCA ; route : in T_Donnee) is
    begin
        Supprimer (liste, route.Destination);
        Enregistrer(liste, route.Destination, route.Masque, route.eth);
    end Mise_a_jour_LRU;    

    function Dans_Cache (Cache : in T_LCA ; route : in T_Donnee) return Boolean is
    begin
        if Est_Vide(Cache) then
            return False;
        elsif Cache.all.Destination = route.Destination and Cache.all.Masque = route.Masque and Cache.all.eth = route.eth then
            return True;
        else
            return Dans_Cache(Cache.all.Suivant,route);
        end if;
    end Dans_Cache;

    procedure Supprimer_Case_1 (Liste : in out T_LCA) is
        Parcours_liste : T_LCA;
    begin   
        Parcours_liste := Liste;
        Liste := Liste.all.suivant;
        Free (Parcours_liste); 
    end Supprimer_Case_1;

-- Tris 

    procedure Actualiser_Stats (Cache : in out T_LCA; Destination : T_Adresse_IP; nb_iterations : Integer) is
    begin
        if Est_Vide (Cache) then 
            null;
        else
            -- Actualiser les statistiques d’utilisation de la route.
            if Cache.all.Destination = Destination then
                Cache.all.Derniere_Utilisation := nb_iterations;                -- Dernière utilisation (LRU).
                Cache.all.Nb_Utilisations := Cache.all.Nb_Utilisations + 1;     -- Nombre d'utilisations (LFU).
            else 
                Actualiser_Stats (Cache.all.Suivant, Destination, nb_iterations);
            end if;
        end if;		
    end Actualiser_Stats;


    -- Supprime un élément du cache selon une politique FIFO.
    procedure Trier_FIFO (Cache : in out T_LCA) is
	    Premiere_case : T_LCA;
	begin
        Premiere_case := Cache;
        Cache := Cache.Suivant;
        Free (Premiere_case);
    end Trier_FIFO;

    -- Supprime un élément du cache selon une politique LRU.
    procedure Trier_LRU (Cache : in out T_LCA) is
        Destination_LRU : T_Adresse_IP; -- La route LRU.
        Numero_LRU : Integer;           -- Dernière utilisation de la route LRU.
        Courant : T_LCA;
    begin
        -- On détermine la route du cache la moins récemment utilisée.
        Courant := Cache;
        Destination_LRU := Courant.All.Destination;
        Numero_LRU := Courant.All.Derniere_Utilisation;
        while not Est_Vide (Courant) loop
            if Courant.All.Derniere_Utilisation < Numero_LRU then
                Numero_LRU := Courant.All.Derniere_Utilisation;
                Destination_LRU := Courant.All.Destination;
            else
                null;
            end if;
            Courant := Courant.All.Suivant;
        end loop;

        -- On supprime cette route du cache.
        Supprimer (Cache, Destination_LRU);
    end Trier_LRU;

    --La seule différence avec Trier_LRU c'est qu'ici on retourne l'élément que l'on supprimer pour le supprimer dans l'arbre.
    function Trier_LRU_Arbre (Cache : in out T_LCA) return T_Adresse_IP is
        Destination_LRU : T_Adresse_IP; -- La route LRU.
    begin
        -- On détermine la route du cache la moins récemment utilisée.
        Destination_LRU := Cache.all.destination;
        -- On supprime cette route du cache.
        Supprimer (Cache, Destination_LRU);
        return Destination_LRU;
    end Trier_LRU_Arbre;

    -- Supprime un élément du cache selon une politique LFU.
    procedure Trier_LFU (Cache : in out T_LCA) is
        Destination_LFU : T_Adresse_IP; -- La route LFU.
        Frequence_LFU : Integer;        -- Nombre d'utilisations de la route LFU.
        Courant : T_LCA;
    begin
         -- On détermine la route du cache la moins souvent utilisée.
        Courant := Cache;
        Destination_LFU := Courant.All.Destination;
        Frequence_LFU := Courant.All.Nb_Utilisations;
        while not Est_Vide (Courant) loop
            if Courant.All.Nb_Utilisations < Frequence_LFU then
                Frequence_LFU := Courant.All.Nb_Utilisations;
                Destination_LFU := Courant.All.Destination;
            else
                null;
            end if;

            Courant := Courant.All.Suivant;
        end loop;

        -- On supprime cette route du cache.
        Supprimer (Cache, Destination_LFU);
    end Trier_LFU;
end Cache;