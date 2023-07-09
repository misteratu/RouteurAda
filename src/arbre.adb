with Ada.Unchecked_Deallocation; 
with utilitaire;                 use utilitaire;
package body arbre is

    procedure Free is 
        new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_Arbre);

    -- Initialiser crée un arbre qui est null
    procedure Initialiser(Arbre_binaire : out T_Arbre) is 
    begin 
        Arbre_binaire := null;
    end Initialiser;

    -- Créer des noeuds vides

    procedure noeuds_vides (Arbre : in T_Arbre ; indice_de_parcours : in Integer ; destination_bit : in Unbounded_String) is 
    begin 
        if To_String(destination_bit)(indice_de_parcours) = '0' then 
            Arbre.all.gauche := new T_Cellule'(0, 0, To_Unbounded_String("Noeud_Vide"), null, null); -- Créer des noeuds vides 
        else 
            Arbre.all.droite := new T_Cellule'(0, 0, To_Unbounded_String("Noeud_Vide"), null, null); -- Créer des noeuds vides 
        end if;
    end noeuds_vides;

    -- Créer un noeud au niveau du bit de différence

    procedure noeud_bit_difference(Arbre : in out T_Arbre ; destination_bit :in  Unbounded_String ;destination_arbre : in Unbounded_String ; indice_de_parcours : in out Integer ; stockage : in out T_Donnee ; masque : in T_Adresse_IP ; eth : in Unbounded_String) is
    begin
        if To_String(Destination_bit)(indice_de_parcours) = To_String(destination_arbre)(indice_de_parcours) and indice_de_parcours < 32 then
            noeuds_vides(Arbre, indice_de_parcours, destination_bit); -- Tant que les deux adresses sont identiques on poursuit la descente dans l'arbre 
            
            -- Poursuivre la création de noeuds vides

            if To_String(Destination_bit)(indice_de_parcours) = '0' then 
                indice_de_parcours := indice_de_parcours + 1;
                noeud_bit_difference(Arbre.all.gauche , destination_bit, destination_arbre ,indice_de_parcours, stockage, masque, eth); 
            else 
                indice_de_parcours := indice_de_parcours + 1;
                noeud_bit_difference(Arbre.all.droite, destination_bit, destination_arbre, indice_de_parcours, stockage , masque , eth); 
            end if;
        else
            branche_g_et_d_noeud_difference (Arbre ,indice_de_parcours ,stockage ,destination_bit ,Masque ,Eth); -- Dédouble les deux adresses au bit de différence 
        end if;
    end noeud_bit_difference;

    -- Créer deux branches gauche et droite après le nœud de différence

    procedure branche_g_et_d_noeud_difference (Arbre : in out T_Arbre ; indice_de_parcours : in Integer; stockage : in out T_Donnee ; destination_bit : in Unbounded_String ; Masque : in T_Adresse_IP ; Eth : in Unbounded_String) is
    begin 
    if To_String(binaire(stockage.destination))(indice_de_parcours) = '0' then  
	
        Arbre.all.gauche := new T_Cellule'(Stockage.destination, Stockage.Masque, Stockage.eth, null, null); -- Déplacer la route rencontrée en bas à gauche

        Arbre.all.droite := new T_Cellule'(String_En_Adresse_IP (destination_bit), Masque, Eth, null, null); -- Placer l'adresse voulue à droite

        Arbre.all.eth := To_Unbounded_String("Noeud_Vide"); -- On affecte noeuds vide au noeud du dessus qui contenait l'adresse rencontrée
        
    else 	
        Arbre.all.droite := new T_Cellule'(Stockage.destination, Stockage.Masque, Stockage.eth, null, null); -- Déplacer la route rencontrée en bas à droite

        Arbre.all.gauche := new T_Cellule'(String_En_Adresse_IP (destination_bit), Masque, Eth, null, null); -- Placer l'adresse voulue à gauche

        Arbre.all.eth := To_Unbounded_string("Noeud_Vide"); -- On affecte noeuds vide au noeud du dessus qui contenait l'adresse rencontrée
        
    end if;


    end branche_g_et_d_noeud_difference;

    -- Enregistrer la route dans un arbre

    procedure Enregistrer_Cache_Arbre (Arbre : in out T_Arbre ; adresse : in out T_Adresse_IP ; masque : in T_Adresse_IP ; Eth : in Unbounded_String) is
    indice_de_parcours : Integer;  -- Indice pour parcourir et comparer les bits 
    begin
    indice_de_parcours := 1; 
    enregistrer_arbre(Arbre, adresse, masque, Eth, indice_de_parcours); -- Appel à la fonction récursive qui permet d'avoir l'indice de parcours
    
    end Enregistrer_Cache_Arbre;

    -- Enregistrer une route dans un arbre

    procedure enregistrer_arbre (Arbre : in out T_Arbre ; adresse : in out T_Adresse_IP ; masque : in T_Adresse_IP ; Eth : in Unbounded_String ; indice_de_parcours : in out Integer) is
    destination_bit : Unbounded_String;     -- adresse en binaire
    Stockage : T_Donnee;                    -- Stockage d'une route (celle rencontrée lors d'un parcours)
    destination_arbre : Unbounded_String;   -- Destination de l'arbre rencontrée

    begin
  
    destination_bit := binaire(adresse);
    
    if Arbre = null then
        Arbre:= new T_Cellule'(adresse, Masque, Eth, null, null); -- Si l'arbre ne contient rien alors création d'une cellule

    elsif Arbre.all.droite = null and Arbre.all.gauche = null then

        -- Enregistrer lorsque l’arbre ne possède ni fils droit ni fils gauche

        if  Arbre.all.destination=String_En_Adresse_IP(Destination_bit) then
            
            null;
        else
               
            Stockage := (Arbre.all.Destination, Arbre.all.Masque, Arbre.all.eth); -- Stocker l'information qui sera perdue à la création des deux feuilles
            Arbre.all.eth := To_Unbounded_String("Noeud_Vide"); -- Rendre le noeuds vide 
            destination_arbre := binaire(Arbre.all.Destination); -- Convertir en binaire l'adresse de l'ancien noeud
            noeud_bit_difference(Arbre, destination_bit,destination_arbre,indice_de_parcours, stockage , masque , eth);  -- Créer un noeud au niveau du bit de différence

        end if;

    elsif Arbre.all.droite /= null and Arbre.all.gauche /= null then
    

        -- Enregistrer lorsque l’arbre possède un fils droit et un fils gauche

        if To_String(Destination_bit)(indice_de_parcours ) = '0' then 

            indice_de_parcours := indice_de_parcours + 1;
            enregistrer_arbre(Arbre.all.gauche, adresse, masque, Eth, indice_de_parcours); -- Continuer le parcours à gauche car le bit est 'O'
        else
            indice_de_parcours := indice_de_parcours + 1;
            enregistrer_arbre(Arbre.all.droite, adresse, masque, Eth, indice_de_parcours); -- Continuer le parcours à droite car le bit est '1'
        end if;

        -- On ne prend pas en compte le cas ou les noeuds sont des noeuds non vides car par l'appel recursif on repart vers le cas nul à gauche et à droite

    elsif Arbre.all.gauche /= null and Arbre.all.droite = null then

        -- Enregistrer lorsque l’arbre possède un fils droit mais pas un fils gauche

        if  Arbre.all.gauche.all.eth /= "Noeud_Vide" and Arbre.all.gauche.all.Destination = String_En_Adresse_IP(Destination_bit) then
            
                null;
        elsif To_String(Destination_bit)(indice_de_parcours ) = '0' and Arbre.all.gauche.all.eth /= "Noeud_Vide" then

            Stockage := (Arbre.all.gauche.all.Destination, Arbre.all.gauche.all.Masque, Arbre.all.gauche.all.eth); -- Stocker l'information qui sera perdue à la création des deux feuilles
            destination_arbre := binaire(Arbre.all.gauche.all.Destination); -- Convertir en binaire l'adresse de l'ancien noeud
            Arbre.all.gauche.all.eth := To_Unbounded_String("Noeud_Vide");
            noeud_bit_difference(Arbre.all.gauche, destination_bit, destination_arbre, indice_de_parcours, stockage , masque , eth); -- Créer un noeud au niveau du bit de différence

        elsif To_String(Destination_bit)(indice_de_parcours) = '0' and Arbre.all.gauche.all.eth = "Noeud_Vide" then 
            
            Indice_de_parcours := indice_de_parcours + 1; -- Incrémenter l'indice de parcours car on part à gauche  
            Enregistrer_arbre (Arbre.all.gauche, adresse, masque, Eth, indice_de_parcours); -- Parcours de nouveau à gauche l'arbre 
            
        else
    
            Arbre.all.droite := new T_Cellule'(String_En_Adresse_IP (destination_bit), Masque, Eth, null, null); -- Crée une nouvelle cellule si on doit sortir de l'arbre
        end if;
        
    elsif Arbre.all.droite /= null and Arbre.all.gauche = null then 

        -- Enregistrer lorsque l’arbre possède un fils gauche mais pas un fils droit

        if Arbre.all.droite.all.eth /= "Noeud_Vide" and Arbre.all.droite.all.Destination = String_En_Adresse_IP(Destination_bit) then
            null;      
        
        elsif To_String(Destination_bit)(indice_de_parcours) = '1' and Arbre.all.droite.eth /= "Noeud_Vide" then   

            Stockage := (Arbre.all.droite.all.Destination, Arbre.all.droite.all.Masque, Arbre.all.droite.all.eth); -- Stocker l'information qui sera perdue à la création des deux feuilles
            destination_arbre := binaire(Arbre.all.droite.all.Destination); -- Convertir en binaire l'adresse de l'ancien noeud
            Arbre.all.droite.all.eth := To_Unbounded_String("Noeud_Vide");
            noeud_bit_difference(Arbre.all.droite, destination_bit,destination_arbre, indice_de_parcours, stockage , masque , eth); -- Créer un noeud au niveau du bit de différence

        elsif To_String(Destination_bit)(indice_de_parcours) = '1' and Arbre.all.droite.eth = "Noeud_Vide" then 
            Indice_de_parcours := indice_de_parcours + 1;	   
            Enregistrer_arbre (Arbre.all.droite, adresse, masque, Eth, indice_de_parcours); -- Parcours de nouveau à droite l'arbre 
        
        else 
            Arbre.all.gauche := new T_Cellule'(String_En_Adresse_IP (destination_bit), Masque, Eth, null, null); -- Crée une nouvelle cellule si on doit sortir de l'arbre
       
        end if;
    else 
        null;
    end if;
    end enregistrer_arbre;


    -- Choisir la meilleure route
    
    function meilleure_route_paquet_routeur_arbre (Table_routage : in T_LCA ; Cache : in T_Arbre ; Paquet : in T_Adresse_IP ; Nb_Defaut_Cache : in out Integer) return T_Donnee is
    paquet_bit : Unbounded_String;  -- Paquet exprimé en binaire
    parcours_Cache : T_Arbre;       -- Copie de arbre pour parcourir
    presence : Boolean;             -- Montre la présence ou non dans le cache d'une route adaptée 
    route : T_Donnee;               -- Route qui correspond au paquet
    Fin_Boucle : Boolean;           -- Booléen permetant de sortir de la boucle
    indice : Integer;               -- Permet de savoir quel bit des destinations comparer 
    begin
        Fin_Boucle := False;
        indice := 1;
        parcours_Cache := Cache;
        paquet_bit := binaire(paquet);
        presence := false;
        while not Fin_Boucle loop
        
            -- Rechercher dans le cache

            if parcours_Cache /= null then

                -- Continuer à chercher si noeud vide et s'arrêter sinon
 
                if parcours_Cache.all.eth /= To_Unbounded_String("Noeud_Vide") then
                
                    -- Rechercher la route si une destination a été trouvé

                    if (Paquet and Parcours_Cache.all.Masque) = Parcours_Cache.all.Destination then
                        presence := true;  -- Montrer que la route qui correspond au paquet est déjà dans le cache
                        Fin_Boucle := true; -- Met fin à la boucle
                        route := (parcours_Cache.all.Destination, parcours_Cache.all.Masque, parcours_Cache.all.eth); -- Enregistre la meilleure route
                    else 
                        Fin_Boucle := true;
                    end if;
                else 

                    -- Rechercher la route si on a trouvé Noeud Vide

                    if To_string(paquet_bit)(indice) = '0' then
                        Parcours_Cache := Parcours_Cache.all.gauche; -- Parcourir l'arbre à gauche
                        indice := indice + 1;            
                    else   
                        Parcours_Cache := Parcours_Cache.all.droite; -- Parcourir l'arbre à droite
                        indice := indice + 1; 
                    end if;
                end if;
            else 
                Fin_boucle := true; -- Fin de la boucle si on tombe sur null
            end if;
        end loop;
        if presence then 
            return route; -- Si une route du cache convient au paquet on renvoie la route
        else 
            Nb_Defaut_Cache := Nb_Defaut_Cache + 1;
            return Meilleure_Route_Paquet_LCA(Table_routage, Paquet); -- On cherche une route qui convient au paquet dans la table de routage 
        end if;
    end meilleure_route_paquet_routeur_arbre;

    -- supprimer les noeuds vides au dessus de l’élément supprimé

    procedure Supprimer_noeuds_vides(Cache : in out T_Arbre ; indice_pere : in Integer ; adresse : in T_Adresse_IP) is
    indice_progression : Integer; -- indice de progression dans l'arbre 
    begin
        for indice_suppression in reverse 1..indice_pere loop -- Cette boucle permet de parcourir l'arbre du plus bas au plus haut 
            indice_progression := 1;  -- on remonte au debut de l'arbre  
            Descente_Arbre(Cache, indice_pere, indice_progression, Adresse); -- Descendre l’arbre et supprimer le dernier Noeud Vide
        end loop; 
    end Supprimer_noeuds_vides;

    -- Descendre l’arbre et supprimer le dernier Noeud Vide

    procedure Descente_Arbre(Cache : in out T_Arbre ; indice_pere : in Integer ; indice_progression : in out Integer ;  Adresse : in T_Adresse_IP ) is
    begin
        if indice_progression < indice_pere then -- On continue de descendre tant qu'on a pas atteint l'élément au dessus de l'élément supprimé 

            -- Descendre l’arbre et supprimer le dernier Noeud Vide

            if To_String(binaire(Adresse))(indice_progression) = '0' then 
                indice_progression := indice_progression + 1;
                Descente_Arbre(Cache.all.gauche, indice_pere, indice_progression, Adresse); -- Descendre l'arbre à gauche
            else
                indice_progression := indice_progression + 1;
                Descente_Arbre(Cache.all.droite, indice_pere , indice_progression ,Adresse); -- Descendre l'arbre à droite
            end if;
        elsif To_String(binaire(Adresse))(indice_progression ) = '0' and Cache.all.droite = null then
            Free(Cache.all.gauche); -- On libère l'élément
        elsif To_String(binaire(Adresse))(indice_progression ) = '1' and Cache.all.gauche = null then
            Free(Cache.all.droite); -- On libère l'élément
        end if;
    end Descente_Arbre;

    -- Supprimer l’élément du cache
    
    procedure Supprimer_Arbre_element(Cache : in out T_Arbre ; Adresse : in T_Adresse_IP ; indice_pere : in out Integer) is  --On fait le choix de ne pas remonter les feuilles sinon la fonction supprimer aurait une trop grande complexité
        paquet_bit : Unbounded_String;   -- Paquet exprimé en binaire                    --Il peut y avoir des erreurs car si l'on ne remonte pas les feuilles on pourrait alors se dire qu'une route existe déjà alors que ce n'est pas le cas
    begin 
        if Cache = null then 
            null;
        else 
            -- supprimer si l’élément peut etre dans le cache

            paquet_bit := binaire(Adresse);
            if Cache.all.eth = To_Unbounded_String("Noeud_Vide") then
                indice_pere := indice_pere + 1;
                if To_string(paquet_bit)(indice_pere) = '0' then
                    Supprimer_Arbre_element(Cache.all.gauche, adresse, indice_pere); -- On parcours l'arbre à gauche car on a pas encore trouvé de noeud

                else   
                    Supprimer_Arbre_element(Cache.all.droite, adresse, indice_pere); -- On parcours l'arbre à droite car on a pas encore trouvé de noeud

                end if;
                
            elsif Cache.all.Destination = Adresse then 
                Free(Cache); -- Supprimer l'élément
            else
                null;
            end if;
        end if;
    end Supprimer_Arbre_element;

    -- Supprimer une destination dans un arbre

    procedure Supprimer_Arbre(Cache : in out T_Arbre ; Adresse : in T_Adresse_IP) is
    indice_pere : Integer; -- indice du noeud précedent l'élément à supprimer 
    begin 
        indice_pere := 0;
        Supprimer_Arbre_element(Cache, Adresse, indice_pere); -- Supprimer d'abord l'élément
        Supprimer_Noeuds_vides(Cache, indice_pere, Adresse); -- Puis supprimer les noeuds vides au dessus qui sont inutiles et qui detruiraient le fonctionnement de enregistrer 

    end Supprimer_Arbre;

    procedure Pour_Chaque_arbre (Arbre : in T_Arbre ) is
    begin
        if Arbre = null then -- On arrete de parcourir si l'arbre est null
            null;
        elsif Arbre.all.eth = To_Unbounded_String("Noeud_Vide") then -- Si on tombe sur noeud vide on continue de parcourir à droite et à gauche
            Pour_Chaque_arbre(Arbre.all.droite);
            Pour_Chaque_arbre(Arbre.all.gauche);
        else
            begin
                Traiter(Arbre.all.Destination, Arbre.all.Masque, Arbre.all.eth);
            exception
                when others => null;
            end;
        end if;
    end Pour_Chaque_arbre;


end arbre;