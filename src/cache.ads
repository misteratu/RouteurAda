with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

package cache is
    -- Implémentation d'une adresse IP suivant le schéma de Horner.
    type T_Adresse_IP is mod 2 ** 32;

    -- Route constituée d'une destination, d'un masque et d'une interface.
    type T_Donnee is
        record
            Destination : T_Adresse_IP;
            Masque : T_Adresse_IP;
            eth : Unbounded_String;
        end record;

    type T_LCA is limited private;


    -- Initialiser une Sda. La Sda est vide.
    procedure Initialiser(Sda: out T_LCA) with
            Post => Est_Vide (Sda);

    -- Est-ce qu'une Sda est vide ?
    function Est_Vide (Sda : T_LCA) return Boolean;

    -- Obtenir le nombre d'éléments d'une Sda.
    function Taille (Sda : in T_LCA) return Integer with
            Post => Taille'Result >= 0
            and (Taille'Result = 0) = Est_Vide (Sda);

    -- Enregistrer une Donnée associée à une Clé dans une Sda.
    -- Si la clé est déjà présente dans la Sda, sa donnée est changée.
    procedure Enregistrer (Sda : in out T_LCA ; Destination : in T_Adresse_IP ; Masque : in T_Adresse_IP ; eth : in Unbounded_String) with
            Post => Destination_Presente (Sda, Destination)  -- donnée insérée
            and (not (Destination_Presente (Sda, Destination)'Old) or Taille (Sda) = Taille (Sda)'Old)
            and (Destination_Presente (Sda, Destination)'Old or Taille (Sda) = Taille (Sda)'Old + 1);

    -- Supprimer la Donnée associée à une Clé dans une Sda.
    -- Exception : Cle_Absente_Exception si Clé n'est pas utilisée dans la Sda
    procedure Supprimer (Sda : in out T_LCA ; Destination : in T_Adresse_IP) with
            Post =>  Taille (Sda) = Taille (Sda)'Old - 1 -- un élément de moins
            and not Destination_Presente (Sda, Destination);         -- la clé a été supprimée

    -- Supprime la première case d'une LCA.
    procedure Supprimer_Case_1 (Liste : in out T_LCA);

    -- Supprimer tous les éléments d'une Sda.
    procedure Vider (Sda : in out T_LCA) with
            Post => Est_Vide (Sda);

    -- Appliquer un traitement (Traiter) pour chaque couple d'une Sda.
    generic
        with procedure Traiter (destination : T_Adresse_IP; masque : T_Adresse_IP; eth : Unbounded_String);
    procedure Pour_Chaque (Sda : in T_LCA);

    -- Vérifier si une destination apparaît dans la LCA donnée.
    function Destination_Presente (Sda : in T_LCA ; Destination : in T_Adresse_IP) return Boolean;

    -- Cherche la meilleure route correspondant à un paquet dans une table sous forme de LCA (table de routage ou cache).
    -- Paramètres:
    --              Table           :       Table de routage ou cache.
    --              Paquet          :       Paquet à faire correspondre.
    function Meilleure_Route_Paquet_LCA (Table : in T_LCA; Paquet : in T_Adresse_IP) return T_donnee;

    -- Étant donné une route d'une table, détermine la route correspondante à insérer dans le cache pour assurer sa cohérence.
    -- Paramètres:
    --              Table_routage   :       Table de routage.
    --              Route           :       Route de la table.
    --              Interface       :       Interface de la route.
    function Coherence (Table_routage : in T_LCA; Route : in T_Adresse_IP; Eth : in Unbounded_String) return T_Donnee;

    -- jsp c'est les arbres ça
    -- Paramètres:
    --              liste           : Liste des routes dans l'ordre du moins récent au plus récent
    --              route           : La route à supprimer et à enregistrer 
    procedure Mise_a_jour_LRU (liste : in out T_LCA ; route : in T_Donnee);


    -- Actualise les statistiques d'utilisation d'une route du cache.
    -- Paramètres:
    --              Cache           :   Cache.
    --              Destination     :   Destination de la route à actualiser.
    --              nb_iterations   :   Numéro de la ligne du fichier paquets qui donne lieu à l'actualisation.
    procedure Actualiser_Stats (Cache : in out T_LCA; Destination : T_Adresse_IP; nb_iterations : Integer);

    -- Trier le cache selon une politique FIFO.
    procedure Trier_FIFO (Cache : in out T_LCA);

    -- Trier le cache selon une politique LRU.
    procedure Trier_LRU (Cache : in out T_LCA);

    -- Trier le cache selon une politique LFU.
    procedure Trier_LFU (Cache : in out T_LCA);

    --
    function Trier_LRU_Arbre (Cache : in out T_LCA) return T_Adresse_IP;

    --
    function Dans_Cache (Cache : in T_LCA ; route : in T_Donnee) return Boolean;

private
    type T_Cellule;

    type T_LCA is access T_Cellule;

    type T_Cellule is
        record
            Destination : T_Adresse_IP;     -- Destination.
            Masque : T_Adresse_IP;          -- Masque.
            eth : Unbounded_String;         -- Interface.
            Derniere_Utilisation : Integer; -- Dernière utilisation de la route (pour LRU).
            Nb_Utilisations : Integer;      -- Nb d'utilisations de la route (pour LFU).
            Suivant : T_LCA;
        end record;
end Cache;