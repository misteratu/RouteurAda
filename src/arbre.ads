with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with cache;                     use cache;

package arbre is

    type T_Arbre is limited private;

-- Initialiser crée un arbre qui est null
procedure Initialiser(Arbre_binaire : out T_Arbre);

-- Créer des noeuds vides

-- Paramètres :
--              Arbre               : L'arbre à modifier 
--              indice_de_parcours  : Le numéro de profondeur de l'arbre 
--              destination_bit     : La destination en binaire de l'adresse à enregistrer 

procedure noeuds_vides(Arbre : in T_Arbre ; indice_de_parcours : in Integer ; destination_bit : in Unbounded_String);

-- Créer un noeud au niveau du bit de différence

-- Paramètres :
--              Arbre               : L'arbre à modifier 
--              destination_bit     : La destination en binaire de l'adresse à enregistrer 
--              destination_arbre   : destination de l'arbre à dedoubler 
--              indice_de_parcours  : Le numéro de profondeur de l'arbre 
--              stockage            : Stocke la route du noeud à decomposer en gauche et droite
--              masque              : Masque de la route à enregistrer 
--              eth                 : Interface de la route à enregistrer 

procedure noeud_bit_difference(Arbre : in out T_Arbre ; destination_bit :in  Unbounded_String ;destination_arbre : in Unbounded_String ; indice_de_parcours : in out Integer ; stockage : in out T_Donnee ; masque : in T_Adresse_IP ; eth : in Unbounded_String);

-- Créer deux branches gauche et droite après le nœud de différence

-- Paramètres :
--              Arbre               : L'arbre à modifier 
--              indice_de_parcours  : Le numéro de profondeur de l'arbre 
--              stockage            : Stocke la route du noeud à decomposer en gauche et droite
--              destination_bit     : La destination en binaire de l'adresse à enregistrer 
--              masque              : Masque de la route à enregistrer 
--              eth                 : Interface de la route à enregistrer 

procedure branche_g_et_d_noeud_difference (Arbre : in out T_Arbre; indice_de_parcours : in Integer; stockage : in out T_Donnee ; destination_bit : in Unbounded_String ; Masque : in T_Adresse_IP ; Eth : in Unbounded_String);

-- Enregistrer une route dans un arbre

-- Paramètres :
--              Arbre               : L'arbre à modifier 
--              adresse             : Adresse à enregistrer 
--              masque              : Masque de la route à enregistrer 
--              eth                 : Interface de la route à enregistrer 
--              indice_de_parcours  : Le numéro de profondeur de l'arbre 

procedure enregistrer_arbre (Arbre : in out T_Arbre ; adresse : in out T_Adresse_IP ; masque : in T_Adresse_IP ; Eth : in Unbounded_String ; Indice_de_parcours : in out Integer);

-- Enregistrer la route dans un arbre

-- Paramètres :
--              Arbre               : L'arbre à modifier 
--              adresse             : Adresse à enregistrer 
--              masque              : Masque de la route à enregistrer 
--              eth                 : Interface de la route à enregistrer 

procedure Enregistrer_Cache_Arbre (Arbre : in out T_Arbre ; adresse : in out T_Adresse_IP ; masque : in T_Adresse_IP ; Eth : in Unbounded_String);

-- Choisir la meilleure route

-- Paramètres :
--              Table_routage       : Représente la table de routage en liste chainée 
--              Cache               : Cache  
--              Paquet              : Paquet à router 
--              Nb_Defaut_Cache     : Nombre de fois que aucune route du cache correspond au paquet

function meilleure_route_paquet_routeur_arbre (Table_routage : in T_LCA ; Cache : in T_Arbre ; Paquet : in T_Adresse_IP ; Nb_Defaut_Cache : in out Integer) return T_Donnee;

-- Supprimer l’élément du cache

-- Paramètres :
--              Cache               : Cache à modifier 
--              adresse             : adresse à supprimer 
--              indice_pere         : indice du noeud au dessus de l'élément à supprimer 

procedure Supprimer_Arbre_element(Cache : in out T_Arbre ; Adresse : in T_Adresse_IP ; indice_pere : in out Integer);

-- Supprimer les noeuds vides au dessus de l’élément supprimé

-- Paramètres :
--              Cache               : Cache à modifier 
--              indice_pere         : indice du noeud au dessus de l'élément supprimé précedement 
--              adresse             : adresse supprimée

procedure Supprimer_noeuds_vides(Cache : in out T_Arbre ; indice_pere : in Integer ; adresse : in T_Adresse_IP);

-- Supprimer une destination dans un arbre

-- Paramètres :
--              Cache               : Cache à modifier 
--              adresse             : adresse à supprimer 

procedure Supprimer_Arbre(Cache : in out T_Arbre ; Adresse : in T_Adresse_IP);

-- Descendre l’arbre et supprimer le dernier Noeud Vide

-- Paramètres :
--              Cache               : Cache à modifier
--              indice_pere         : indice du noeud au dessus de l'élément supprimé précedement 
--              indice_progression  : indice de la profondeur dans l'arbre 
--              adresse             : adresse à supprimer 

procedure Descente_Arbre(Cache : in out T_Arbre ; indice_pere : in Integer ; indice_progression : in out Integer ;  Adresse : in T_Adresse_IP);

generic
    with procedure Traiter (destination : T_Adresse_IP; masque : T_Adresse_IP; eth : Unbounded_String);
procedure Pour_Chaque_arbre (Arbre : in T_Arbre);

private
    type T_Cellule;

    type T_Arbre is access T_Cellule;

    type T_Cellule is
        record
            Destination : T_Adresse_IP; -- Destination.
            Masque : T_Adresse_IP;      -- Masque.
            eth : Unbounded_String;     -- Interface.
            droite : T_Arbre;
            gauche : T_Arbre;
        end record;
end arbre;