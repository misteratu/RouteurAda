with cache;                      use cache;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;

package utilitaire is
    function Ieme_Octet (Adresse : T_Adresse_IP; i : Integer) return Integer;

    function binaire (destination : in T_Adresse_IP) return Unbounded_String;

    function Compter_1_Dans_Nombre_Binaire (masque : T_Adresse_IP) return Integer;

    function Longueur_Masque (Masque : T_Adresse_IP) return Integer;

    function Creer_Masque (Longueur : Integer) return T_Adresse_IP;

    function String_En_Adresse_IP (binaire : Unbounded_String) return T_Adresse_IP;
    
    function convertir_adresse_entier(ligne : in Unbounded_String) return T_Adresse_IP;

end utilitaire;