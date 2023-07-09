package body utilitaire is
    -- ' ieme_octet '
    -- Donner la valeur du i ème bit d'une adresse IP.
    function Ieme_Octet (Adresse : T_Adresse_IP; i : Integer) return Integer is
        a : T_Adresse_IP;   -- i ème octet de l'adresse IP.
    begin
        case i is -- Schéma de Horner
			when 1 => a := Adresse / 256**3;
			when 2 => a := (Adresse mod 256**3) / 256**2;
			when 3 => a := ((Adresse mod 256**3) mod 256**2) / 256;
			when 4 => a := ((Adresse mod 256**3) mod 256**2) mod 256;
			when others => a := 0;
        end case;
        return Integer (a);
    end Ieme_Octet;

    -- ' binaire '

    function binaire (destination : in T_Adresse_IP) return Unbounded_String is
    Chaine_Binaire : String(1..destination'Size);
    destination_bis : T_Adresse_IP;
    begin
    -- Initialiser la chaîne de caractères résultat à des zéros
    for k in Chaine_Binaire'Range loop
        Chaine_Binaire(k) := '0';
    end loop;
    
    -- Convertir l'entier en binaire en parcourant chaque bit de poids fort à faible
    destination_bis := destination;
    for Numero_Bit in reverse 1..length (To_unbounded_string(Chaine_Binaire)) loop 
        if destination_bis mod 2 = 1 then
            Chaine_Binaire(Numero_Bit) := '1';
        end if;
        destination_bis := destination_bis / 2;
    end loop;
    return To_Unbounded_String(Chaine_Binaire);
    end binaire;

    -- Donne le nombre de 1 dans un chiffre binaire.
    -- ' Compter_1_dans_nombre_binaire '
    function Compter_1_Dans_Nombre_Binaire (masque : T_Adresse_IP) return Integer is
        Compteur : Integer := 0;    -- Compteur de 1.
    begin
        for i in 0..31 loop -- Parcourt les 32 bits.
            if (masque and 2**i) /= 0 then
                Compteur := Compteur + 1;
            else
                null;
            end if;
        end loop;
        return Compteur;
    end Compter_1_Dans_Nombre_Binaire;

    function Longueur_Masque (Masque : T_Adresse_IP) return Integer is
        Compteur : Integer := 0;
        i : Integer := 0;
    begin
        while i <= 31 and (Masque and 2**(31-i)) /= 0 loop
            Compteur := Compteur + 1;
            i := i + 1;
        end loop;
        return Compteur;
    end Longueur_Masque;

    function Creer_Masque (Longueur : Integer) return T_Adresse_IP is
        Binaire : Unbounded_String;
    begin
        for i in 1..Longueur loop
            Binaire := Binaire & "1";
        end loop;
        for i in Longueur..31 loop
            Binaire := Binaire & "0";
        end loop;

        return String_En_Adresse_IP (Binaire);
    end Creer_Masque;

    -- ' binaire_a_entier '
    function String_En_Adresse_IP (binaire : Unbounded_String) return T_Adresse_IP is
        resultat : T_Adresse_IP := 0;
        caractere : Character;
        chaine : Unbounded_String;
    begin
        for i in 1..length(binaire) loop
            caractere := To_string(binaire)(i);
            chaine := To_Unbounded_String(Integer'Image(0) & caractere);
            resultat := resultat * 2 + (T_Adresse_IP'Value(To_String(chaine)));
        end loop;
        return resultat;
    end String_En_Adresse_IP;

    -- Cette fonction est conçue pour convertir une adresse IP de sa représentation sous forme de chaîne de caractères en une valeur entière.
-- Cette fonction permet d'enlever les points, les espaces et de séparer les 3 données qui sont la destination, le masque et l'interface.
-- Pour cela on parcours la ligne et dès que l'on trouve un point on récupère l'entier entre ce point et le point précédent.
    function convertir_adresse_entier(ligne : in Unbounded_String) return T_Adresse_IP is
        indice_point : Integer;         -- Indice d'un point ou d'un espace.
        Nb_point_parcourus : Integer;   -- Nombre de points parcourus.
        Paquet : T_Adresse_IP;          -- Un paquet suivant de schéma de Horner.
    begin
        indice_point := 0;
        Paquet := 0;
        Nb_point_parcourus := 0;  
        for i in 1..Length(ligne) loop 
            if To_String(ligne)(i) = '.' then -- Vérifie si le caractère de ligne est un point.
                paquet := paquet  + T_adresse_IP'Value(To_String(ligne)((indice_point + 1)..i-1)) * 256 ** (3 - Nb_point_parcourus); -- On applique le schéma de Horner pour convertir l'adresse en un entier.
                indice_point := i;
                Nb_point_parcourus := Nb_point_parcourus + 1; 
            elsif i = length(ligne) then -- Pour le dernier octet on part de la fin car après le dernier chiffre il n'y a pas de points ou pas d'espace.
                paquet := paquet  + T_adresse_IP'Value(To_String(ligne)((indice_point + 1)..i)) * 256 ** (3 - Nb_point_parcourus);
            else 
                null;
            end if;
        end loop;
        return Paquet;
    end convertir_adresse_entier;
end utilitaire;