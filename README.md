# RouteurAda

Ce programme est une implémentation d'un routeur utilisant différentes structures de données. Un routeur est un dispositif réseau qui transmet les paquets de données reçus sur une interface d'entrée vers la bonne interface de sortie en utilisant les informations stockées dans sa table de routage.

## Gestion des fichiers nécessaires

Pour faire fonctionner le routeur, nous avons besoin de deux fichiers principaux : un fichier contenant la table de routage et un autre fichier contenant les paquets à router.

### Fichier contenant la table de routage

Ce fichier doit être un fichier texte (.txt) et doit contenir la table de routage dans le format suivant :

```shell
147.127.0.0 255.255.0.0 eth1
147.127.18.0 255.255.255.0 eth0
212.19.8.128 255.255.255.128 eth2
```


Chaque ligne doit être composée d'une adresse IP de destination (une adresse IP est représentée par 4 entiers de 0 à 255 séparés par des points), d'une adresse IP pour le masque et de l'interface de sortie à utiliser. Chacun de ces éléments doit être séparé par au moins un espace.

### Fichier contenant les paquets

Ce fichier doit également être un fichier texte et doit être écrit de la manière suivante :

```shell
212.212.212.212
147.127.18.80
147.127.18.85
147.127.19.1
147.127.20.20
147.127.32.32
```

Chaque ligne contient une adresse IP qui sera ensuite routée par le programme.

Des mots-clés sont utilisables dans ce fichier pour effectuer différentes actions lors de l'exécution du programme. Ces mots-clés sont :

- `table` : affiche la table de routage
- `cache` : affiche le cache
- `stat` : affiche le nombre de demandes de routes, le nombre de défauts de cache et le taux de défaut de cache
- `fin` : arrête le traitement des paquets

Chaque mot-clé doit être écrit sur une ligne distincte, par exemple :

```shell
table
212.212.212.212
cache
147.127.18.80
cache
stat
147.127.18.85
147.127.19.1
fin
147.127.20.20
147.127.32.32
```

## Exploitation du programme

Les fichiers principaux sont les fichiers `routeur_ll.adb` (routeur fonctionnant avec des listes chaînées) et `routeur_la.adb` (routeur fonctionnant avec des arbres binaires).

Afin d'exécuter le programme, il est nécessaire de compiler le programme que vous souhaitez utiliser. Dans le terminal, utilisez la commande suivante :

```shell
gnatmake -gnatwa routeur_ll.adb
```
ou
 ```shell
gnatmake -gnatwa routeur_la.adb
```

Ensuite, exécutez le programme en écrivant :

```shell
./routeur_ll [arguments]
```
ou
 ```shell
./routeur_la [arguments]
```

Deux fichiers sont fournis avec les programmes : `paquets.txt` et `table.txt`. Sans spécifier d'options, ces deux fichiers seront utilisés.

Les arguments peuvent apparaître dans n'importe quel ordre, et la dernière occurrence d'un argument spécifie sa valeur. Les arguments disponibles sont les suivants :

- `-c <taille>` : Définit la taille du cache. `<taille>` est la taille du cache. La valeur 0 indique qu'il n'y a pas de cache. La valeur par défaut est 10.
- `-P FIFO|LRU|LFU` : Définit la politique utilisée pour le cache (par défaut FIFO, et uniquement LRU est disponible pour les arbres).
- `-s` : Affiche les statistiques (nombre de défauts de cache, nombre de demandes de route, taux de défaut de cache). C'est l'option activée par défaut.
- `-S` : Ne pas afficher les statistiques.
- `-t <fichier>` : Définit le nom du fichier contenant les routes de la table de routage. Par défaut, le fichier `table.txt` est utilisé.
- `-p <fichier>` : Définit le nom du fichier contenant les paquets à router. Par défaut, le fichier `paquets.txt` est utilisé.
- `-r <fichier>` : Définit le nom du fichier contenant les résultats (adresse IP destination du paquet et interface utilisée). Par défaut, le fichier `resultats.txt` est utilisé.

Par exemple, la commande suivante :

```shell
./routeur_ll -s -c 10 -P FIFO -S -p LRU -c 50
```

lancera le programme utilisant les listes chaînées, sans afficher les statistiques, avec un cache de taille 50 et une politique LRU.

## Conclusion

Ce programme de routeur vous permettra de router des paquets en utilisant une table de routage et différentes structures de données. Expérimentez avec les options et les fichiers fournis pour personnaliser votre expérience de routage.




