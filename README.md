# RouteurAda

Ce programme est une implémentation d'un routeur utilisant différentes structures de données. Un routeur est un dispositif réseau qui transmet les paquets de données reçus sur une interface d'entrée vers la bonne interface de sortie en utilisant les informations stockées dans sa table de routage.

## Gestion des fichiers nécessaires

Pour faire fonctionner le routeur, nous avons besoin de deux fichiers principaux : un fichier contenant la table de routage et un autre fichier contenant les paquets à router.

### Fichier contenant la table de routage

Ce fichier doit être un fichier texte (.txt) et doit contenir la table de routage dans le format suivant :

```shell
table
212.212.212.212
cache 147.127.18.80
cache stat 147.127.18.85
147.127.19.1
fin
147.127.20.20
147.127.32.32
```
