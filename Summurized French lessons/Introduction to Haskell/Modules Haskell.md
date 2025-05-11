## Modules Haskell  
Le code Haskell est organisé en **modules**, qui sont des fichiers contenant le code source Haskell. Chaque module correspond à un seul fichier, et l'extension standard des modules Haskell est **.hs**. Chaque module doit commencer par le nom du module, qui est également le même que celui du fichier correspondant, par exemple **module Triple.hs** :

```haskell
module Triple -- nom du module
(
    triple -- interface du module (ce qui est explicitement exporté)
) where

{- Contenu du module -}
triple x = 3 * x -- déclaration de fonction
```

Tout d'abord, nous avons le nom du module **Triple**, suivi de l'interface du module encadrée par des parenthèses. L'interface du module précise **ce qui est exporté** depuis ce module, c'est-à-dire que si ce module est importé ailleurs, seule la **fonction triple** sera utilisable depuis ce module. Si une autre fonction était déclarée dans le contenu du module (par exemple **quadruple**), cette fonction ne serait pas exportée sauf si elle était spécifiée dans l'interface du module existante. Toutefois, **l'interface du module est facultative**, et si elle est omise, toutes les déclarations du module seront exportées.

Les **commentaires** servent à documenter notre code et tout ce qui est placé dans les commentaires ne sera pas évalué dans le programme. Les commentaires en Haskell peuvent être **sur une seule ligne ou multi-lignes** :
- Les **commentaires sur une seule ligne** commencent par **--**.
- Les **commentaires multi-lignes** sont encadrés par **{- -}**.

Tous les commentaires de ce guide suivront également ce même format.

---

