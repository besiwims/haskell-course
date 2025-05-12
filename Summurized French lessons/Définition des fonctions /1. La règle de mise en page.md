# La règle de mise en page

Avant de nous plonger dans l’utilisation des fonctions dans Haskell, explorons la règle de disposition. La règle de mise en page stipule que chaque définition au même niveau doit commencer à la même position de ligne (colonne) dans le script. Cela nous permet de déterminer les regroupements de différentes définitions simplement à partir de l’indentation. Définissons une fonction qui additionne les carrés de deux nombres :
```Haskell
sumSquares x y = a + b
  où
 a = x ^ 2 -- (^) est la fonction de puissance
    b = y ^ 2
    
ghci> sumCarrés 2 5
29
```
D’après l’indentation, il est évident pour Haskell que et sont des définitions locales dans la fonction , définies à l’aide du mot-clé. Les définitions locales existent en tant qu’expressions d’assistance intermédiaires pour structurer nos fonctions et rendre notre code plus lisible. Il est également possible d’envelopper les variables locales et de les mettre entre accolades pour indiquer explicitement le regroupement, auquel cas la disposition n’a pas d’importance (bien qu’il soit considéré comme une bonne pratique d’utiliser la règle de disposition pour donner à notre code une meilleure lisibilité), mais nous devons également séparer explicitement chaque définition locale par :absumSquareswhereab;

```haskell
sumSquares x y = a + b
  où
    {
 a = x ^ 2 ; -- nous devons séparer les expressions par ' ;' dans ce cas
      b = y ^ 2
    }

ghci> sumCarrés 2 5
29
```
