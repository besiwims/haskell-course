Functions
---

En Haskell, les fonctions fonctionnent exactement comme en mathématiques. En mathématiques, les fonctions définissent la dépendance sans ambiguïté de la valeur de sortie à ses arguments, c’est-à-dire qu’elles associent les valeurs d’entrée à une valeur de sortie. Cela signifie que pour toute combinaison d’arguments, il ne peut y avoir qu’un seul résultat. Regardons un exemple simple d’une fonction qui prend un argument et le multiplie par trois :

```haskell
f(x) = 3 * x
```

Pour toute valeur d’entrée `x`, il n’y a qu’une seule sortie possible :

```haskell
f(2) = 6  
f(−5) = −15
```

C’est exactement ce que l’on veut dire lorsqu’on dit que Haskell est un langage de programmation purement fonctionnel – parce qu’il se concentre sur les fonctions pures dont les valeurs de sortie sont entièrement déterminées par leurs arguments. Cela signifie qu’il n’y a pas d’effets de bord dans les fonctions Haskell : une fonction avec les mêmes arguments produira toujours le même résultat, peu importe ce qui se passe dans le reste du programme. Cela les rend très fiables.

En d’autres termes, les fonctions Haskell ne permettent rien d’autre que de prendre des entrées à partir de leurs arguments et de produire une valeur de retour. Ainsi, des actions comme afficher à l’écran, lire ou écrire dans des fichiers ne sont pas possibles dans une fonction Haskell classique. Cependant, cela voudrait dire que Haskell ne peut pas être très utile, et c’est pourquoi il est tout de même possible d’intégrer ces fonctionnalités importantes et utiles à l’aide des **Monades** (nous n’aborderons pas les monades tout de suite, mais pour l’instant, faites-moi confiance : Haskell peut avoir des applications très concrètes – après tout, **Cardano** est construit avec Haskell).

En Haskell, on peut définir une fonction en utilisant une équation qui spécifie :

* Le nom de la fonction
* Les noms de ses arguments
* Le corps de la fonction qui précise comment le résultat sera calculé

Pour définir notre fonction de triplement ci-dessus en Haskell, on écrirait :

```haskell
triple x = 3 * x
```

où `triple` est le nom de la fonction, `x` est son seul argument et `3 * x` est le corps de la fonction. Remarquez qu’en Haskell, il n’est pas nécessaire d’utiliser des parenthèses autour des arguments. Le nom de la fonction et le premier argument, ainsi que tout argument supplémentaire, sont simplement séparés par un espace.

Lorsque la fonction est appliquée à des arguments réels, le corps de la fonction reçoit les arguments et le résultat est calculé (avec des accolades représentant des commentaires dans le bloc ci-dessous) :

```haskell
triple 4
= { application de triple }
3 * 4
= { application de * } 
12
```
Très bien ! Voici une continuation de l’explication traduite en français, avec plus de détails sur les fonctions Haskell et leur style de définition :

---

### Fonctions avec plusieurs arguments

En Haskell, les fonctions peuvent également prendre plusieurs arguments. Par exemple, une fonction qui additionne deux nombres peut être définie comme suit :

```haskell
add x y = x + y
```

Ici :

* `add` est le nom de la fonction,
* `x` et `y` sont les deux arguments,
* `x + y` est le corps de la fonction.

Quand on applique la fonction :

```haskell
add 2 5
= { application de add }
2 + 5
= 7
```

### Évaluation paresseuse (Lazy Evaluation)

Haskell utilise une stratégie appelée **évaluation paresseuse**, ce qui signifie que les expressions ne sont évaluées que lorsqu’elles sont réellement nécessaires. Cela permet de définir des structures infinies, comme des listes sans fin, sans planter le programme tant qu'on ne tente pas d'en consommer une infinité.

Exemple :

```haskell
infList = [1..]  -- une liste infinie à partir de 1
take 5 infList   -- prend seulement les 5 premiers éléments
```

Résultat :

```haskell
[1,2,3,4,5]
```

Haskell ne calculera que ce dont il a besoin, ici les 5 premiers éléments seulement.

### Immutabilité

Une autre caractéristique essentielle de Haskell est l’**immutabilité**. Une fois qu’une variable est définie, sa valeur ne peut jamais être changée. Par exemple :

```haskell
x = 10
x = x + 1  -- Erreur ! Cela n'est pas permis en Haskell
```

On ne peut pas redéfinir `x` comme on le ferait dans des langages impératifs.

---



