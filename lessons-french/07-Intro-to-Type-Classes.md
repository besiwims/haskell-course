Intro-to-Type-Classes

## Introduction aux Classes de Types

### Plan
- L'excellence des classes de types
- Qu'est-ce qu'une classe de types ?
- Classes de types courantes
  - Eq, Ord
  - Num, Integral, Floating
  - Read, Show
- Le type valide le plus général
- Contraintes multiples

Il s'agit d'une introduction au concept et à l'utilisation des classes de types du point de vue du consommateur. Autrement dit, en développant en Haskell, nous rencontrons des classes de types et nous voulons comprendre comment les utiliser.

Dans deux leçons, après avoir appris à créer des types, nous verrons comment les classes de types et les instances sont définies.

---

### L'excellence des classes de types

Jusqu'à présent, nous avons appris que lors de la définition d'une fonction, nous pouvons choisir de l'utiliser avec un type spécifique comme ceci :

```haskell
sqr :: Int -> Int
sqr v = v * v
```

Cela offre beaucoup de sécurité car, en s'assurant que votre fonction ne prend que des valeurs de type `Int`, vous pouvez être certain que les opérations mathématiques sont valides. Cependant, cela limite l'utilisation de la fonction à ce seul type. Si vous souhaitez une version pour `Double` ou `Float`, vous devez la redéfinir sous un autre nom comme `sqrDouble` ou `sqrFloat`.

Nous pouvons également définir une fonction de manière polymorphe :

```haskell
fst :: (a, b) -> a
fst (x, _) = x
```

Ce qui apporte une grande flexibilité car nous pouvons utiliser des valeurs de n'importe quel type en entrée, mais nous perdons toute la sécurité associée à l'utilisation de types précis.

Les classes de types permettent de combiner ces deux mondes :

- La flexibilité des types polymorphes
- La sécurité des types bien définis

Elles permettent d'utiliser des valeurs polymorphes avec des restrictions. Autrement dit, les valeurs peuvent appartenir à différents types, mais uniquement à un sous-ensemble autorisé. C'est ce qu'on appelle le **polymorphisme ad hoc** ou **surcharge**.

Maintenant que nous savons pourquoi les classes de types sont formidables, voyons en détail ce qu'elles sont !

---

### Qu'est-ce qu'une classe de types ?

Imaginons que vous rencontriez des personnes appartenant à un club de dessin avancé. Vous savez qu'elles savent dessiner, car c'est un prérequis pour être admis dans ce club !

Les classes de types fonctionnent de la même manière : ce sont des **groupes** auxquels les types peuvent appartenir s'ils respectent certains comportements (définis sous forme de fonctions).

Une classe de types définit un ensemble de fonctions, et chaque type qui en est une instance implémente ces fonctions.

Par exemple, considérons le type `Bool`. Pour voir les classes de types auxquelles `Bool` appartient, nous pouvons utiliser la commande `:i Bool` dans `ghci`.

```haskell
instance Eq Bool -- Définie dans ‘GHC.Classes’
instance Ord Bool -- Définie dans ‘GHC.Classes’
instance Enum Bool -- Définie dans ‘GHC.Enum’
instance Show Bool -- Définie dans ‘GHC.Show’
instance Read Bool -- Définie dans ‘GHC.Read’
instance Bounded Bool -- Définie dans ‘GHC.Enum’
```

Cela signifie que `Bool` implémente toutes ces classes de types et supporte les comportements définis par celles-ci.

Voyons maintenant les classes de types les plus courantes !

---

### Classes de types courantes

Nous allons passer en revue quelques classes de types essentielles et voir ce qu'elles représentent.

#### La classe de types `Eq`

La classe `Eq` concerne l'égalité. Un type qui est une instance de `Eq` peut comparer deux valeurs pour vérifier si elles sont égales (`==`) ou différentes (`/=`).

```haskell
True == False  -- False
True /= False  -- True
```

Les signatures des fonctions `==` et `/=` sont les suivantes :

```haskell
(==) :: Eq a => a -> a -> Bool
(/=) :: Eq a => a -> a -> Bool
```

Le symbole `=>` représente une **contrainte de classe**. Il indique qu'un type polymorphe doit être une instance d'une certaine classe de types.

---

#### La classe de types `Ord`

La classe `Ord` concerne l'ordre et permet de comparer les valeurs avec les opérateurs `<`, `>`, `<=`, `>=`.

```haskell
4 > 9      -- False
'a' >= 'b' -- False
```

Elle fournit également des fonctions comme `min`, `max` et `compare`.

```haskell
compare 4 9         -- LT (4 est plus petit que 9)
'f' `compare` 'e'   -- GT ('f' est plus grand que 'e')
```

Pour être une instance de `Ord`, un type doit d'abord être une instance de `Eq`.

---

#### La classe de types `Num`

La classe `Num` regroupe tous les types numériques et leur permet d'utiliser les opérations `+`, `-`, `*`, etc.

```haskell
(+) :: Num a => a -> a -> a
(-) :: Num a => a -> a -> a
(*) :: Num a => a -> a -> a
```

Exemple :

```haskell
5 - 1      -- 4
8.9 + 0.1  -- 9.0
```

---

#### Classes de types `Integral` et `Fractional`

La classe `Integral` inclut uniquement les nombres entiers (`Int`, `Integer`), tandis que `Fractional` concerne les nombres fractionnaires (`Float`, `Double`).

```haskell
div :: Integral a => a -> a -> a
(/) :: Fractional a => a -> a -> a
```

Exemples :

```haskell
3 `div` 5    -- 0
10 / 3       -- 3.3333333
```

---

#### La classe de types `Show`

`Show` permet de convertir une valeur en `String` grâce à `show`.

```haskell
show (3 :: Int) -- "3"
show True       -- "True"
```

---

#### La classe de types `Read`

`Read` est l'opposé de `Show`, elle convertit une `String` en une valeur du type spécifié.

```haskell
read "3" :: Int  -- 3
read "True" :: Bool  -- True
```

Si la conversion est impossible, une erreur est levée.

---

### Le type valide le plus général

Lorsqu'on écrit une fonction, Haskell infère le type le plus général possible en fonction des contraintes appliquées. Par exemple :

```haskell
fToC x = (x - 32) * 5 / 9
```

L'opérateur `/` exige un type `Fractional`, donc le compilateur déduit :

```haskell
fToC :: Fractional a => a -> a
```

---

### Contraintes multiples

Une fonction peut nécessiter plusieurs contraintes de classe de types. Par exemple :

```haskell
doubleIfEq :: (Eq a, Num a) => a -> a -> a
doubleIfEq x y = if x == y then x * 2 else x
```

Cela signifie que `x` et `y` doivent appartenir aux classes `Eq` et `Num`.

Ainsi, Haskell nous permet de combiner flexibilité et sécurité grâce aux classes de types !

