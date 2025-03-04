02-Functions-Data-Types-and-Signatures

Voici la traduction en français :

---

# Types de données, Signatures et Polymorphisme

## Plan
- Introduction pragmatique aux types
- Signatures des fonctions
- Expérimentation avec les fonctions
- Variables en Haskell
- Fonctions en notation infixe et préfixe
- Types de données courants
- Valeurs polymorphiques et variables de type
- Amusement avec les listes !

---

## Introduction pragmatique aux types

### Le `::`
Un type est une étiquette que possède chaque expression et qui en restreint l'utilisation.

Nous utilisons une double colonne `::` pour afficher ou assigner le type d'une expression. Par exemple :

```haskell
monExpression :: MonType
```

Cela signifie que l'expression `monExpression` a le type `MonType`.

### Types fréquemment utilisés
Les types standard les plus couramment utilisés en Haskell sont :

- **Int** et **Integer** pour les nombres entiers.
- **Float** et **Double** pour les nombres réels à virgule flottante.
- **Bool** pour les valeurs `True` et `False`.
- **Char** pour les caractères individuels.
- **String** pour les chaînes de texte.

### Comment vérifier un type ?
Dans **GHCI**, la commande `:type` (ou `:t` en abrégé) suivie d'une expression valide nous indique son type.

Exemples :

```haskell
:type True
:type False
:t (3 < 5)
:t 'A'
:t "Hello world!"
```

---

## Signature d’une fonction

Le symbole `::` se lit **"est de type"** et indique une signature de type. Prenons l'exemple d'une fonction qui élève un nombre au carré :

```haskell
square :: Int -> Int
square v = v * v
```

- **La première ligne** est la **signature** : elle annonce au monde entier que cette fonction existe et avec quels types elle fonctionne.
- **La deuxième ligne** est la **définition** : elle décrit exactement ce que fait la fonction.

### Comprendre une signature de fonction
Dans :

```haskell
square :: Int -> Int
```

- À **gauche du `::`**, on trouve le **nom de la fonction**.
- À **droite du `::`**, on trouve son **type**.

Chaque fonction prend un certain nombre de paramètres de certains types et retourne une valeur d'un certain type.

Exemple avec plusieurs arguments :

```haskell
prod :: Int -> Int -> Int
prod x y = x * y
```

Ici :
- **Deux arguments** de type `Int`.
- **Un résultat** de type `Int`.

Les paramètres et le retour sont séparés par des flèches `->`.

---

## Jouer avec les fonctions

### Variables en Haskell (Noms/définitions)
Prenons cet exemple :

```haskell
name = "Bob"
```

Si une fonction n'a pas de paramètre, elle **retourne toujours la même valeur**.

```haskell
name :: String
name = "Bob"
```

C'est ce qu'on appelle une **définition**. En Haskell, une variable est **immuable** : une fois qu'une valeur est assignée, elle ne peut plus être changée !

```haskell
x = 3
-- 7 + x renverra toujours 10
```

Même pour les paramètres de fonction :

```haskell
volumeOfACylinder r h = pi * r^2 * h
```

Une fois les valeurs passées, elles ne peuvent pas être modifiées à l’intérieur de la fonction.

---

## Notations infixe et préfixe

### Notation préfixe
```haskell
prod x y = x * y
prod 4 5  -- 20
```
Ici, `prod` est **avant** ses arguments → notation **préfixe**.

### Notation infixe
```haskell
1 + 2
```
`+` est une **fonction infixe** car elle est **entre** ses arguments.

Les **opérateurs** sont des fonctions en notation infixe.

### Conversion infixe ↔ préfixe
Utilisez des **parenthèses** autour d'une fonction infixe pour l'utiliser en mode préfixe :

```haskell
(+) 1 2
```

Utilisez des **backticks `** pour convertir une fonction préfixe en infixe :

```haskell
4 `prod` 5
```

---

## Types de données courants

### Nombres entiers : `Int` et `Integer`
- `Integer` : précision arbitraire (jamais de dépassement).
- `Int` : plus rapide, mais limité en taille (64 bits sur CPU 64 bits).

```haskell
2^62 :: Int  -- OK
2^64 :: Int  -- Erreur !
2^127 :: Integer  -- OK
```

### Nombres réels : `Float` et `Double`
- **Float** : précision simple (32 bits).
- **Double** : précision double (64 bits).

```haskell
3.14159265358979323846 :: Float  -- Arrondi
3.14159265358979323846 :: Double -- Plus précis
```

⚠ **Toujours préférer `Double`**, plus précis et sans inconvénient majeur.

### Booléens : `Bool`
Seuls deux valeurs possibles : `True` et `False`.

```haskell
5 /= 0  -- True
3 >= 0  -- True
7.2 < 6.1  -- False
```

Opérateurs logiques :

```haskell
True && False  -- False
True || False  -- True
```

### Caractères : `Char`
Un seul caractère Unicode, entre **apostrophes simples** :

```haskell
'a'
'@'
'7'
```

### Chaînes : `String`
Une **liste de `Char`**, entourée de **guillemets doubles** :

```haskell
"Hellooooooo!"
```

Ces deux expressions sont équivalentes :

```haskell
['H','i','!'] == "Hi!"
```

---

## Polymorphisme et variables de type

Les types nous protègent contre les erreurs. Mais, si une fonction ne dépend pas d'un type précis, nous pouvons utiliser un **type polymorphe**.

Exemple :

```haskell
first :: (a, b) -> a
first (x, y) = x
```

Ici, `a` et `b` peuvent être **n'importe quel type**.

Haskell a déjà une fonction **équivalente** :

```haskell
:t fst
:t snd
```

Autre exemple de polymorphisme :

```haskell
:t head  -- Fonction qui retourne le premier élément d'une liste
:t tail  -- Fonction qui retourne la liste sans le premier élément
```

---

## Amusement avec les listes !

### Accéder à un élément
On utilise `!!` pour récupérer un élément :

```haskell
"abc" !! 1  -- 'b'
[12,13,16,18] !! 3  -- 18
```

### Ranges (Listes définies par plages)
```haskell
[3..22]  -- 3 à 22
[3,5..22]  -- Pas de 2
['a','c'..'z']  -- Une lettre sur deux
```

### Listes infinies
```haskell
[1..]  -- Liste infinie à partir de 1
take 5 [1..]  -- Prend les 5 premiers éléments
```

### Concaténation et préfixe
```haskell
2 : [3,4,5]  -- Ajout au début
[1,3,7] ++ [3,3,1]  -- Fusionner deux listes
```

⚠ **Concaténer avec `++` peut être lent !** Ajoutez plutôt au début avec `:`.

### Fonctions utiles sur les listes :
```haskell
length [2,4,5,6,7]  -- 5
null []  -- True (liste vide)
sum [-1,0,1,6,-5,-1]  -- 0
5 `elem` [6,3,5,7,5]  -- True
```

### Manipulation de texte
```haskell
words "To be or not to be?"
lines "Comment ça va?\nBien, et toi?"
```

---

## Conclusion
Nous avons vu :
✔ Types de base (`Int`, `Bool`, `String`, …)  
✔ Signatures de fonction  
✔ Variables immuables  
✔ Notations infixe et préfixe  
✔ Polymorphisme  
✔ Listes et manipulations  

💡 Haskell est puissant et flexible grâce à son **système de types avancé** ! 🚀
