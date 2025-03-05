Creating-non-parameterized.md

**Création de types non paramétrés**  

Nous avons déjà abordé ce que sont les types et pourquoi ils sont utiles dans les leçons précédentes. Dans celle-ci, nous allons apprendre à créer nos propres types.  

### Plan  
- Synonymes de types  
  - Comment les définir  
  - Pourquoi les utiliser  
- Nouveaux types avec `data`  
  - Création de types  
  - Utilisation des types  
- Paramètres de valeur  
- Syntaxe des enregistrements  

---

## **Synonymes de types**  

Lorsque vous avez appris les chaînes de caractères (`String`) en Haskell, vous avez découvert que `String` est un sucre syntaxique pour `[Char]`. Cela signifie que `String` et `[Char]` sont équivalents et peuvent être utilisés de manière interchangeable.  

C'est parce que `String` est un **synonyme de type** pour `[Char]`.  

### **Comment définir des synonymes de types**  
Pour définir un synonyme de type, on utilise le mot-clé `type`, suivi du nouveau nom du type et du type existant auquel il est équivalent :  

```haskell
type String = [Char]
```  
Vous pouvez nommer le synonyme comme vous le souhaitez, tant qu'il commence par une majuscule.  

Lorsque vous définissez un synonyme de type, vous ne créez **pas** un nouveau type ! Vous dites simplement à Haskell qu'un type existant peut être référencé avec un nom différent (un synonyme).  

### **Pourquoi utiliser des synonymes de types ?**  
Pourquoi ajouter de la complexité sans ajouter de nouvelles fonctionnalités ?  

Parce que les synonymes de types permettent de transmettre **plus d’informations** ! Voyons un exemple.  

Imaginez que vous commencez à travailler avec une bibliothèque permettant de créer des transactions monétaires.  

Vous voulez créer une nouvelle transaction et vous regardez la signature de la fonction que vous devez utiliser :  

```haskell
generateTx :: String -> String -> Int -> String
```  
Pas très explicite... Vous pouvez deviner que `Int` est la valeur à transférer, mais que représentent ces `String` ? Et que contient la `String` renvoyée ?  

Comparez maintenant avec cette signature :  

```haskell
generateTx :: Address -> Address -> Value -> Id
```  
Cette deuxième version est **beaucoup plus claire** ! Les deux premiers paramètres sont des adresses, le troisième est la valeur de la transaction, et la fonction semble retourner l’identifiant de la transaction.  

Tout cela rien qu'en lisant la signature du type !  

Voyons ce que nous avons fait pour améliorer la compréhension :  

```haskell
generateTx :: String -> String -> Int -> String 
generateTx from to value = from ++ to ++ show value
```  
Ajoutons maintenant des synonymes de types :  

```haskell
type Address = String
type Value = Int
type Id = String

generateTx :: Address -> Address -> Value -> Id
generateTx from to value = from ++ to ++ show value
```  
Super facile ! Et si vous voulez vérifier la définition d’un type, vous pouvez ouvrir GHCi, charger le fichier et utiliser :  

```haskell
:i Address
```  
On peut aussi combiner des synonymes pour créer des types plus complexes :  

```haskell
type Name = String
type Address = (String, Int)
type Person = (Name, Address)

bob = ("Bob Smith", ("Main St.", 555)) :: Person
:t bob
:t fst bob
```  
Cela permet de rendre le code plus clair sans modifier le comportement. Mais si nous avons besoin de **créer un tout nouveau type**, nous devons utiliser `data` !  

---

## **Définir de nouveaux types avec `data`**  

Nous pouvons créer des nouveaux types ainsi :  

```haskell
data PaymentMethod = Cash | Card | Cryptocurrency

data Color = Red | Green | Blue

data Bool = True | False      -- Définition réelle de Bool

data Ordering = LT | EQ | GT  -- Définition réelle de Ordering
```  
Nous utilisons le mot-clé `data`.  

- La partie avant le `=` est le **nom du type**.  
- La partie après le `=` sont les **constructeurs de valeur**.  

Le **symbole `|`** signifie **"ou"**. Ainsi, nous pouvons lire :  

> Le type `PaymentMethod` peut prendre la valeur `Cash`, `Card` ou `Cryptocurrency`.  

🔴 **Attention** : Le nom du type et ses constructeurs **doivent commencer par une majuscule** !  

### **Utiliser notre nouveau type**  

Nous pouvons maintenant utiliser ce type dans un autre type :  

```haskell
type Name = String
type Address = (String, Int)

data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show)

type Person = (Name, Address, PaymentMethod)

bob = ("Bob Smith", ("Main St.", 555), Cash) :: Person
bob
```  
Cela affiche :  

```haskell
("Bob Smith",("Main St.",555),Cash)
```  
Le `deriving (Show)` permet d'afficher les valeurs dans le terminal.  

On peut aussi utiliser **le pattern matching** :  

```haskell
howItPays :: Person -> String
howItPays (_, _, Cash) = "Paie en liquide"
howItPays (_, _, Card) = "Paie par carte"
howItPays (_, _, Cryptocurrency) = "Paie en cryptomonnaie"

howItPays bob
```  
Cela retourne :  

```haskell
"Paie en liquide"
```  
On peut donc utiliser nos nouveaux types **comme n’importe quel autre type** !  

---

## **Paramètres de valeur**  

Si nous avons besoin d’un type plus **complexe**, comme une forme géométrique, nous pouvons utiliser **des paramètres** dans les constructeurs.  

Par exemple, imaginons que nous voulons représenter **un cercle et un rectangle**.  

Un cercle a un **rayon**. Un rectangle a une **longueur et une largeur**.  

Nous pouvons définir cela ainsi :  

```haskell
data Shape = Circle Float | Rectangle Float Float
```  
Ici, `Circle` prend **un paramètre** (le rayon) et `Rectangle` prend **deux paramètres** (les longueurs des côtés).  

Et voici quelque chose d’intéressant :  

```haskell
:t Circle
Circle :: Float -> Shape
```  
✅ **`Circle` est une fonction** qui prend un `Float` et retourne un `Shape` !  

Nous pouvons créer des valeurs de notre type comme ceci :  

```haskell
smallCircle = Circle 3
hugeCircle = Circle 100

:t smallCircle
smallCircle :: Shape
```  
Idem pour un rectangle :  

```haskell
rect1 = Rectangle 10 5
rect2 = Rectangle 256 128

:t rect1
rect1 :: Shape
```  
Maintenant, **utilisons ces valeurs** !  

Nous pouvons écrire une fonction qui calcule l’aire d’une `Shape` :  

```haskell
area :: Shape -> Float
area (Circle r) = pi * r^2        -- Pattern matching sur les constructeurs de valeur
area (Rectangle l1 l2) = l1 * l2
```  
Testons :  

```haskell
area smallCircle
area rect2
```  
Résultats :  

```haskell
28.274334
32768.0
```  
🚀 **Nous avons créé un type vraiment utile !**

Voici la traduction en français :  

---

Mais je n’en ai pas encore fini avec ces formes. J’en veux plus ! Je veux ajouter des couleurs ! Et des points dans l’espace 2D qui indiquent la position du centre de la forme !

Pour cela, nous pourrions faire quelque chose comme cette monstruosité :

```haskell
data Shape
  = Circle (Float, Float) Float String
  | Rectangle (Float, Float) Float Float String
```
Où nous ajoutons les points dans l’espace sous forme de tuples de valeurs `Float`, et les couleurs sous forme de `String`.

Nous pourrions facilement redéfinir la fonction `area` pour ce nouveau type comme ceci :

```haskell
area :: Shape -> Float
area (Circle _ r _) = pi * r^2
area (Rectangle _ l1 l2 _) = l1 * l2
```
Mais ensuite, si nous voulons extraire des champs spécifiques du type `Shape`, nous devons créer une fonction personnalisée pour chacun d’eux :

```haskell
color :: Shape -> String
color (Circle _ _ c) = c
color (Rectangle _ _ _ c) = c

point :: Shape -> (Float, Float)
point (Circle p _ _) = p
point (Rectangle p _ _ _) = p
```
--- Etc...

Nous pouvons améliorer cela en définissant des synonymes de types :

```haskell
type Point = (Float,Float)
type Radius = Float
type Width = Float
type Height = Float
type Color = String

data Shape
    = Circle Point Radius Color
    | Rectangle Point Width Height Color
```
Le type est maintenant beaucoup plus lisible, c’est vrai.

Mais cela fait beaucoup de synonymes de types juste pour améliorer la compréhension de la signature. Et en plus, cela ne résout pas les autres problèmes, plus urgents !

Mais ne vous inquiétez pas, Haskell a une solution pour nous ! Voici la *syntaxe des enregistrements* !

---

### Syntaxe des enregistrements (*Record Syntax*)

La *syntaxe des enregistrements* est une façon alternative de définir des types de données, qui apporte plusieurs avantages.

Nous allons commencer avec un exemple plus simple, puis nous corrigerons notre type `Shape`.

Supposons que nous voulions créer un type `Employee` contenant le nom de l’employé et son nombre d’années d’expérience.

Sans la syntaxe des enregistrements, nous l’écririons comme ceci :

```haskell
data Employee = Employee String Float
```
Dans ce cas, comme le type n’a qu’un seul constructeur, il est courant d’utiliser le même nom que celui du type. Ce n’est pas obligatoire, c’est juste une convention.

Mais avec la syntaxe des enregistrements, nous pouvons faire ceci :

```haskell
data Employee = Employee { name :: String, experienceInYears :: Float } deriving (Show)
```
Comme vous pouvez le voir :

- Les constructeurs de valeur en syntaxe d’enregistrement ont leurs paramètres (*champs*) entourés d’accolades `{}`.
- Chaque champ a un nom commençant par une minuscule suivi de son type.
- Les champs sont séparés par des virgules.

Nous pouvons maintenant créer des valeurs comme ceci :

```haskell
richard = Employee { name = "Richard", experienceInYears = 7.5 }
```
Haskell nous permet également de récupérer les champs directement :

```haskell
name richard      -- "Richard"
experienceInYears richard  -- 7.5
```
Un autre avantage est la mise à jour facile des champs :

```haskell
newMatt = matt { experienceInYears = 6 }
```
---

### Appliquer cela à `Shape`

Avant la syntaxe des enregistrements :

```haskell
data Shape
  = Circle (Float, Float) Float String
  | Rectangle (Float, Float) Float Float String
```
Avec la syntaxe des enregistrements :

```haskell
data Shape
  = Circle
      { position :: (Float, Float)
      , radius   :: Float
      , color    :: String
      }
  | Rectangle
      { position :: (Float, Float)
      , width    :: Float
      , height   :: Float
      , color    :: String
      }
  deriving (Show)
```
On peut maintenant créer et manipuler des valeurs plus facilement :

```haskell
circ = Circle { position = (1, 2), radius = 6, color = "Green" }
rect1 = Rectangle (9, 3) 7 3 "Yellow"
rect2 = rect1 {width = 12}
```
Et extraire des champs sans effort :

```haskell
position circ   -- (1.0,2.0)
color rect2     -- "Yellow"
```
---

### Un dernier avantage !

Nous pouvons toujours utiliser le *pattern matching* habituel :

```haskell
area :: Shape -> Float
area (Circle _ r _) = pi * r^2
area (Rectangle _ w h _) = w * h
```
Mais avec la syntaxe des enregistrements, nous pouvons simplifier encore plus :

```haskell
area :: Shape -> Float
area Circle {radius=r} = pi * r^2
area Rectangle {width=w, height=h} = w * h
```
Cela nous permet d’ignorer les champs non pertinents sans avoir à modifier les fonctions si nous ajoutons de nouveaux champs à `Shape` !

---

### Conclusion

La syntaxe des enregistrements est particulièrement utile pour les types ayant de nombreux champs, comme des configurations d’application ou des formulaires complexes.

Elle rend le code plus lisible, réduit les erreurs et facilite l’évolution du code.

---

C’est tout pour aujourd’hui ! Dans la prochaine leçon, nous irons encore plus loin avec des types plus complexes. Assurez-vous de bien assimiler cette leçon, et à bientôt ! 🚀
