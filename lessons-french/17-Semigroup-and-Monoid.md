Semigroup-and-Monoid

**Abstractions, Semigroupe et Monoid**  

Ceci est la première leçon de la section **"Abstraire des modèles"** du cours. Dans cette leçon, nous allons aborder :  

### Plan  
- Que signifie abstraire un modèle ?  
- Pourquoi abstraire des modèles (en général) ?  
- Avant-goût : Pourquoi abstraire **Semigroupe** et **Monoid** ?  
- La classe de types **Semigroup**  
- La classe de types **Monoid**  
- Que peut-on faire avec **Semigroup** et **Monoid** ?  

---

### Que signifie abstraire un modèle ?  
Nous, humains, sommes très doués pour détecter des modèles. Par exemple, dans la 6ᵉ leçon de ce cours, nous avons écrit ces fonctions :  

```haskell
sum'     :: [Int]  -> Int
sum'     []     = 0
sum'     (x:xs) = x + sum' xs

product' :: [Int]  -> Int
product' []     = 1
product' (x:xs) = x * product' xs

and'     :: [Bool] -> Bool
and'     []     = True
and'     (x:xs) = x && and' xs
```
Nous avons remarqué qu'il y avait un modèle répétitif dans ces fonctions. Nous avons donc créé une fonction unique qui contient ce modèle et prend ce qui varie en tant qu'arguments :  

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v []     = v
foldr f v (x:xs) = f x (foldr f v xs)
```
Nous avons alors remplacé les implémentations originales par cette abstraction :  

```haskell
sum'     :: [Int]  -> Int
sum'     = foldr (+) 0 

product' :: [Int]  -> Int
product' = foldr (*) 1 

and'     :: [Bool] -> Bool
and'     = foldr (&&) True
```
Cette série d’étapes illustre bien le processus d’abstraction :  
1. Écrire du code.  
2. Identifier un modèle.  
3. Créer une structure* pour contenir ce modèle (si utile).  
4. Utiliser cette structure au lieu d'écrire explicitement le modèle.  

> *Par "structure", on entend les types, les fonctions et les classes de types. D'autres langages de programmation utilisent d'autres mécanismes (comme les classes en POO).  

Abstraire un modèle signifie donc extraire une idée récurrente et la généraliser pour la réutiliser.  

**Note importante :** Il n'est pas toujours pertinent d’abstraire tous les modèles. L’expérience vous aidera à distinguer les bons modèles à généraliser.  

---

### Pourquoi abstraire des modèles (en général) ?  
Une question légitime serait : **"Pourquoi devrais-je abstraire des modèles ?"** Il y a plusieurs raisons, mais les plus importantes sont :  

1. **Réutiliser facilement du code**  
   Comme nous l'avons vu avec `foldr`, nous avons implémenté le modèle récursif une seule fois et nous pouvons maintenant l'utiliser partout.  

2. **Cacher les détails non importants**  
   L'implémentation de la récursion est cachée dans `foldr`, rendant le code plus lisible et plus clair.  

3. **Avoir un code clair et concis**  
   En utilisant `foldr`, le lecteur comprend immédiatement que nous sommes en train de **réduire une liste**.  

Ces raisons s'appliquent à toutes les abstractions bien conçues. Mais pourquoi s'intéresser spécifiquement à **Semigroup** et **Monoid** ?  

---

### Avant-goût : Pourquoi abstraire **Semigroup** et **Monoid** ?  
Un problème réel qui devient beaucoup plus simple grâce à ces abstractions est :  

➡️ **La montée en échelle**  

Plus précisément :  
- **L’optimisation des calculs**  
- **La gestion de la complexité des résultats sans complexifier le code**  

À la fin de cette leçon, nous verrons comment **Semigroup** et **Monoid** nous aident à mieux gérer ces défis.  

---

## La classe de types **Semigroup**  

Dans notre premier exemple, nous avons abstrait un modèle en une fonction (`foldr`). Ici, nous allons abstraire un concept sous forme de **classe de types**.  

Si nous regardons la classe de types `Num` en Haskell :  

```haskell
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```
`Num` est une **abstraction des comportements d’un nombre** : addition, soustraction, multiplication, etc.  

Nous allons maintenant faire la même chose, mais pour un **autre concept**. Regardons ces exemples :  

```haskell
("Abstrais " ++ "moi") ++ "!"  -- "Abstrais moi!"
"Abstrais " ++ ("moi" ++ "!")  -- "Abstrais moi!"

(2 * 3) * 4  -- 24
2 * (3 * 4)  -- 24

(True && False) && True  -- False
True && (False && True)  -- False
```
Que remarquez-vous ?  

Dans chaque cas, nous avons une **opération binaire** qui combine deux valeurs du même type pour en produire une nouvelle :  

```haskell
(++)  :: [a] -> [a] -> [a]
(*)   :: Num a => a -> a -> a
(&&)  :: Bool -> Bool -> Bool
```
Et en plus, ces opérations sont **associatives** :  

```haskell
x <> (y <> z) = (x <> y) <> z
```
Nous avons donc une **abstraction naturelle** : **Semigroup**, qui représente les types possédant une opération binaire associative.  

Voici comment nous définissons la classe de types **Semigroup** en Haskell :  

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```
C'est tout !  

Nous avons choisi `(<>)` comme opérateur car il est souvent utilisé en infixe.  

### Implémentation pour les listes  
L'opération binaire évidente pour les listes est `++`, qui est **associative**. Nous pouvons donc définir :  

```haskell
instance Semigroup [a] where
  (<>) = (++)
```
Nous pouvons tester l’associativité :  

```haskell
("est " <> "ceci ") <> "vrai?" == "est " <> ("ceci " <> "vrai?")

(([1] <> [2]) <> []) <> [3,4] == [1] <> ([2] <> ([] <> [3,4]))
```
Ces tests confirment que `(<>)` respecte bien la loi d'associativité.  

---

### Plusieurs instances possibles  
Parfois, un type peut avoir **plusieurs opérations associatives valables**. Exemple avec les nombres :  

```haskell
(+)  :: Num a => a -> a -> a
(*)  :: Num a => a -> a -> a
```
Les deux sont associatifs, mais nous ne pouvons choisir qu'un seul `(<>)`... ou alors, nous créons **deux types distincts** :  

```haskell
newtype Sum a = Sum { getSum :: a }
newtype Product a = Product { getProduct :: a }

instance Num a => Semigroup (Sum a) where
  (Sum a) <> (Sum b) = Sum (a + b)

instance Num a => Semigroup (Product a) where
  (Product a) <> (Product b) = Product (a * b)
```
Et maintenant, nous pouvons tester :  

```haskell
Sum 3 <> Sum 2      -- Sum {getSum = 5}
Product 5 <> Product 9  -- Product {getProduct = 45}
```
Cela fonctionne ! 🎉  

---

### Cas d'un type personnalisé  
Prenons un type `Severity` pour la gravité d'une urgence :  

```haskell
data Severity = Low | Medium | High | Critical deriving (Show, Eq)

instance Semigroup Severity where
  Critical <> _ = Critical
  _ <> Critical = Critical
  High <> _     = High
  _ <> High     = High
  Medium <> _   = Medium
  _ <> Medium   = Medium
  _ <> _        = Low
```
Ainsi, **une urgence plus grave l'emporte toujours**.  

---

**Félicitations 🎉 !** Vous avez compris **Semigroup**. Maintenant, passons à **Monoid** ! 🚀


La classe de type Monoid

La classe de type Monoid repose sur la classe de type Semigroup pour ajouter un comportement supplémentaire minime mais significatif. Jetons un coup d'œil au même exemple que nous avons vu au début, mais avec une légère modification :

("Abstract " ++ "me") ++ "!"        -- "Abstract me!"
"Abstract " ++ "" ++ ("me" ++ "!")  -- "Abstract me!"

(2 * 3) * 4      -- 24
2 * 1 * (3 * 4)  -- 24

(True && False) && True          -- False
True && True && (False && True)  -- False

Remarquez-vous les changements que j'ai apportés au code ? Et qu'en est-il des résultats ?

Comme vous pouvez le voir, j'ai ajouté une opération supplémentaire dans la deuxième ligne de chaque exemple, mais cela n'affecte pas le résultat final car l'une des valeurs n'a aucun effet. Nous appelons une valeur qui ne modifie pas le résultat : la valeur "Identité". Ce n'est pas la première fois que nous rencontrons ce concept. Nous avons d'abord appris les identités lorsque nous avons appris la récursion et combien elles sont essentielles pour définir les cas de base.

Et comme vous pouvez le voir, le 1 est l'identité pour la multiplication, le True est l'identité pour &&, et la chaîne vide est l'identité pour la concaténation des chaînes de caractères, ce qui signifie, plus généralement, que la liste vide est l'identité de la concaténation des listes.

Donc, si nous l'écrivons explicitement, le modèle que nous voyons ici est :

Un Monoid est un type qui possède une opération binaire associative avec un élément identité.

Mais nous avons déjà une classe de type qui représente une opération binaire associative. Ainsi, au lieu de nous répéter, nous pouvons faire de Monoid une sous-classe de Semigroup et ajouter seulement l'identité. Quelque chose comme ceci :

```haskell
class Semigroup a => Monoid a where
  mempty :: a
```

Ici, la valeur `mempty` représente l'élément identité. Elle est appelée ainsi par convention. Vous pouvez la lire comme "m" (pour Monoid) vide.

Et cela serait conceptuellement tout. Mais, si nous regardons la classe de type Monoid réelle en Haskell, elle pourrait ressembler à ceci :

```haskell
class Semigroup a => Monoid a where
  mempty  :: a             -- Élément identité
  mappend :: a -> a -> a   -- <>
  mconcat :: [a] -> a      -- foldr <> mempty
  {-# MINIMAL mempty | mconcat #-}
```

Pourquoi donc ces fonctions supplémentaires ?

Eh bien, car dans les versions précédentes de Haskell, nous n'avions pas la classe de type Semigroup. La classe de type Monoid était autonome et devait définir sa propre opération binaire associative. La fonction `mappend` était l'opération binaire associative que nous avons définie dans Semigroup, et la fonction `mconcat` est un comportement que nous obtenons gratuitement grâce à `mempty` et `mappend`. C'est juste `foldr` appliqué à l'opérateur binaire et `mempty`.

Nous n'avons pas supprimé `mappend` de Monoid lorsque Semigroup a été introduit, car cela aurait cassé presque tous les programmes écrits en Haskell. Ainsi, pour éviter une vague d'e-mails en colère des développeurs Haskell, les mainteneurs ont progressivement introduit les changements pour donner à tout le monde le temps de s'adapter avant de le supprimer.

Remarquez, cependant, que la restriction selon laquelle `mempty` doit être l'identité de l'opération n'est indiquée nulle part dans le code. Nous ne pouvons pas l'imposer par le code, donc nous créons des lois qui indiquent au développeur qu'il doit adhérer à certaines règles supplémentaires lorsqu'il implémente des instances de Monoid :

**Identité à droite**
```haskell
x <> mempty = x -- e.g.: Sum 4 <> Sum 0 == Sum 4
```

**Identité à gauche**
```haskell
mempty <> x = x -- e.g.: Sum 0 <> Sum 4 == Sum 4
```

**Associativité**
```haskell
x <> (y <> z) = (x <> y) <> z -- (Loi de Semigroup)
```

**Concaténation**
```haskell
mconcat = foldr (<>) mempty
```

### Pourquoi utiliser Semigroup et Monoid ?

**Calcul distribué**

Si le résultat d'un calcul IO est un Monoid, alors le calcul IO lui-même est un Monoid ! Cela signifie que vous pouvez diviser le travail en plusieurs serveurs et fusionner les résultats au fur et à mesure que les serveurs terminent leurs calculs. Cela permet d'accélérer les calculs en parallèle sans modifier la logique initiale.

**Complexité croissante sans complexification du code**

Lorsque vous ajoutez des formulaires à une application, l'intégration devient de plus en plus difficile. Mais si les formulaires eux-mêmes étaient des Semigroup, ils pourraient être combinés sans effort supplémentaire.

C'est pourquoi Semigroup et Monoid sont des concepts puissants et utiles en programmation fonctionnelle !

