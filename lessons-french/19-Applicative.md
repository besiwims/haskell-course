Applicative

Foncteur Applicatif

Plan
Pourquoi les foncteurs applicatifs ?
Notre parcours jusqu'à présent
Les limites du foncteur
Application de fonctions au niveau du foncteur 🏆
Être pur 😇
La classe de type Applicative
Les lois des foncteurs applicatifs
🎆 Programmer avec des effets 🎆
Fonctions supplémentaires et Applicative telles que définies dans base

Pourquoi les foncteurs applicatifs ?
Notre parcours jusqu'à présent
Nous avons besoin de foncteurs applicatifs (ou simplement Applicatives) en raison d'une limitation des foncteurs réguliers. Pour mieux comprendre, récapitulons notre parcours d'abstraction pour map et Functor.

Nous avons commencé avec une série de fonctions récursives :

```haskell
lowerString :: [Char] -> [Char]
lowerString []     = []
lowerString (x:xs) = toLower x : lowerString xs

addOne :: Num a => [a] -> [a]
addOne []     = []
addOne (x:xs) = (x + 1) : addOne xs

boolToBit :: [Bool] -> [Char]
boolToBit []     = []
boolToBit (x:xs) = (if x then '1' else '0') : boolToBit xs
```

Ces fonctions étaient utiles mais limitées à l'application d'une fonction spécifique à une liste de types spécifiques. Nous avons remarqué qu'elles partageaient des traits communs, ce qui nous a permis d'extraire la fonction `map` :

```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

lowerString = map toLower
addOne = map (+1)
boolToBit = map (\x -> if x then '1' else '0')
```

La fonction `map` est une version plus puissante car plus générale. Nous pouvons appliquer n'importe quelle fonction à une liste de valeurs et recréer les fonctions originales en passant simplement la fonction concrète à appliquer.

Cependant, `map` ne fonctionne que pour les listes. D'autres structures contiennent des valeurs que nous souhaitons manipuler. Nous pouvons définir des fonctions similaires pour elles :

```haskell
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing  = Nothing
maybeMap f (Just x) = Just (f x)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x)       = Leaf (f x)
treeMap f (Node lt x rt) = Node (treeMap f lt) (f x) (treeMap f rt)
```

Nous avons alors extrait les traits communs pour obtenir la classe de type `Functor` :

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a
    (<$) = fmap . const
    {-# MINIMAL fmap #-}
```

**Lois des foncteurs :**
- Loi d'identité : `fmap id == id`
- Loi de composition : `fmap (f . g) == fmap f . fmap g`

Nous avons alors créé des instances pour `Functor` :

```haskell
instance Functor [] where
  fmap _ []     = []
  fmap f (x:xs) = f x : fmap f xs

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)
  
instance Functor Tree where
  fmap f (Leaf x)       = Leaf (f x)
  fmap f (Node lt x rt) = Node (fmap f lt) (f x) (fmap f rt)
```

### Les limites du foncteur

Essayons d'appliquer des fonctions à `Maybe` :

```haskell
:t (+1)             -- Int -> Int
:t (+1) <$> Just 3  -- Maybe Int
(+1) <$> Just 3

:t (+)             -- Int -> Int -> Int
:t (+) <$> Just 3  -- Maybe (Int -> Int)
```

Mais comment additionner `Just 3` et `Just 2` ?

```haskell
almostThere = (<$> Just 3) <$> ((+) <$> Just 2)
:t almostThere -- Maybe (Maybe Int)
almostThere
```

Nous obtenons un `Maybe (Maybe Int)`, ce qui n'est pas ce que nous voulons.

Une solution serait de créer une version de `fmap` pour chaque arité :

```haskell
fmap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
fmap2 f (Just a) (Just b) = Just (f a b)
fmap2 _ _ _               = Nothing

fmap2 (+) (Just 3) (Just 2)
```

Mais cela devient vite ingérable. Grâce à la curryfication, nous pouvons trouver une meilleure solution !

### Application de fonctions au niveau du foncteur

Nous voulons une opération similaire à l'application de fonctions en Haskell :

```haskell
($) :: (a -> b) -> a -> b
```

L'opérateur `(<*>)` généralise cette application pour les foncteurs :

```haskell
(<*>) :: Functor f => f (a -> b) -> f a -> f b
```

Exemple avec `Maybe` :

```haskell
Just (+1) <*> Just 3  -- Just 4
Just (+) <*> Just 3 <*> Just 2  -- Just 5
```

### Être pur 😇

Nous avons besoin d'une fonction `pure` pour injecter des valeurs dans un contexte applicatif :

```haskell
pure :: a -> f a
```

Exemple :

```haskell
pure 3 :: Maybe Int  -- Just 3
```

Avec `Applicative`, nous avons tout ce qu'il faut pour travailler avec plusieurs foncteurs de manière propre et générale !

Voici la traduction en français :  

---

### Définition de l'instance pour Identity  

```haskell
newtype Identity a = Identity { getValue :: a } deriving (Show, Eq)

instance Functor Identity where
  -- fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)
```

Bien sûr, comme **Applicative** est une sous-classe de **Functor**, nous devons d'abord définir **Functor**. C'est assez simple. Nous effectuons un **pattern matching** pour extraire la valeur encapsulée par le constructeur **Identity**, nous appliquons la fonction à la valeur sous-jacente, puis nous la ré-emballons.  

Passons maintenant à la partie nouvelle :  

```haskell
instance Applicative Identity where
  -- pure :: a -> Identity a
  pure = Identity
  
  -- (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  -- Identity f <*> Identity a = Identity (f a)
  Identity f <*> ia = fmap f ia -- même définition que ci-dessus
```

Pour créer une instance de la classe de type **Applicative**, nous devons définir **pure** et l'opérateur d'application `(<*>)`.  

- Pour **pure**, nous n'avons pas beaucoup d'options. Nous devons soulever une valeur au niveau de **Identity**, et nous n'avons qu'un seul constructeur avec le même type que **pure**. Donc, nous l'utilisons.  

- Pour l'opérateur `(<*>)`, nous avons deux choix :  
  - Nous pouvons utiliser le **pattern matching** pour extraire à la fois la fonction et la valeur, appliquer la fonction à la valeur, et ré-emballer le résultat.  
  - Nous pouvons utiliser **fmap** et ne faire un **pattern matching** que sur la fonction.  

Comme vous pouvez le voir dans la définition du foncteur, c'est la même chose, sauf que nous évitons de nous répéter.  

---

### Création d'une instance Applicative pour Maybe  

Nous faisons la même chose pour le type **Maybe**.  

```haskell
data Maybe a = Nothing | Just a
```

#### Instance Functor pour Maybe  

```haskell
instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)
```

#### Instance Applicative pour Maybe  

```haskell
instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure = Just
  
  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Just f <*> ma = fmap f ma
  _      <*> _  = Nothing
```

Nous ne devons pas exécuter cette cellule car ces instances sont déjà fournies par **Prelude**.  

L'implémentation de **Functor** ayant déjà été faite, passons à **Applicative**.  

Pour **pure**, nous avons deux options qui respectent le type :  

- Nous pourrions créer une fonction qui ignore l'entrée et retourne toujours **Nothing** ❌  
- Nous pourrions utiliser **Just**, qui fonctionne parfaitement bien ✅  

Conceptuellement, **Just** est le bon choix. Si nous utilisons **Nothing**, nous obtenons toujours **Nothing**, quelle que soit la valeur levée. Mais en plus de cela, **Just** est le seul choix si nous voulons respecter les lois de **Applicative** !  

---

## Les lois de l'Applicative  

L'opérateur `<*>` doit respecter certaines lois pour assurer la cohérence avec la structure d'un foncteur applicatif.  

### 1. Identité  

**Loi :**  
```haskell
pure id <*> v = v
```
Exemple avec **Maybe** :  
```haskell
(pure id <*> Just 3) == Just 3
```

Si nous définissions une mauvaise implémentation de `pure` :  
```haskell
wrongPure = \x -> Nothing
(wrongPure id <*> Just 3) == Just 3  -- ❌ Faux !
```

Cela montre que **pure** doit être **Just**, sinon l'identité ne fonctionne pas.  

---

### 2. Homomorphisme  

**Loi :**  
```haskell
pure f <*> pure x = pure (f x)
```
Cela garantit que l'application dans le contexte applicatif fonctionne comme une application normale.  

Exemple :  
```haskell
leftSide = pure show <*> pure 3
rightSide = pure (show 3)

leftSide == rightSide  -- ✅ Vrai !
```

---

### 3. Composition  

**Loi :**  
```haskell
(pure (.) <*> u <*> v) <*> w = u <*> (v <*> w)
```

Exemple :  
```haskell
leftSide  = (pure (.) <*> Just show <*> Just (*2)) <*> Just 3
rightSide = Just show <*> (Just (*2) <*> Just 3)

leftSide == rightSide  -- ✅ Vrai !
```

Cette loi garantit que `<*>` est **associatif**.  

---

### 4. Interchangeabilité  

**Loi :**  
```haskell
f <*> pure x = pure (\f -> f x) <*> f
```
Exemple :  
```haskell
leftSide  = Just (+1) <*> pure 3
rightSide = pure (\f -> f 3) <*> Just (+1)

leftSide == rightSide  -- ✅ Vrai !
```

Cela montre que nous pouvons **inverser l'ordre** des arguments dans l'application d'une fonction.  

---

## Instance Applicative pour Either  

Nous définissons maintenant l'instance **Applicative** pour `Either e` :  

```haskell
instance Functor (Either e) where
  fmap f (Left e) = Left e
  fmap f (Right a) = Right (f a)
```

```haskell
instance Applicative (Either e) where
  -- pure :: a -> Either e a
  pure = Right
  
  -- (<*>) :: Either e (a -> b) -> Either e a -> Either e b
  Left e <*> _ = Left e
  Right f <*> r = fmap f r
```

Pourquoi utilisons-nous **Right** pour `pure` ?  
- **Left** ne peut pas contenir de valeur de type `a`, donc on ne peut pas l'utiliser.  
- Si `pure` était **Left**, tout serait toujours une erreur, ce qui serait inutile.  

Exemples d'utilisation :  

```haskell
Right (+1) <*> pure 1
(+) <$> Right 1 <*> Right 2
(\a b c -> a + b * c) <$> Right 1 <*> pure 2 <*> pure 3
```

Nous pouvons également vérifier que cette implémentation respecte toutes les lois d'Applicative ✅.  

---

## Programmation avec Effets  

Nous avons maintenant une **nouvelle perspective** sur l'Applicative.  

En effet, les **types** comme `Maybe`, `Either`, ou `[]` peuvent être vus comme des **effets simulés**.  

- `Maybe` simule **l'effet d'un calcul qui peut échouer**.  
- `Either e` simule **l'effet d'un calcul avec un message d'erreur**.  
- `[]` simule **l'effet d'un calcul non déterministe** (avec plusieurs résultats possibles).  
- `IO` représente un **véritable effet** (interactions avec le monde extérieur).  

Le concept clé ici est que **pure insère une valeur sans effet dans un contexte effectif**.  

---

## Les Fonctions Utiles de Applicative  

```haskell
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f x = pure f <*> x -- Lève une fonction unaire.

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y -- Lève une fonction binaire.

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f x y z = liftA2 f x y <*> z -- Lève une fonction ternaire.
```

Utilisation :  
```haskell
liftA (+1) [1, 2]
liftA2 (+) (Right 2) (Right 3)
```

---

Félicitations ! Vous comprenez maintenant **Applicative** et son lien avec la programmation avec effets ! 🚀


Celui qui ignore la valeur du deuxième argument.

Nous avons dit que l'opérateur apply séquence les effets/calculs/actions et combine leurs résultats. Eh bien, ceux-ci font la même chose, mais ignorent la valeur produite par l'une des entrées. Remarquez que, tout comme leurs analogues de la classe de type Functor, la flèche pointe vers la valeur que vous obtiendrez.

Si vous voulez exécuter l'effet ou le calcul sans vous soucier du résultat, comme afficher un message sur la console, vous pouvez utiliser l'un de ces opérateurs.

Ensuite, nous avons l'opérateur d'application inversée :

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (\a f -> f a) -- Une variante de '<*>' avec les types des arguments inversés.

-----------------------------------------------------------------------------------------------

-- Tentative 1 : Valeurs correctes, ordre des effets incorrect
(<*>) (id <$ print "Récupérer la valeur de la BD") (print "Sauvegarder la valeur dans la BD")

-- Tentative 2 : Valeurs correctes, ordre des effets incorrect
(flip (<*>)) (print "Sauvegarder la valeur dans la BD") (id <$ print "Récupérer la valeur de la BD")

-- Tentative 3 : Valeurs correctes, ordre des effets correct
(<**>) (print "Sauvegarder la valeur dans la BD") (id <$ print "Récupérer la valeur de la BD")
"Récupérer la valeur de la BD"
"Sauvegarder la valeur dans la BD"
"Récupérer la valeur de la BD"
"Sauvegarder la valeur dans la BD"
"Sauvegarder la valeur dans la BD"
"Récupérer la valeur de la BD"

Cet opérateur fait la même chose que l'opérateur apply, mais il prend d'abord la valeur applicative, puis la fonction applicative.

Cet opérateur est utile dans les cas où les valeurs obtenues sont correctes, mais l'ordre des effets est erroné. Dans cet exemple, nous utilisons des effets IO. Le type IO est une instance de Applicative.

Dans la première tentative, nous évaluons le premier Applicative et récupérons la valeur de notre base de données. Ensuite, nous évaluons le second Applicative et sauvegardons la valeur dans la base de données. Après avoir exécuté les deux effets, nous appliquons la fonction id au résultat de print, qui est une unité.

Le problème est que les effets sont dans le mauvais ordre ! Nous devons d'abord sauvegarder la valeur avant de pouvoir la récupérer. Mais nous ne pouvons pas simplement changer l'ordre des arguments. Le résultat du premier effet est une fonction (dans ce cas, la fonction id), et le résultat du second effet est une valeur constante, l'unité. L'un est une fonction et l'autre est une valeur constante. Que faire ?

Une solution envisageable est d'utiliser la fonction flip que nous avons apprise il y a plusieurs leçons. Elle est conçue spécifiquement pour inverser les arguments d'une fonction. C'est ce que nous voyons dans la deuxième tentative. Et le code semble correct. Mais, si nous évaluons l'expression, nous la réduisons et nous obtenons la même expression finale. La fonction flip ne fait que changer la façon dont nous fournissons les valeurs à la fonction d'origine, mais pas comment ces valeurs sont évaluées. Donc, même si nous écrivons les valeurs dans un ordre différent, les effets s'exécutent dans le même ordre. C'est précisément le problème que ce nouvel opérateur résout. Il ne fait pas que permuter les arguments, mais il résout aussi les effets dans l'ordre où ils sont présentés. Ainsi, nous pouvons enfin sauvegarder d'abord la valeur dans la base de données, puis la récupérer.

Une autre fonction utile est la fonction forever :

forever :: (Applicative f) => f a -> f b
forever a = a *> forever a -- Même chose que : a *> a *> a *> a... infiniment

forever getLine
premier
second
third
quatrième
...
Comme vous pouvez le voir dans la définition, nous obtenons un applicatif, nous exécutons son effet, nous ignorons le résultat, et nous exécutons récursivement forever avec le même Applicative. Cette fonction ne s'arrêtera jamais d'elle-même.

C'est la première fois que nous avons une fonction de boucle infinie réellement utile ! Maintenant que nous avons les Applicatives, nous pouvons exécuter des effets. Donc, les boucles infinies ont du sens parce que nous ne nous intéressons pas au retour de la fonction, mais à ce que fait l'effet. Un cas d'utilisation courant, par exemple, est d'avoir un serveur écoutant indéfiniment les connexions des clients. Comme nous l'avons vu précédemment, le type IO est une instance de Applicative, donc nous pouvons utiliser l'opérateur de séquence *> . Vous voulez que le serveur continue d'écouter tant qu'il est en marche, donc vous utilisez une boucle infinie.

(...)

