Functor

Voici la traduction en français :  

---

# **Foncteur**  
Bienvenue dans une nouvelle leçon du cours Haskell. Celle-ci est entièrement consacrée à la classe de types **Functor**.  

## **Les foncteurs sont 🌏 PARTOUT !! 🌎**  
Dans cette leçon :  
✅ Vous comprendrez le concept de **Foncteur**  
✅ Vous apprendrez tout ce qu'il faut savoir pour les utiliser en pratique  

Les foncteurs existent en mathématiques, dans les langages de programmation, en linguistique, sous votre lit en attendant que vous dormiez… Les foncteurs sont partout ! Il est même possible que vous ayez déjà travaillé avec des foncteurs sans le savoir.  

Après cette leçon, non seulement vous comprendrez le concept de foncteur en Haskell, mais nous irons encore plus loin afin que vous sachiez tout ce qu'il faut pour les utiliser concrètement.  

## **Comment allons-nous faire cela ?**  

### **Plan**  
- **Abstraction de la fonction `map`**  
- **La classe de types `Functor`**  
- **Définition d'instances de `Functor`**  
- **Instances de `Functor` contre-intuitives**  
- **Le foncteur `Either a` 🤔**  
- **Le foncteur `(,) a` 🤨**  
- **Le foncteur `(->) r` 🤯**  
- **Définition de `<$>` et élévation 🏋️ d'une fonction**  
- **Les poupées russes du foncteur 🪆**  
- **Fonctions supplémentaires et le foncteur défini dans `base`**  

---

## **Abstraction de la fonction `map`**  
Ce chapitre sera très facile puisque vous connaissez déjà la fonction `map`. Implémentons une fonction qui transforme une chaîne de caractères en minuscules :  

```haskell
import Data.Char (toLower)

lowerString :: [Char] -> [Char]
lowerString []     = []
lowerString (x:xs) = toLower x : lowerString xs

lowerString "Salut, Comment Ça Va ?"
-- "salut, comment ça va ?"
```  

Créons maintenant une fonction qui ajoute 1 à chaque élément d'une liste de nombres :  

```haskell
addOne :: Num a => [a] -> [a]
addOne []     = []
addOne (x:xs) = (x + 1) : addOne xs

addOne [1,1,2,3]
-- [2,2,3,4]
```  

Et une fonction qui transforme une liste de valeurs booléennes en une liste de bits représentés par des caractères :  

```haskell
boolToBit :: [Bool] -> [Char]
boolToBit []     = []
boolToBit (x:xs) = (if x then '1' else '0') : boolToBit xs

boolToBit [True,False,True]
-- "101"
```  

Vous voyez où je veux en venir ? Un même **schéma** se répète, donc nous allons l'extraire dans une fonction générique.  

Nous avons toujours une **liste** en entrée, mais les types des éléments peuvent varier. Nous devons également transformer un type `a` en un type `b`. Voici donc notre abstraction :  

```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```  

Essayons-la avec nos exemples précédents :  

```haskell
map toLower "Salut, Comment Ça Va ?"  
map (+1) [1,1,2,3]  
map (\x -> if x then '1' else '0') [True,False,True]  
```  

Sortie :  

```
"salut, comment ça va ?"
[2,2,3,4]
"101"
```

Nous avons extrait le concept d'application d'une fonction arbitraire à chaque élément d'une liste, ce qui évite de **dupliquer du code**. Mais nous pouvons encore **généraliser** davantage ce concept avec la **classe de types `Functor`** ! 🚀  

---

## **Abstraction de la classe de types `Functor`**  
Nous avons de nombreux **types** en Haskell. Prenons par exemple les valeurs optionnelles avec le type `Maybe`.  

Comme pour les listes, nous voulons pouvoir appliquer une fonction à une valeur contenue dans un `Maybe` **sans modifier la structure**.  

```haskell
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing  = Nothing
maybeMap f (Just x) = Just (f x)
```  

Utilisation :  

```haskell
maybeMap toLower (Just 'A')  -- Just 'a'
maybeMap (+1) (Just 3)       -- Just 4
maybeMap (\x -> if x then '1' else '0') (Just True)  -- Just '1'

maybeMap toLower Nothing     -- Nothing
maybeMap (+1) Nothing        -- Nothing
```  

Nous retrouvons le même **schéma** que pour `map` ! 🔄  

Que ce soit pour les listes, `Maybe`, ou encore les **arbres** (`Tree`), nous appliquons une fonction à **chaque élément**, sans modifier la **structure** globale.  

Nous pouvons **abstraire** encore plus cette notion avec une **classe de types** appelée **`Functor`**.  

---

## **Définition de la classe `Functor`**  
Un **foncteur** est un type qui permet d'appliquer une fonction aux valeurs qu'il contient, **sans modifier la structure**.  

Voici la définition de la classe `Functor` :  

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```  

Nous avons cependant **une contrainte importante** : `fmap` ne doit **pas modifier la structure**.  

Par exemple, si nous définissons `fmap` pour les listes **de manière incorrecte** :  

```haskell
wrongFmap :: (a -> b) -> [a] -> [b]
wrongFmap _ []     = []
wrongFmap f (x:xs) = f x : f x : wrongFmap f xs
```  

Nous obtenons un **mauvais comportement** :  

```haskell
wrongFmap (+1) [1,2,3]
-- [2,2,3,3,4,4] ❌
```

Cela **viole** la loi d'identité des foncteurs :  

```haskell
fmap id == id
```  

Une autre loi importante est la **loi de composition** :  

```haskell
fmap (f . g) == fmap f . fmap g
```  

Si vous respectez **la loi d'identité**, alors la loi de **composition** est automatiquement satisfaite grâce au **système de types de Haskell** ! 🎉  

---

## **Définition d'instances de `Functor`**  

Voyons comment définir `Functor` pour les listes, `Maybe` et un type `Tree` :  

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

Vérifions que cela fonctionne :  

```haskell
fmap boolToBit [False,True,False]
-- "010"

fmap boolToBit (Just True)
-- Just '1'

fmap id [1,2,3] == id [1,2,3]
-- True ✅
```

Nos instances de `Functor` respectent bien les **lois des foncteurs** ! 🎉  

Et voilà ! Vous savez maintenant ce qu'est un **foncteur** en Haskell et comment l'utiliser en pratique. 🚀

---

Juste 4  
"1010"  
"1010"  
[2,4,6]  
[2,4,6]  
Et voilà comment l'utiliser.  

Maintenant, il y a une raison pour laquelle cet opérateur a un signe dollar au milieu. C'est une allusion à l'application de fonction. Si nous regardons les types :  

```haskell
($)  ::              (a -> b) ->   a ->   b
(<$>) :: Functor f => (a -> b) -> f a -> f b
```

Nous voyons que cela semble familier, non seulement en termes de type mais aussi conceptuellement. Nous savons depuis la leçon 5 que les fonctions sont associatives à droite, donc nous pouvons entourer les deux types à droite, et ce serait la même chose que de ne pas les avoir :  

```haskell
($)  ::              (a -> b) -> (  a ->   b)
(<$>) :: Functor f => (a -> b) -> (f a -> f b)
```

En regardant cela, nous voyons que l'opérateur d'application de fonction prend une fonction `(a -> b)` et retourne la même fonction `(a -> b)` sans aucun changement. Ce que nous savions déjà. Nous utilisons cet opérateur uniquement parce qu'il est associatif à droite, ce qui nous permet de supprimer les parenthèses.  

Mais si nous regardons l'opérateur `fmap` (`<$>`), nous voyons qu'il prend une fonction `(a -> b)` et retourne la même fonction, mais qui fonctionne maintenant pour la version fonctorielle de `a` et `b`. C'est ce que nous appelons "élever" une fonction. Nous disons que l'opérateur `fmap` (`<$>`) élève la fonction `(a -> b)` pour qu'elle puisse fonctionner au niveau `f`.  

Au cas où ce ne serait pas encore clair, voyons deux exemples :  

```haskell
:t toLower             -- Type de la fonction d'origine
toLower 'A'

:t (toLower <$>)       -- Type de la fonction élevée
toLower <$> Just 'A'
```

```haskell
boolToBit :: Bool -> Char
boolToBit x = if x then '1' else '0'

:t boolToBit            -- Type de la fonction d'origine
boolToBit False

:t (boolToBit <$>)      -- Type de la fonction élevée
boolToBit <$>  [False]
```

---

Cela te semble bien jusqu'à présent ? Veux-tu que je continue la traduction de tout le texte ? 😊
