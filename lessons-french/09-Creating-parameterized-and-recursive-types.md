Creating-parameterized-and-recursive-types

### Création de types paramétrés et récursifs

#### Plan
- Paramétrisation des types
- Paramétrisation des synonymes de type
- Paramétrisation des types de données
- Types de données récursifs
- Tweet me a river
- Une séquence de nœuds
- Un arbre de nœuds
- Kinds
- Le mot-clé `newType`

### Paramétrisation des types
Un constructeur de valeur prend des valeurs en paramètre et produit une valeur.

```
                        |
                        v
```

Un constructeur de type prend des types en paramètre et produit un type.

Nous pouvons utiliser les constructeurs de type avec des synonymes de type et avec des nouveaux types. Commençons par les synonymes de type.

### Paramétrisation des synonymes de type
En revenant à notre dernier synonyme de type, nous avions :

```haskell
type Name = String
type Address = (String, Int)
type Person = (Name, Address)

bob = ("Bob Smith", ("Main St.", 555)) :: Person
```

Imaginons que nous devions identifier les entreprises par leur identifiant numérique et les fournisseurs par leur identifiant alphanumérique.

Nous pourrions faire quelque chose comme ceci :

```haskell
type Address = (String, Int)
type Name = String
type CompanyId = Int
type ProviderId = String

type Person = (Name, Address)
type Company = (CompanyId, Address)
type Provider = (ProviderId, Address)

bob = ("Bob Smith", ("Main St.", 555)) :: Person
io = (584264, ("Cardano St.", 999)) :: Company
google = ("Google LLC", ("Amphitheatre Parkway", 1600)) :: Provider
```

Nous obtenons le résultat souhaité, mais nous avons répété la même structure trois fois. Une approche différente consisterait à définir un synonyme de type paramétrique.

Par exemple, nous pourrions créer le type `Entity a` :

```haskell
type Entity a = (a, Address)
```

Ainsi, nous pouvons ajuster le type de `a` selon nos besoins :

```haskell
type Name = String
type Address = (String, Int)
type CompanyId = Int
type ProviderId = String

type Entity a = (a, Address)

bob = ("Bob Smith", ("Main St.", 555)) :: Entity Name
io = (584264, ("Cardano St.", 999)) :: Entity CompanyId
google = ("Google LLC", ("A. Parkway", 1600)) :: Entity ProviderId
other = (True, ("Some street", 0)) :: Entity Bool
```

Nous avons à présent quatre valeurs différentes avec quatre types différents, tous obtenus en passant un type différent au même constructeur de type.

Remarque :
- `Entity` seul est un constructeur de type, pas un type.
- `Entity Name`, `Entity CompanyId`, `Entity ProviderId` et `Entity Bool` sont des types complètement différents !

Nous pouvons également utiliser plusieurs paramètres :

```haskell
type Entity a b = (a, b)
```

Cela nous permet de définir `Address` comme un cas particulier de `Entity` :

```haskell
type Address = Entity String Int
```

Nous obtenons alors :

```haskell
bob = ("Bob Smith", ("Main St.", 555)) :: Entity Name Address
io = (584264, ("Cardano St.", 999)) :: Entity CompanyId Address
google = ("Google LLC", ("A. Parkway", 1600)) :: Entity ProviderId Address
other = (True, ("Some street", 0)) :: Entity Bool Address
```

### Paramétrisation des types de données
Pour ajouter des paramètres en définissant de nouveaux types, il suffit d'ajouter le paramètre à gauche du `=` et de l'utiliser à droite :

```haskell
data Box a = Empty | Has a deriving (Show)
```

Ici, `Box` est un constructeur de type prenant une variable de type `a`. Nous avons deux constructeurs de valeur :

```haskell
Empty :: forall a. Box a
Has   :: forall a. a -> Box a
```

Par exemple :

```haskell
box1 = Has "Qu'y a-t-il dans la boîte ?!"
box2 = Empty
```

Nous pouvons également créer des fonctions pour manipuler ces types :

```haskell
addN :: Num a => a -> Box a -> Box a
addN _ Empty   = Empty
addN n (Has a) = Has (a + n)

addBoxes :: Num a => Box a -> Box a -> Box a
addBoxes _ Empty = Empty
addBoxes Empty _ = Empty
addBoxes (Has a) (Has b) = Has (a + b)
```

### Types de données récursifs
Nous ne pouvons pas définir des synonymes de type récursifs, mais nous pouvons définir des types de données récursifs.

#### Un tweet récursif

```haskell
data Tweet = Tweet
  { contents :: String
  , likes :: Int
  , comments :: [Tweet]
  } deriving (Show)
```

Nous pouvons créer une structure de tweets imbriquée :

```haskell
tweet :: Tweet
tweet = Tweet "Je suis en colère !" 5
    [ Tweet "Moi aussi !" 0 []
    , Tweet "Ca m'énerve que tu sois énervé" 2
        [ Tweet "Je ne comprends rien." 3 [] ]
    ]
```

La fonction suivante calcule l'engagement :

```haskell
engagement :: Tweet -> Int
engagement Tweet {likes = l, comments = c} = l + length c + sum (map engagement c)
```

Nous obtenons `engagement tweet = 13`.

C'est un long texte ! Voici la traduction en français :  

---

## Maintenant, écrivez une fonction pour vérifier si un élément spécifique est présent dans cette séquence.  

L'intervieweur était satisfait, mais il ne faisait que commencer ! Il a ensuite demandé :  

Pas de problème ! Vous deviez implémenter la fonction `elem` pour votre nouveau type, de la même manière qu'elle est implémentée pour les listes :  

```haskell
-- data Sequence a = EmptyS | a :-> (Sequence a)

elemSeq :: (Eq a) => a -> Sequence a -> Bool
elemSeq _ EmptyS = False
elemSeq x (y :-> ys) = x == y || elemSeq x ys
```

Vous définissez la fonction `elemSeq` qui prend une valeur de type `a` et une valeur de type `Sequence a` et retourne un `Bool`. Où `a` est une instance de la classe de type `Eq` (car vous allez vérifier l'égalité).  

Vous avez deux constructeurs, donc vous commencez avec deux équations. Une pour le constructeur `EmptyS` et une pour le constructeur `:->`.  

- Si la séquence est vide, peu importe la valeur recherchée, elle ne peut pas être présente.  
- Si la séquence contient au moins un élément, vous extrayez la valeur du premier élément (`y`), vérifiez si elle est égale à la valeur recherchée (`x`), et appliquez récursivement `elemSeq` au reste de la séquence.  

Si au moins un élément correspond, la fonction doit retourner `True`. C'est pourquoi on utilise l'opérateur `||` (OU logique) : dès qu'un match est trouvé, toute l'exécution retournera `True`.  

Exemple d'utilisation :  

```haskell
seq5 = 'a' :-> 'b' :-> '4' :-> '%' :-> EmptyS

elemSeq 'c' seq5  -- False
elemSeq '%' seq5  -- True
```

### Une meilleure performance avec un arbre binaire de recherche  

L'intervieweur approuve votre implémentation, mais il soulève un problème :  
> "J'ai des dizaines de milliers d'éléments, et si nous devons les vérifier un par un, cela prendra une éternité !"  

Vous aviez vu venir cette remarque et avez répondu :  
> "Pas de problème ! Si les valeurs sont triées, nous pourrions utiliser un **arbre binaire de recherche** !"  

#### Un arbre de nœuds  

Imaginez que vous deviez chercher un mot dans un dictionnaire physique. Est-ce que vous commencez par la première page et continuez jusqu'à la fin ? Bien sûr que non ! Vous ouvrez le dictionnaire vers le milieu, puis vous choisissez la moitié appropriée en fonction de l'ordre alphabétique, et ainsi de suite.  

Ce processus est appelé **"algorithme de recherche binaire"**, et il est **bien plus efficace** qu'une recherche linéaire.  

Si un dictionnaire contient **10 000** pages :  
- Une recherche linéaire prendrait **jusqu'à 10 000** opérations.  
- Une recherche binaire prend au **pire 13** opérations !  

C'est un changement majeur en termes d'efficacité.  

### Définition d'un arbre binaire de recherche en Haskell  

Un **arbre binaire de recherche** (Binary Search Tree - BST) a ces caractéristiques :  
1. Chaque nœud a au **maximum deux sous-nœuds**.  
2. Il a **une seule racine** (par exemple, le nœud `8` dans l'image).  
3. Il y a **un unique chemin** pour atteindre chaque nœud.  
4. Tous les éléments **à gauche** d'un nœud sont **inférieurs** à sa valeur.  
5. Tous les éléments **à droite** sont **supérieurs**.  

Voici comment traduire cela en Haskell :  

```haskell
data Tree a = EmptyT | Node a (Tree a) (Tree a) deriving (Show)
```

Cette définition est similaire à `Sequence`, sauf que `Node` contient **deux** sous-arbres au lieu d'un seul.  

Nous pouvons maintenant créer quelques arbres :  

```haskell
emptyTree :: Tree a
emptyTree = EmptyT

oneLevelTree :: Tree Char
oneLevelTree = Node 'a' EmptyT EmptyT

twoLevelTree :: Tree Integer
twoLevelTree = Node 8
  (Node 3  EmptyT EmptyT)
  (Node 10 EmptyT EmptyT)

threeLevelTree :: Tree Integer
threeLevelTree = Node 8
  (Node 3
    (Node 1 EmptyT EmptyT)
    (Node 6 EmptyT EmptyT)
  )
  (Node 10
    EmptyT
    (Node 14 EmptyT EmptyT)
  )
```

### Recherche d'un élément dans un BST  

Nous devons maintenant écrire `elemTree`, une fonction qui vérifie si une valeur est présente dans un BST.  

Type de la fonction :  

```haskell
elemTree :: (Ord a) => a -> Tree a -> Bool
```

Pourquoi `Ord a` ? Car nous allons comparer les valeurs avec `<` et `>`.  

Implémentation :  

```haskell
elemTree :: (Ord a) => a -> Tree a -> Bool
elemTree v EmptyT = False
elemTree v (Node x left right)
    | v == x = True
    | v > x  = elemTree v right
    | v < x  = elemTree v left
```

Explication :  
- Si l'arbre est vide → `False`  
- Si la valeur du nœud correspond → `True`  
- Si la valeur est **plus grande**, on continue la recherche dans le **sous-arbre droit**  
- Si la valeur est **plus petite**, on continue la recherche dans le **sous-arbre gauche**  

Exemples :  

```haskell
elemTree 6 threeLevelTree  -- True
elemTree 17 threeLevelTree -- False
```

Avec un BST, au lieu de parcourir **tous** les éléments un par un, nous divisons le problème en deux à chaque étape. **Un gain énorme de performance !**  

---

## La forme d'un type dicte sa fonction  

En général, la définition d'une fonction suit **la structure du type**.  

Exemples :  

1. **Boîte avec un seul élément**  
   ```haskell
   -- data Box a = Empty | Has a
   extract :: a -> Box a -> a
   extract def Empty   = def
   extract _   (Has x) = x
   ```
   → Deux constructeurs, donc **deux définitions**. Pas de récursion.  

2. **Séquence récursive**  
   ```haskell
   -- data Sequence a = EmptyS | a :-> (Sequence a)
   elemSeq _ EmptyS = False
   elemSeq x (y :-> ys) = x == y || elemSeq x ys
   ```
   → Un constructeur est **récursif**, donc l'implémentation l'est aussi.  

3. **Arbre binaire**  
   ```haskell
   -- data Tree a = EmptyT | Node a (Tree a) (Tree a)
   elemTree v EmptyT = False
   elemTree v (Node x left right)
       | v == x = True
       | v > x  = elemTree v right
       | v < x  = elemTree v left
   ```
   → Un constructeur **deux fois récursif**, donc la fonction appelle **deux fois** `elemTree`.  

---

## Les **kinds** : Le type des types  

Haskell permet d'obtenir des informations sur un type avec `:k`.  

Exemples :  

```haskell
:k Int       -- Int :: *
:k Box       -- Box :: * -> *
:k Tree      -- Tree :: * -> *
:k Box Int   -- Box Int :: *
```

Explication :  
- `*` → Type concret (comme `Int` ou `Bool`).  
- `* -> *` → Constructeur de type qui prend un type concret (comme `Box a`).  
- `* -> * -> *` → Constructeur de type qui prend **deux** types (comme `data Pair a b = Pair a b`).  

---

## Le mot-clé **newtype**  

Haskell propose `newtype`, qui est similaire à `data`, **mais plus performant**.  

```haskell
newtype Color a = Color a
newtype Product a = Product { getProduct :: a }
```

**Avantage** : Meilleure optimisation en Haskell !  

---

Et voilà ! 🚀 Vous avez appris comment la structure d'un type influence la manière dont on écrit les fonctions associées.
