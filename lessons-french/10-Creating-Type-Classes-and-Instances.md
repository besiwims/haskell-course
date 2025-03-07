10-Creating-Type-Classes-and-Instances

Here’s the French translation of your text:  

---

### Création de Classes de Types et d'Instances  
#### Plan  
- Surcharge  
- Étapes pour créer des Classes de Types et des Instances  
- La classe de type `Eq`  
- Définition de la Classe de Type  
- Définition de plusieurs instances  
- Amélioration de notre classe de type `Eq` avec la récursion mutuelle (et MCD)  
- Définition d'une instance pour un type paramétré  
- La classe de type `WeAccept`  
- La classe de type `Container`  
- Exploration de la classe de type `Ord` (sous-classification)  
- Dérivation  
- Les erreurs possibles avec la dérivation  

### Surcharge  
Avant d'apprendre ce qu'est la surcharge (overloading), découvrons ce que signifie le mot "date".  

#### DATE :  
Que signifie le mot "date" ? Si je vous disais que vous avez une seule chance de répondre correctement et que je vous donnerais 100 $ pour la bonne réponse, l'intuition vous dirait : "Ça dépend !"  

- Si vous dites : *"Quelle est votre date de naissance ?"*, alors cela signifie :  
  - Le moment où un événement se produit.  

- Si vous dites : *"Joe a invité Laura à un rendez-vous."*, alors cela signifie :  
  - Un engagement social qui a souvent un caractère romantique (sauf si Joe finit dans la friendzone).  

- Si vous dites : *"Je veux dater un fossile"*, j'ose espérer que vous ne parlez pas d'un rendez-vous galant, mais plutôt de :  
  - L'acte d'estimer ou de calculer une date ou une chronologie.  

En consultant un dictionnaire, vous verrez que "date" est aussi le nom d’un fruit et possède encore plus de définitions !  

En programmation, nous dirions que le mot "date" est **surchargé**, car il possède plusieurs définitions sous le même nom.  

D'ailleurs, le mot "surcharge" est lui-même… surchargé.  

---

### SURCHARGE :  
Dans un contexte quotidien, la surcharge signifie généralement :  
- Mettre une charge trop importante sur ou dans quelque chose.  

En programmation classique, cela signifie :  
- Avoir plusieurs implémentations d'une fonction sous le même nom.  

La façon dont cela fonctionne dépend du langage :  
- Certains langages, comme JavaScript, ne prennent pas en charge la surcharge. Vous ne pouvez donc pas l'utiliser.  
- D'autres, comme C++, permettent de créer plusieurs fonctions avec le même nom, et le compilateur choisira la bonne définition en fonction des types des arguments.  

En Haskell, la surcharge signifie :  
- Avoir plusieurs implémentations d'une fonction ou d'une valeur sous le même nom.  

Mais Haskell va encore plus loin : la surcharge ne se limite pas aux fonctions. **Les valeurs peuvent aussi être surchargées !**  

Exemples :  
- Les littéraux `1`, `2`, etc., sont surchargés, car ils peuvent être interprétés comme n'importe quel type numérique (`Int`, `Integer`, `Float`, etc.).  
- La valeur `minBound` est surchargée car, selon le type, elle peut avoir différentes valeurs (`'\NUL'` pour `Char`, `-2147483648` pour `Int`).  
- L'opérateur d'égalité `(==)` fonctionne avec plusieurs types, chacun ayant sa propre implémentation.  
- La fonction `max` fonctionne aussi avec de nombreux types.  

Nous avons donc utilisé des fonctions et valeurs surchargées tout du long. **Mais comment sont-elles définies ?** C'est là qu'interviennent les **classes de types** en Haskell.  

---

### Étapes pour créer des Classes de Types et des Instances  
Dans l'introduction aux classes de types, nous avons vu leur utilité :  
- Elles permettent de définir des fonctions pouvant être utilisées par plusieurs types.  
- Elles garantissent la sécurité en n'acceptant que les types compatibles.  

Créer une classe de type en Haskell est étonnamment simple :  

1. Déclarer une **classe de type**, en spécifiant certains comportements.  
2. Définir un **type** comme **instance** de cette classe, en implémentant ces comportements.  

C'est tout !  

Voyons cela en pratique en redéfinissant la classe de type `Eq`.  

---

### La classe de type `Eq`  
La classe `Eq` existe déjà en Haskell, mais imaginons un monde où elle n’existe pas. Dans ce monde, chaque type aurait sa propre fonction pour tester l’égalité, ce qui serait pénible.  

Définissons donc `Eq` nous-mêmes :  

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

Ici, nous :  
- Utilisons le mot-clé `class` pour déclarer une classe de type.  
- Nommons cette classe `Eq`.  
- Déclarons une variable de type `a` qui représentera n'importe quel type futur.  
- Spécifions deux fonctions :  
  - `(==)`, qui teste l’égalité entre deux valeurs du même type `a`.  
  - `(/=)`, qui teste l’inégalité.  

Nous avons maintenant une classe de type prête à l'emploi !  

---

### Définition d'une Instance pour `Eq`  
Nous allons définir un type pour les **moyens de paiement** :  

```haskell
data PaymentMethod = Cash | Card | CC -- CC = Crypto
type User = (String, PaymentMethod)
```

Imaginons que nous voulions comparer les moyens de paiement entre deux utilisateurs :  

```haskell
samePM :: User -> User -> Bool
samePM (_, pm1) (_, pm2) = pm1 == pm2  -- Erreur !
```

Le compilateur renvoie une erreur car `PaymentMethod` n'est pas une instance de `Eq`. Pour corriger cela, nous devons le définir comme une instance :  

```haskell
instance Eq PaymentMethod where
  Cash == Cash = True
  Card == Card = True
  CC == CC = True
  _ == _ = False
```

Nous avons défini `==` pour `PaymentMethod`, et maintenant l’opérateur fonctionne !  

---

### Amélioration avec la Récursion Mutuelle  
Nous pouvons améliorer notre implémentation en évitant la répétition :  

```haskell
class Eq a where
  (==), (/=)  :: a -> a -> Bool
  x /= y      = not (x == y)
  x == y      = not (x /= y)
```

Désormais, nous n'avons plus qu'à implémenter une seule fonction (`==` ou `/=`), et Haskell déduira l’autre automatiquement.  

Notre instance devient :  

```haskell
instance Eq PaymentMethod where
  Cash == Cash = True
  Card == Card = True
  CC == CC = True
  _ == _ = False
```

---

### Définition d’une Instance pour un Type Paramétré  
Imaginons une **boîte contenant un élément** :  

```haskell
data Box a = Empty | Has a
```

Nous voulons que `Box a` soit une instance de `Eq`. Mais pour comparer `Has x` et `Has y`, nous devons nous assurer que `a` est lui-même une instance de `Eq` :  

```haskell
instance (Eq a) => Eq (Box a) where
  Has x == Has y = x == y
  Empty == Empty = True
  _ == _ = False
```

Maintenant, `Box a` est une instance de `Eq` pour tous les types `a` qui sont eux-mêmes des instances de `Eq`.  

---

### Conclusion  
Nous avons appris :  
✅ Ce qu'est la surcharge en programmation.  
✅ Comment définir une classe de type (`Eq`).  
✅ Comment créer des instances pour des types simples et paramétrés.  
✅ Comment optimiser nos implémentations avec la récursion mutuelle.  

Et maintenant… allons encore plus loin ! 🚀

Bien sûr ! Voici la traduction complète en français :  

---

### La Classe de Type WeAccept  

Imaginons que nous écrivons une application qui accepte des paiements pour une entreprise, et que cette entreprise n'accepte pas toutes les méthodes de paiement, toutes les blockchains et tous les pays. Nous devons donc créer des fonctions pour vérifier cela :  

#### -- Fonction pour vérifier si nous acceptons cette méthode de paiement  
```haskell
weAcceptPayment :: PaymentMethod -> Bool
weAcceptPayment p = case p of
   Cash -> False
   Card -> True
   CC   -> True
```
#### -- Fonction pour vérifier si nous acceptons cette blockchain  
```haskell
weAcceptBlockchain :: Blockchain -> Bool
weAcceptBlockchain b = case b of
   Bitcoin  -> True
   Ethereum -> False
   Cardano  -> True
```
#### -- Type Pays  
```haskell
newtype Country = Country { countryName :: String }
```
#### -- Fonction pour vérifier si nous acceptons ce pays  
```haskell
weAcceptCountry :: Country -> Bool
weAcceptCountry c = case countryName c of
   "Mordor"  -> False
   _         -> True
```
En regardant ce code, nous réalisons que ce comportement de vérification de l'acceptation par l'entreprise pourrait être utilisé dans de nombreux autres aspects, comme les fournisseurs, les technologies, etc. Il y a beaucoup d'éléments qu'une entreprise peut décider d'accepter ou non.  

Pour éviter d'avoir un grand nombre de fonctions similaires éparpillées dans notre code, nous décidons de créer une classe de type qui représente ce comportement.  

Et cette classe de type ressemble à ceci :  

#### -- Création de la classe de type WeAccept  
```haskell
class WeAccept a where
  weAccept :: a -> Bool
```
#### -- Vérification du genre de WeAccept  
```haskell
:k WeAccept
WeAccept :: * -> Constraint
```
Maintenant que nous avons notre classe de type, nous pouvons créer les instances pour `PaymentMethod`, `Blockchain`, `Country`, et même `Box`, comme ceci :  

```haskell
instance WeAccept PaymentMethod where
  weAccept x = case x of
   Cash -> False
   Card -> True
   CC   -> True

instance WeAccept Blockchain where
  weAccept x = case x of
   Bitcoin  -> True
   Ethereum -> False
   Cardano  -> True

instance WeAccept Country where
  weAccept x = case countryName x of
    "Mordor" -> False
    _        -> True

instance (WeAccept a) => WeAccept (Box a) where
  weAccept (Has x) = weAccept x
  weAccept Empty   = False
```
Et voilà ! Nous avons maintenant la possibilité d'appliquer la fonction `weAccept` surchargée à trois types différents :  

```haskell
weAccept Cardano
weAccept Cash
weAccept (Country "Mordor")
weAccept (Has Bitcoin)
```
Résultat :  
```haskell
True
False
False
True
```
Nous pouvons également créer des fonctions pouvant être appliquées à tous les types qui sont des instances de `WeAccept` :  

#### -- Création de `fancyFunction`  
```haskell
fancyFunction :: (WeAccept a) => a -> String
fancyFunction x =
  if weAccept x
    then "Do something fancy"
    else "Don't do it!"
```
#### -- Utilisation de `fancyFunction`  
```haskell
fancyFunction Cardano
fancyFunction Card
fancyFunction (Country "Mordor")
fancyFunction (Has Bitcoin)
```
Résultat :  
```haskell
"Do something fancy"
"Do something fancy"
"Don't do it!"
"Do something fancy"
```
Une autre classe de type à notre actif ! Ça devient de plus en plus facile !  

Nous allons faire encore un exemple avant de passer à la section suivante. Celui-ci est un peu plus difficile, mais si vous le comprenez, vous serez capable de comprendre n'importe quelle classe de type, peu importe sa complexité !  

---

### La Classe de Type Container  

Voici le scénario : nous travaillons sur un logiciel logistique qui gère deux types de colis.  

1. Une boîte classique qui peut ou non contenir quelque chose.  
2. Un cadeau, qui peut aussi contenir quelque chose ou être vide, mais qui possède toujours une étiquette avec le nom du destinataire.  

Nous avons donc ces deux types :  

```haskell
data Box a       = Empty          | Has a            deriving (Show)
data Present t a = EmptyPresent t | PresentFor t a   deriving (Show)
```
#### -- Vérification des genres  
```haskell
:k Box
:k Present
Box :: * -> *
Present :: * -> * -> *
```
Puisque nous avons décidé que l'étiquette du cadeau (`t`) peut être un numéro, un nom ou tout autre élément permettant d’identifier un client, nous allons également paramétrer son type.  

Certaines parties du processus nécessitent des fonctions communes aux deux types :  

1. Une fonction pour vérifier si une boîte ou un cadeau est vide.  
2. Une fonction pour vérifier si une valeur spécifique est contenue à l'intérieur.  
3. Une fonction pour remplacer le contenu de la boîte ou du cadeau.  

Au lieu d'écrire ces fonctions séparément, allons directement à la définition de la classe de type :  

```haskell
class Container c where
    isEmpty  ::  c a -> Bool
    contains ::  (Eq a) => c a -> a -> Bool
    replace  ::  c a -> b -> c b
```
Cette classe de type s'appelle `Container` car elle fournit des comportements liés aux conteneurs.  

Nous pouvons maintenant créer les instances pour `Box` et `Present` :  

#### -- Instance pour `Box`  
```haskell
instance Container Box where
    isEmpty Empty = True
    isEmpty _     = False
    
    contains (Has x) y = x == y
    contains Empty   _ = False
 
    replace _ x = Has x
```
#### -- Instance pour `Present`  
```haskell
instance Container (Present t) where
    isEmpty (EmptyPresent _) = True
    isEmpty _                = False
    
    contains (PresentFor _ x) y = x == y
    contains (EmptyPresent _) _ = False
    
    replace (PresentFor tag _) x = PresentFor tag x
    replace (EmptyPresent tag) x = PresentFor tag x
```
#### -- Utilisation de la classe de type `Container`  
```haskell
Has False `contains` False         -- True
isEmpty (Has 'a')                  -- False
PresentFor "Tommy" 5 `contains` 5  -- True
PresentFor "Tommy" 5 `replace` "Arduino"   -- PresentFor "Tommy" "Arduino"
```
Nous avons réussi à créer une classe de type générique permettant de manipuler différents conteneurs !  

---

### Sous-classes et la Classe de Type `Ord`  

Nous allons maintenant explorer la classe de type `Ord`, qui est une sous-classe de `Eq`.  

Pour faire de `Box a` une instance de `Ord`, nous devons définir une fonction de comparaison :  

```haskell
instance (Ord a) => Ord (Box a) where
  Has x `compare` Has y = x `compare` y
  Empty `compare` Has _ = LT
  Has _ `compare` Empty = GT
  Empty `compare` Empty = EQ
```
Utilisation :  
```haskell
Has 9 >= Has 5    -- True
Empty `compare` Has 0  -- LT
Empty < Empty     -- False
```
---

### Dérivation Automatique  

Certaines instances peuvent être dérivées automatiquement :  

```haskell
data Choice = No | Idk | Yes deriving (Eq, Ord, Show, Bounded, Enum)
```
Ce qui nous permet d'utiliser directement ces comportements sans les implémenter nous-mêmes !  

---

### Conclusion  

Nous avons :  
✅ Créé et compris `WeAccept`  
✅ Défini `Container` pour les boîtes et les cadeaux  
✅ Appris le sous-typage avec `Ord`  
✅ Découvert la dérivation automatique  

Félicitations 🎉 Vous avez tout ce qu'il faut pour travailler avec les classes de types en Haskell ! 🚀
