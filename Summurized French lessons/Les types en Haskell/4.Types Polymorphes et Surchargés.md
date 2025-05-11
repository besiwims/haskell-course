# Types Polymorphes et Surchargés

Nous avons déjà abordé les types **polymorphes** avec notre fonction `triple`, lorsque nous l’avons rendue compatible à la fois avec des entiers et des nombres à virgule flottante.
Nous avons utilisé `Num a` dans la **signature de type** de la fonction pour indiquer qu’elle pouvait accepter les deux types numériques comme arguments.

La syntaxe `Num` représente une **contrainte de classe**, et `a` est une **variable de type** dans la signature de la fonction.
La ligne suivante :

```haskell
triple :: Num a => a -> a
```

se lit ainsi :

> « Pour tout type `a` qui est une instance de la classe `Num`, la fonction `triple` a pour type `a -> a` ».

Tout type qui inclut une **contrainte de classe** est appelé un **type surchargé**, et donc notre fonction `triple` est une **fonction surchargée**.

---

Nous pouvons aussi spécifier une **variable de type sans contrainte de classe**, ce qui rend le type **complètement polymorphe** : cela signifie que **n'importe quel type** peut être utilisé comme argument.

C’est le cas, par exemple, pour certaines fonctions sur les **listes** que nous avons utilisées précédemment.
Leur résultat ne dépend pas du type des éléments dans la liste, elles doivent donc fonctionner **quel que soit ce type**.

Par exemple, les fonctions suivantes :

```haskell
head :: [a] -> a
-- une liste de type a retourne un élément de type a, quel que soit le type a pour cette liste

tail :: [a] -> [a]
-- une liste de type a retourne une liste de type a, quel que soit le type a pour cette liste
```

Ces fonctions sont **polymorphes** : elles ne dépendent pas d’un type particulier et s’adaptent à **tous les types d’éléments** présents dans les listes.

---

Souhaitez-vous un schéma ou une fiche de révision résumant les types polymorphes et surchargés ?


Nous avons déjà abordé les types **polymorphes** avec notre fonction `triple`, lorsque nous l’avons rendue compatible à la fois avec des entiers et des nombres à virgule flottante.
Nous avons utilisé `Num a` dans la **signature de type** de la fonction pour indiquer qu’elle pouvait accepter les deux types numériques comme arguments.

La syntaxe `Num` représente une **contrainte de classe**, et `a` est une **variable de type** dans la signature de la fonction.
La ligne suivante :

```haskell
triple :: Num a => a -> a
```

se lit ainsi :

> « Pour tout type `a` qui est une instance de la classe `Num`, la fonction `triple` a pour type `a -> a` ».

Tout type qui inclut une **contrainte de classe** est appelé un **type surchargé**, et donc notre fonction `triple` est une **fonction surchargée**.

---

Nous pouvons aussi spécifier une **variable de type sans contrainte de classe**, ce qui rend le type **complètement polymorphe** : cela signifie que **n'importe quel type** peut être utilisé comme argument.

C’est le cas, par exemple, pour certaines fonctions sur les **listes** que nous avons utilisées précédemment.
Leur résultat ne dépend pas du type des éléments dans la liste, elles doivent donc fonctionner **quel que soit ce type**.

Par exemple, les fonctions suivantes :

```haskell
head :: [a] -> a
-- une liste de type a retourne un élément de type a, quel que soit le type a pour cette liste

tail :: [a] -> [a]
-- une liste de type a retourne une liste de type a, quel que soit le type a pour cette liste
```

Ces fonctions sont **polymorphes** : elles ne dépendent pas d’un type particulier et s’adaptent à **tous les types d’éléments** présents dans les listes.

---

### 📌 Exemples de **types polymorphes**

Ces fonctions fonctionnent avec **n'importe quel type** sans contrainte particulière.

#### 1. `length`

```haskell
length :: [a] -> Int
```

* Prend une liste de n'importe quel type `a`
* Renvoie un entier représentant la taille de la liste

**Exemples d’utilisation :**

```haskell
length [1,2,3]       -- 3
length ["a", "b"]    -- 2
length [True, False] -- 2
```

#### 2. `fst`

```haskell
fst :: (a, b) -> a
```

* Prend un **couple** (paire) de n’importe quels types `a` et `b`
* Renvoie le **premier élément**

**Exemple :**

```haskell
fst (1, "hello") -- 1
fst (True, 9)    -- True
```

#### 3. `snd`

```haskell
snd :: (a, b) -> b
```

* Renvoie le **deuxième élément** d’un couple

**Exemple :**

```haskell
snd (1, "world") -- "world"
```

---

### 📌 Exemples de **types surchargés**

Ces fonctions exigent que le type appartienne à **une certaine classe de types** comme `Num`, `Eq`, ou `Ord`.

#### 1. `+` (addition)

```haskell
(+) :: Num a => a -> a -> a
```

* Fonctionne uniquement avec des types numériques (`Int`, `Float`, `Double`, etc.)

**Exemple :**

```haskell
3 + 4     -- 7
3.2 + 1.1 -- 4.3
```

#### 2. `==` (égalité)

```haskell
(==) :: Eq a => a -> a -> Bool
```

* Fonctionne avec n’importe quel type appartenant à la **classe Eq** (types comparables)

**Exemples :**

```haskell
5 == 5         -- True
"a" == "b"     -- False
[1,2] == [1,2] -- True
```

#### 3. `<` (inférieur à)

```haskell
(<) :: Ord a => a -> a -> Bool
```

* Fonctionne avec des types appartenant à **Ord** (types ordonnables)

**Exemples :**

```haskell
2 < 5         -- True
"apple" < "z" -- True
```

#### 4. `show`

```haskell
show :: Show a => a -> String
```

* Convertit n’importe quel type appartenant à **Show** en **chaîne de caractères**

**Exemples :**

```haskell
show 42        -- "42"
show True      -- "True"
show [1,2,3]   -- "[1,2,3]"
```

---
