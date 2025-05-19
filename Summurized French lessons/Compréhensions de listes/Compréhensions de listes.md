# **Compréhensions de listes**

Dans ce chapitre, nous introduisons les **compréhensions de listes**, qui sont utilisées pour créer de **nouvelles listes à partir de listes existantes**. Le terme provient des mathématiques, où la **compréhension d’ensemble** est utilisée pour décrire un ensemble en énumérant ses éléments et/ou en énonçant des conditions que ses membres doivent satisfaire.

En Haskell, la syntaxe est la suivante :

```haskell
[ <GÉNÉRATEUR> | <ÉLÉMENT> <- <LISTE>, <GARDE> ]
```

* Le **GÉNÉRATEUR** est une expression qui précise comment les éléments de la nouvelle liste doivent être calculés.
* L’**ÉLÉMENT** est un élément extrait de la **LISTE** existante.
* La **GARDE** est une condition optionnelle que l’ÉLÉMENT doit satisfaire pour être inclus dans la nouvelle liste.

La ligne complète peut se lire ainsi :
**« Crée une liste en appliquant le GÉNÉRATEUR à chaque ÉLÉMENT de la LISTE qui satisfait la condition de GARDE. »**

---

### **Exemple simple :**

Créons une liste de **nombres pairs** à partir de la liste `[1..10]` :

```haskell
ghci> [x | x <- [1..10], even x]
[2,4,6,8,10]
```

Cela se lit comme :
**« Crée une liste de tous les nombres `x` tels que `x` est un élément de la liste `[1..10]` et `x` est pair. »**

---

### **Avec transformation (générateur actif) :**

On peut également appliquer une fonction à `x` :

```haskell
ghci> [x * 2 | x <- [1..10], even x]
[4,8,12,16,20]
```

Ici, chaque `x` pair est multiplié par 2 avant d’être inclus dans la liste.

---

### **Sans utiliser x dans le générateur :**

On peut ignorer `x` dans le résultat, tout en l’utilisant dans la garde :

```haskell
ghci> ["even!" | x <- [1..10], even x]
["even!","even!","even!","even!","even!"]
```

Ici, on ajoute la chaîne `"even!"` pour chaque nombre pair de `[1..10]`.

---

### **Compréhensions avec plusieurs listes et gardes :**

On peut aussi utiliser **plusieurs listes** et **plusieurs conditions** dans une compréhension :

```haskell
ghci> [ (x, y) | x <- [1..3], y <- ['a'..'c'] ]
[
    (1,'a'), (1,'b'), (1,'c'),
    (2,'a'), (2,'b'), (2,'c'),
    (3,'a'), (3,'b'), (3,'c')
]
```

Ici, la liste `['a'..'c']` est parcourue **trois fois**, une fois pour chaque élément de `[1..3]`. Cela génère toutes les combinaisons possibles de paires `(x, y)`.

---
