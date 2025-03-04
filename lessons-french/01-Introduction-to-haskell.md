01-Introduction-to-haskell

Voici la traduction en français :

---

# Introduction à Haskell

## Plan
- Qu'est-ce que Haskell ?
- Langages de programmation fonctionnels
- Composition de fonctions
- Effets explicites (Pure)
- Syntaxe de base
- Indentation et commentaires
- Définition et utilisation de fonctions
- Système de types de Haskell
- Évaluation paresseuse (Laziness)
- Outils : GHC (GHCi), Cabal, Stack
- Comment utiliser JupyterLab

Chaque leçon est un notebook Jupyter.  
Chaque notebook Jupyter est une série de cellules.  
Pour exécuter une cellule, cliquez sur ⇧⏎ (Shift + Entrée).  
Vous pouvez expérimenter avec le code dans les cellules.  
Une fois l’onglet fermé, toutes les modifications seront perdues.

## Qu'est-ce que Haskell ?
Nous passerons en revue chaque propriété de Haskell individuellement et répondrons à cette question à la fin du cours.

---

## Langages de programmation fonctionnels
Haskell est un langage de programmation fonctionnel.

Dans les langages de programmation impératifs, les définitions de fonctions sont une séquence d'instructions impératives.

Dans les langages de programmation fonctionnels, les définitions de fonctions sont des arbres d'expressions qui transforment des valeurs en d'autres valeurs.

Les programmes sont construits en appliquant et en composant des fonctions.

---

## Composition de fonctions
La composition de fonctions consiste à acheminer le résultat d'une fonction vers l'entrée d'une autre, créant ainsi une nouvelle fonction.

Comme en mathématiques, le résultat de chaque fonction est transmis en argument à la suivante, et le résultat de la dernière fonction devient le résultat global.

Par exemple, supposons que nous ayons deux fonctions `f` et `g` :

- Composer ces fonctions signifie d'abord calculer `f(x)` pour obtenir `y`, puis utiliser `y` comme argument pour `g(y)` afin d'obtenir `z`.

Nous pouvons créer une fonction qui va de `x` à `z` :

```haskell
h = g . f
```

Ainsi, nous pouvons construire des fonctions complexes en composant des fonctions simples.

Exemple :

- Une fonction qui prend un fichier Excel et retourne la liste des joueurs qu'il contient.
- Une fonction qui prend une liste de joueurs et la trie par score.
- Une fonction qui prend une liste de joueurs et retourne les trois meilleurs.

En les composant, nous obtenons une seule fonction qui prend un fichier Excel et renvoie les trois meilleurs joueurs.

---

## Effets explicites (Programmation pure)
Les langages purement fonctionnels considèrent tous les calculs comme l'évaluation de fonctions mathématiques.

En mathématiques, une expression comme `y = f(x)` signifie que `y` est une fonction de `x`.

Pour une valeur donnée de `x`, `y` aura toujours la même valeur.

Peu importe le contexte, `y` dépendra uniquement de `x`.

En programmation fonctionnelle pure, les fonctions dépendent uniquement de leurs arguments et n'interagissent pas avec un état global ou local (ce qui signifie qu'elles n'ont pas d'effets de bord).

Ainsi, une fonction pure renverra toujours le même résultat pour une même entrée.

Cela apporte plusieurs avantages :

- Vérification et preuve de la correction du code plus faciles.
- Possibilité de remplacer une expression par son équivalent sans changer le résultat (comme en algèbre).
- Moins d'erreurs potentielles.
- Exécution plus simple en parallèle ou en concurrence.

Haskell est un langage purement fonctionnel, mais il permet d'interagir avec le monde extérieur (I/O, communication réseau, etc.) en marquant explicitement ces effets dans le système de types. Nous verrons comment cela fonctionne dans les prochaines leçons.

---

## Syntaxe de base

### Commentaires
```haskell
-- Utilisez deux tirets pour commenter une seule ligne.

{-
Utilisez des accolades avec un tiret
  pour ouvrir et fermer
des commentaires multi-lignes.
-}
```

### Indentation
Haskell est sensible à l'indentation. Cela signifie que les espaces, tabulations et sauts de ligne sont importants.

Règle d'or : le code qui fait partie d'une expression doit être plus indenté que le début de cette expression.

Nous verrons des exemples plus tard.

---

## Définition des fonctions
Haskell étant un langage fonctionnel, vous écrirez beaucoup de fonctions. Voici comment en définir une :

```haskell
greaterThan18 x = x > 18
```

Explication :
- `greaterThan18` est le nom de la fonction.
- `x` est un paramètre.
- `=` assigne l'expression `x > 18` au nom `greaterThan18`.

### Utilisation d'une fonction
```haskell
greaterThan18 30  -- Renvoie True
```

Autres exemples :

```haskell
-- Une fonction qui additionne 6 nombres
add6numbers u v w x y z = u + v + w + x + y + z
add6numbers 1 2 3 4 5 6  -- 21

-- Volume d'un cylindre
volumeOfACylinder r h = pi * r^2 * h
volumeOfACylinder 3 10  -- Résultat en unités cubiques

-- Conversion Fahrenheit → Celsius
fToC x = (x - 32) * 5 / 9
fToC 212  -- 100 (Celsius)
```

Points clés :
- Les paramètres sont séparés par des espaces.
- Tout ce qui suit `=` est le corps de la fonction.
- Les noms de fonction doivent commencer par une minuscule.
- L'utilisation de parenthèses permet de prioriser les calculs.

---

## Système de types de Haskell
Nous approfondirons ce sujet dans la leçon 2.

Les types définissent les valeurs qu'une variable ou une expression peut prendre.

Exemples :
- `32`, `9999695939294`, `0.5` sont des nombres.
- `6A3` n'est pas un nombre et causera une erreur.

Le processus qui vérifie ces contraintes est appelé **vérification des types**.

### Langages dynamiquement vs statiquement typés
- **Dynamique** (JavaScript, Python) : les types sont vérifiés à l'exécution.
- **Statique** (Java, C, Haskell) : les types sont vérifiés avant l'exécution.

Haskell est statiquement typé et chaque expression a un type. Heureusement, Haskell peut **inférer** les types automatiquement.

---

## Évaluation paresseuse (Laziness)
Haskell est un langage paresseux : il n’évalue pas les expressions tant que leur résultat n'est pas nécessaire.

Exemples :

```haskell
giveMe x = take x [1..]  -- Liste infinie de nombres naturels
giveMe 7  -- [1,2,3,4,5,6,7]
```

Haskell évite les calculs inutiles :

```haskell
cheapComputation = 7 
expensiveComputation = sum [1..10000000]  -- Somme d'une liste très longue
if cheapComputation > 5 || expensiveComputation > 5 then "Terminé" else "Jamais affiché"
```

Ici, `expensiveComputation` n’est jamais calculé car `cheapComputation > 5` est `True`.

---

## Outils

### GHC et GHCi
GHC (Glasgow Haskell Compiler) est le compilateur de Haskell. Il permet :
- De compiler et exécuter des programmes Haskell.
- D’exécuter des expressions interactivement via GHCi.

Pour utiliser GHCi dans un terminal :
```sh
ghci
:l monFichier.hs  # Charger un fichier
:q                # Quitter
```

### Cabal et Stack
Ce sont des outils pour gérer les dépendances et les projets Haskell. Nous utiliserons **Cabal** dans ce cours.

---

## Conclusion : Qu'est-ce que Haskell ?
Haskell est un langage **statiquement typé**, **paresseux**, **fonctionnel**, avec **effets explicites**, et des fonctions élégantes comme :

```haskell
volumeOfACylinder r h = pi * r^2 * h
```

Nous verrons plus de concepts avancés dans les prochaines leçons ! 🚀
