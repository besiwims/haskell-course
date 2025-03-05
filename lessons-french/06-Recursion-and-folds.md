Recursion-and-folds

:opt no-lint

**Récursion et Folds**

**Plan**

- Pourquoi la récursion ?
- Penser de manière récursive
  - `sum` et `product`
- Étapes pour créer votre propre fonction récursive
- Exemples de récursion
  - `and`, `length`, `reverse`, `drop`, `take`, `map`, `filter`
- Extraction du motif `foldr`
- La fonction `foldl`
- La fonction `foldl'`
- Quand utiliser `foldr`, `foldl` et `foldl'`

---

## Pourquoi la récursion ?

L'une des fonctionnalités de base nécessaires dans tout langage de programmation est la répétition. Par exemple :

- Vous avez une liste d'objets et vous souhaitez effectuer une action sur chacun d'eux, un par un.
- Vous voulez effectuer des calculs 5 fois avec des valeurs différentes.

Dans les langages de programmation impératifs, ces tâches répétitives sont gérées à l'aide de boucles itératives. Par exemple, en JavaScript, on peut avoir :

```javascript
for (i = 0; i < 5; i = i + 1) {
    // Faire quelque chose
}

let i = 0;
while (i < 5) {
  // Faire quelque chose
  i = i + 1;
}
```

Mais, si nous essayions de créer quelque chose de similaire en Haskell, nous aurions un gros problème : la variable `i`.

Comme nous l'avons vu dans la première leçon, Haskell est un langage purement fonctionnel. Or, ces deux blocs de code reposent sur la modification de `i` à chaque itération. Cela signifie qu'ils ont l'effet de bord de modifier un état global au fur et à mesure de l'exécution du programme.

En Haskell, nous n'avons donc pas de fonctions de boucle intégrées. À la place, nous avons la récursion !

Pourquoi la récursion est-elle meilleure que les boucles ? Voici quelques raisons :

- Tout ce que vous pouvez faire avec des boucles peut être fait en utilisant la récursion. Et, en plus de cela, il existe même des programmes que vous pouvez définir récursivement mais qui ne peuvent pas être écrits en utilisant des boucles `for`.
- De nombreuses fonctions peuvent être naturellement définies en utilisant la récursion. Cela signifie que la manière dont vous pensez abstraitement à la fonction et la manière dont vous l'écrivez en récursion sont très similaires.
- Certaines fonctions sont plus claires et plus concises si elles sont définies en utilisant la récursion.
- Vous pouvez utiliser l'induction pour faire des raisonnements mathématiques et prouver des propriétés des fonctions définies par récursion.

Maintenant que vous savez que vous êtes sur le point d'apprendre un concept très puissant, plongeons dedans !

---

## Penser de manière récursive

La récursion se produit lorsqu'une chose est définie en termes d'elle-même. Une fonction récursive est donc une fonction définie en termes d'elle-même.

C'est tout. Le concept est très simple. C'est l'implémentation qui pose souvent problème.

Prenons un exemple en Python et en Haskell pour illustrer la différence entre la programmation impérative et la programmation fonctionnelle.

En Python, la fonction `sum` pourrait être définie comme ceci :

```python
def sum(liste):
    total = 0
    for i in liste:
        total = total + i
    return total
```

Ici, nous décrivons étape par étape ce que le programme doit faire.

En Haskell, nous définissons la même fonction de manière déclarative :

```haskell
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs
```

La fonction `sum` est définie en fonction d'elle-même, avec une condition de base qui permet d'arrêter la récursion.

Si nous appelons `sum [1,2,3,4,5]`, Haskell l'évalue de cette manière :

```haskell
sum [1,2,3,4,5] = 1 + sum [2,3,4,5]
                = 1 + 2 + sum [3,4,5]
                = 1 + 2 + 3 + sum [4,5]
                = 1 + 2 + 3 + 4 + sum [5]
                = 1 + 2 + 3 + 4 + 5 + sum []
                = 1 + 2 + 3 + 4 + 5 + 0
                = 15
```

La condition de base est essentielle, sinon la récursion ne s'arrêterait jamais.

---

## Étapes pour créer votre propre fonction récursive

1. Écrivez le type de la fonction.
2. Enumérez les cas possibles basés sur les entrées.
3. Identifiez et définissez les cas les plus simples (cas de base).
4. Pensez à ce que vous avez à disposition (paramètres, opérateurs, autres fonctions).
5. Définissez les autres cas.
6. Réfléchissez à la fonction : peut-elle être simplifiée ou généralisée ?

---

Ce n'est qu'un aperçu du sujet. En pratiquant, vous gagnerez en intuition et deviendrez rapidement à l'aise avec la récursion en Haskell ! 🚀

Here is the translation of your text to French:

---

**length'** : Une fonction qui donne la longueur d'une liste  
Pour calculer la longueur d'une liste, nous devons prendre une liste et retourner un entier. Et parce qu'en principe, nous n'opérons pas sur les éléments de la liste, nous pouvons utiliser un type polymorphique comme ceci :

```haskell
length' :: [a] -> Int
```

Maintenant, parce que cela prend une liste, nous allons définir les cas standard pour les listes :

```haskell
length' :: [a] -> Int
length' []     =
length' (x:xs) =
```

En cherchant des cas simples, nous pouvons identifier que la longueur d'une liste vide est, bien sûr, 0 éléments. Nous remplaçons donc cela :

```haskell
length' :: [a] -> Int
length' []     = 0
length' (x:xs) =
```

Et maintenant, nous pouvons calculer la longueur d'une liste en ajoutant 1 pour chaque élément de la liste, n'est-ce pas ? Et parce que nous avons le premier élément (x) isolé par correspondance de motif, nous pouvons ajouter 1 pour cela et calculer récursivement la longueur du reste de la liste (xs) :

```haskell
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs
```

Cela pourrait être la fonction finale. Mais comme nous n'utilisons pas réellement x, nous pouvons l'ignorer dans notre motif :

```haskell
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs
```

```haskell
length' [1,2,3,4,5]
length' ['a'..'z']
5
26
```

Et c'est notre définition finale.

---

**reverse'** : Une fonction qui inverse une liste.  
Pour inverser une liste, nous prenons une liste d'éléments et retournons une liste d'éléments. Et parce qu'en principe, nous n'opérons pas sur les éléments de la liste, nous pouvons utiliser un type polymorphique comme ceci :

```haskell
reverse' :: [a] -> [a]
```

Maintenant, parce que cela prend une liste, nous allons définir les cas standards pour les listes :

```haskell
reverse' :: [a] -> [a]
reverse' []     =
reverse' (x:xs) =
```

L'inverse de la liste vide est simplement la liste vide. Donc c'est le cas facile. Et c'est aussi un cas de base car ce n'est pas récursif :

```haskell
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) =
```

Et maintenant, si nous prenons le premier élément, le mettons à la fin, et faisons cela jusqu'à ce que nous atteignions la fin de la liste initiale, elle sera inversée ! Donc, nous devons juste prendre x, le mettre à la fin, et faire la même chose récursivement jusqu'à ce que nous manquions d'éléments, ce qui est notre cas de base :

```haskell
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]
```

```haskell
reverse' [1,2,3,4,5]
reverse' "stressed" -- Quel est l'inverse de "stressed" ?
[5,4,3,2,1]
"desserts"
```

Ok. Nous avons vu assez d'exemples simples. Passons à quelque chose d'un peu plus compliqué :

---

**drop'** : Supprimer les n premiers éléments d'une liste  
Donc cela prend un entier et une liste et retourne une liste. Et parce qu'en principe, nous n'opérons pas sur les éléments de la liste, nous pouvons utiliser un type polymorphique comme ceci :

```haskell
drop' :: Int -> [a] -> [a]
```

Ok ! C'est nouveau ! Nous avons maintenant deux arguments différents à prendre en compte.

La manière de faire cela est de présenter toutes les combinaisons possibles de motifs standard. Parce que nous avons des nombres, nous prenons initialement en compte le motif pour 0 et tout autre nombre. Et parce que nous avons des listes, nous devons prendre en compte le motif pour les listes vides et non vides.

Donc, nous avons :

```haskell
drop' :: Int -> [a] -> [a]
drop' 0 []     =
drop' 0 (x:xs) =
drop' n []     =
drop' n (x:xs) =
```

Comme vous pouvez le voir, il y a plus de choses à prendre en compte. Mais ce n'est pas nécessairement plus difficile. Pensons à chaque cas individuellement.

Si nous supprimons 0 éléments d'une liste vide, il est logique que le résultat soit une liste vide.
Si nous supprimons 0 éléments d'une liste non vide, nous retournons exactement la même liste.
Si nous supprimons n éléments d'une liste vide, nous pouvons retourner une erreur ou une liste vide. Nous choisissons de retourner la liste vide.
En remplaçant cela dans les définitions :

```haskell
drop' :: Int -> [a] -> [a]
drop' 0 []     = []
drop' 0 (x:xs) = x:xs
drop' n []     = []
drop' n (x:xs) =
```

Là, nous avons complété 3 des 4 cas. Maintenant, qu'en est-il de supprimer n éléments d'une liste non vide ?

Nous avons déjà le premier élément séparé de la liste. Donc si nous supprimons cela, il reste un élément à supprimer. Mais si nous faisons simplement quelque chose comme `drop n xs`, la fonction continuera à supprimer des éléments jusqu'à ce que la liste soit vide.

Heureusement, il existe une solution simple. Si nous appelons récursivement `drop'` avec `xs`, nous supprimerons un élément à chaque appel récursif. Nous pouvons donc soustraire 1 à n à chaque appel pour le synchroniser. Ainsi, si nous avons plus de n éléments, nous arrêterons la récursion lorsque nous atteindrons `n = 0` :

```haskell
drop' :: Int -> [a] -> [a]
drop' 0 []     = []
drop' 0 (x:xs) = x:xs
drop' n []     = []
drop' n (x:xs) = drop' (n - 1) xs
```

Ok. Nous avons une fonction qui fonctionne. Mais il y a quelques améliorations à apporter :

Les deux cas qui prennent une liste vide retournent une liste vide. Nous pouvons donc ignorer l'Int dans ces cas.
Dans le deuxième cas, nous passons simplement l'entrée, donc il n'y a pas besoin de correspondance de motifs.
Nous n'utilisons pas x dans la définition récursive, donc nous pouvons également l'ignorer.
En apportant ces modifications, nous obtenons :

```haskell
drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs
drop' n (_:xs) = drop' (n - 1) xs
```

Cela semble être notre définition finale pour `drop`. Mais est-ce vraiment le cas ? Que se passe-t-il si `n < 0` ? Théoriquement, cela n'a aucun sens. Mais en pratique, quelqu'un pourrait être assez fou pour essayer !

Dans ce cas, notre fonction actuelle continuera à supprimer des éléments un par un jusqu'à ce qu'elle s'épuise, car nous n'atteindrons jamais `n = 0`.

Cela pourrait être une manière de gérer ce cas. Mais intuitivement, vous penseriez que supprimer un nombre négatif d'éléments ferait la même chose que supprimer zéro élément.

Nous devons donc ajuster notre définition pour prendre cela en compte. Et pour le faire, nous pouvons modifier le cas qui gère `n == 0` pour gérer `n <= 0` en liant le nombre à la variable `n` et en utilisant des gardes pour vérifier la propriété désirée.

Comme ceci :

```haskell
drop' :: Int -> [a] -> [a]
drop' _ []           = []
drop' n xs | n <= 0  = xs
drop' n (_:xs)       = drop' (n - 1) xs
```

```haskell
drop' (-3) [1,2,3]
```

--- 

length': A function that gives you the length of a list
To calculate the length of a list, we have to take a list and return an integer. And because, in principle, we won't operate on the elements of the list, we can use a polymorphic type like this:

length' :: [a] -> Int
Now, because it takes a list, we'll define the standard cases for lists:

length' :: [a] -> Int
length' []     =
length' (x:xs) =
Now, looking for easy cases, we can identify that the length of an empty list is, of course, 0 elements. So we replace that:

length' :: [a] -> Int
length' []     = 0
length' (x:xs) =
And now, we can calculate the length of a list if we add 1 for each element of the list, right? And because we have the first element (x) singled out by pattern matching, we can add 1 for it and recursively calculate the length of the rest of the list (xs):

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs
That one could be the final function. But because we don't actually use x, we can ignore it in our pattern:

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

length' [1,2,3,4,5]
length' ['a'..'z']
5
26
And that's our final definition.

reverse': A function that reverses a list.
To reverse a list, we take a list of elements and return a list of elements. And because, in principle, we won't operate on the elements of the list, we can use a polymorphic type like this:

reverse' :: [a] -> [a]
Now, because it takes a list, we'll define the standard cases for lists:

reverse' :: [a] -> [a]
reverse' []     =
reverse' (x:xs) =
The reverse of the empty list it's just the empty list. So that's the easy one. And it's also a base case because it's not recursive:

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) =
And now, if we take the first element, put it at the end, and keep doing that until we reach the end of the initial list, it will be reversed! So, we just need to take x, put it at the end, and do the same recursively until we run out of elements, which is our base case:

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse' [1,2,3,4,5]
reverse' "stressed" -- What's the reverse of stressed?
[5,4,3,2,1]
"desserts"
Ok. We saw enough easy examples. Now let's do something a little bit more complicated:

drop': Remove the first n elements from a list
So it takes an integer and a list and returns a list. And because, in principle, we won't operate on the elements of the list, we can use a polymorphic type like this:

drop' :: Int -> [a] -> [a]
OK! This is new! We have two different arguments to take into account now.

The way to do this is to present all possible standard pattern combinations. Because we have numbers, we initially take into account the pattern for 0 and any other number. And because we have lists, we have to take into account the pattern for empty and non-empty lists.

So, we have:

drop' :: Int -> [a] -> [a]
drop' 0 []     =
drop' 0 (x:xs) =
drop' n []     =
drop' n (x:xs) =
As you can see, there're more things to take into account. But it isn't necessarily more difficult. Let's think about each case individually.

If we drop 0 elements from an empty list, it makes sense that the result would be an empty list.
If we drop 0 elements from a non-empty list, we return the same exact list.
If we drop n elements from an empty list, we can return an error or an empty list. We choose to return the empty list.
Replacing that in the definitions:

drop' :: Int -> [a] -> [a]
drop' 0 []     = []
drop' 0 (x:xs) = x:xs
drop' n []     = []
drop' n (x:xs) =
There you go. We completed 3 of the 4 cases. Now, what about when we want to drop n number of elements from a non-empty list?

We already have the first element separated from the list. So if we remove that one is one less element to remove. But if we just do something like drop n xs, the function will keep dropping elements until the list is empty.

Luckily, there's an easy solution. If we recursively call drop' with xs, we'd be dropping one element on each recursive call. So we can subtract 1 from n on each call to keep it synced. That way, if there are more than n elements, we'll stop the recursion when we reach n = 0:

drop' :: Int -> [a] -> [a]
drop' 0 []     = []
drop' 0 (x:xs) = x:xs
drop' n []     = []
drop' n (x:xs) = drop' (n - 1) xs
Ok. We have a working function. But there are a few things to be improved:

Both cases that take an empty list return an empty list. So we can ignore the Int in those cases.
In the second case, we just pass through the input, so there's no need for pattern matching.
We don't use x in the recursive definition, so we can also ignore it.
Doing those changes, we get:

drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs
drop' n (_:xs) = drop' (n - 1) xs
It looks like we arrived at our final drop definition. But did we really? What happens if n < 0? Theoretically, it doesn't make any sense. But in practice, someone could be crazy enough to try it!

In that case, our current function will keep dropping elements one by one until it runs out because we'll never get to n = 0.

That could be a way to handle that case. But intuitively, you'd think that dropping a negative number of elements would do the same as dropping zero elements.

So we have to adjust our definition to accommodate that. And to do it, we can change the the case that handles n == 0 to handle n <= 0 by binding the number to the variable n and using guards to check for the desired property.

Like this:

drop' :: Int -> [a] -> [a]
drop' _ []           = []
drop' n xs | n <= 0  = xs
drop' n (_:xs)       = drop' (n - 1) xs


drop' (-3) [1,2,3]

yesYouDo :: String -> String
yesYouDo = ("Ok, I do"++) . drop' 7

yesYouDo "I don't like chocolate."
yesYouDo "I don't like to write silly examples."
[1,2,3]
"Ok, I do like chocolate."
"Ok, I do like to write silly examples."
And now the function does work as intended!

take': Take (and return) the first n elements from a list
This function is oddly similar to drop'. It takes an integer and a list and returns a list. But this time, the list contains all the elements from the first one until n. Because we just saw a similar case, we'll do the first and second steps together:

take' :: Int -> [a] -> [a]
take' 0 []     =
take' 0 (x:xs) =
take' n []     =
take' n (x:xs) =
Same as before, let's think about each case individually:

If we take 0 elements from an empty list, it makes sense that the result would be an empty list.
If we take 0 elements from a non-empty list, we take nothing, so we return an empty list.
If we take n elements from an empty list, we can return an error or an empty list. We choose to return the empty list.
So, replacing that:

take' :: Int -> [a] -> [a]
take' 0 []     = []
take' 0 (x:xs) = []
take' n []     = []
take' n (x:xs) =
Voici la traduction en français :  

---

**length' : Une fonction qui vous donne la longueur d'une liste**  
Pour calculer la longueur d'une liste, nous devons prendre une liste et renvoyer un entier. Et comme, en principe, nous n'allons pas opérer sur les éléments de la liste, nous pouvons utiliser un type polymorphe comme ceci :  

```haskell
length' :: [a] -> Int
```

Maintenant, comme cette fonction prend une liste, nous allons définir les cas standards pour les listes :  

```haskell
length' :: [a] -> Int
length' []     =
length' (x:xs) =
```

En cherchant les cas simples, nous pouvons identifier que la longueur d'une liste vide est, bien sûr, 0 éléments. Nous remplaçons donc cela :  

```haskell
length' :: [a] -> Int
length' []     = 0
length' (x:xs) =
```

Et maintenant, nous pouvons calculer la longueur d'une liste si nous ajoutons 1 pour chaque élément de la liste, n'est-ce pas ? Et comme nous avons isolé le premier élément (`x`) grâce au pattern matching, nous pouvons ajouter 1 pour celui-ci et calculer récursivement la longueur du reste de la liste (`xs`) :  

```haskell
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs
```

Celle-ci pourrait être notre fonction finale. Mais comme nous n'utilisons pas réellement `x`, nous pouvons l'ignorer dans notre pattern :  

```haskell
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs
```

```haskell
length' [1,2,3,4,5]
length' ['a'..'z']
```

Sortie :  
```haskell
5
26
```

Et voilà notre définition finale.  

---

**reverse' : Une fonction qui inverse une liste.**  
Pour inverser une liste, nous prenons une liste d'éléments et nous renvoyons une liste d'éléments. Et comme, en principe, nous n'allons pas opérer sur les éléments de la liste, nous pouvons utiliser un type polymorphe comme ceci :  

```haskell
reverse' :: [a] -> [a]
```

Maintenant, comme cette fonction prend une liste, nous allons définir les cas standards pour les listes :  

```haskell
reverse' :: [a] -> [a]
reverse' []     =
reverse' (x:xs) =
```

L'inverse d'une liste vide est simplement une liste vide. C'est donc le cas le plus simple. Et c'est aussi un cas de base, car il n'est pas récursif :  

```haskell
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) =
```

Et maintenant, si nous prenons le premier élément, le mettons à la fin et continuons ainsi jusqu'à atteindre la fin de la liste initiale, elle sera inversée ! Nous devons donc simplement prendre `x`, le mettre à la fin et faire la même chose récursivement jusqu'à ce que nous n'ayons plus d'éléments, ce qui correspond à notre cas de base :  

```haskell
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]
```

```haskell
reverse' [1,2,3,4,5]
reverse' "stressed" -- Quel est l'inverse de "stressed" ?
```

Sortie :  
```haskell
[5,4,3,2,1]
"desserts"
```

Ok. Nous avons vu suffisamment d'exemples simples. Maintenant, faisons quelque chose d'un peu plus compliqué.  

---

**drop' : Supprime les n premiers éléments d'une liste**  
Cette fonction prend un entier et une liste et retourne une liste. Et comme, en principe, nous n'allons pas opérer sur les éléments de la liste, nous pouvons utiliser un type polymorphe comme ceci :  

```haskell
drop' :: Int -> [a] -> [a]
```

OK ! C'est nouveau ! Nous avons maintenant deux arguments différents à prendre en compte.  

La manière de procéder est de présenter toutes les combinaisons de motifs standard possibles. Comme nous avons des nombres, nous devons initialement prendre en compte le motif pour `0` et tout autre nombre. Et comme nous avons des listes, nous devons prendre en compte les motifs pour les listes vides et non vides.  

Nous avons donc :  

```haskell
drop' :: Int -> [a] -> [a]
drop' 0 []     =
drop' 0 (x:xs) =
drop' n []     =
drop' n (x:xs) =
```

Comme vous pouvez le voir, il y a plusieurs cas à considérer. Mais ce n'est pas nécessairement plus difficile. Pensons à chaque cas individuellement :  

- Si nous supprimons 0 éléments d'une liste vide, il est logique que le résultat soit une liste vide.  
- Si nous supprimons 0 éléments d'une liste non vide, nous retournons exactement la même liste.  
- Si nous supprimons `n` éléments d'une liste vide, nous pouvons soit retourner une erreur, soit une liste vide. Nous choisissons de retourner la liste vide.  

En remplaçant cela dans les définitions :  

```haskell
drop' :: Int -> [a] -> [a]
drop' 0 []     = []
drop' 0 (x:xs) = x:xs
drop' n []     = []
drop' n (x:xs) =
```

Nous avons complété 3 des 4 cas. Maintenant, que faire lorsque nous voulons supprimer `n` éléments d'une liste non vide ?  

Nous avons déjà le premier élément séparé de la liste. Donc si nous supprimons celui-là, il nous reste `n - 1` éléments à supprimer. Mais si nous faisons simplement `drop' n xs`, la fonction continuera de supprimer des éléments jusqu'à ce que la liste soit vide.  

Heureusement, il existe une solution simple. Si nous appelons récursivement `drop'` sur `xs`, nous supprimerons un élément à chaque appel récursif. Nous pouvons donc soustraire 1 de `n` à chaque appel pour maintenir la synchronisation. De cette façon, si la liste contient plus de `n` éléments, nous arrêterons la récursion lorsque `n = 0` :  

```haskell
drop' :: Int -> [a] -> [a]
drop' 0 []     = []
drop' 0 xs     = xs
drop' n []     = []
drop' n (_:xs) = drop' (n - 1) xs
```

Mais que se passe-t-il si `n < 0` ? Théoriquement, cela n'a pas de sens. Mais en pratique, quelqu'un pourrait essayer !  

Dans ce cas, notre fonction actuelle continuerait à supprimer des éléments un par un jusqu'à ce que la liste soit vide.  

Une meilleure solution serait d'interpréter `drop' (-n)` comme `drop' 0`. Nous pouvons ajuster notre définition avec des gardes (`|`) pour gérer ce cas :  

```haskell
drop' :: Int -> [a] -> [a]
drop' _ []           = []
drop' n xs | n <= 0  = xs
drop' n (_:xs)       = drop' (n - 1) xs
```

---

**take' : Prendre (et retourner) les n premiers éléments d'une liste**  
Cette fonction est étrangement similaire à `drop'`. Elle prend un entier et une liste et retourne une liste, mais cette fois, elle contient tous les éléments du premier jusqu'à `n`.  

```haskell
take' :: Int -> [a] -> [a]
take' 0 []     =
take' 0 (x:xs) =
take' n []     =
take' n (x:xs) =
```

Nous appliquons la même logique que précédemment pour compléter cette fonction.

Voici la traduction en français :

---

Nous pouvons les préfixer à une nouvelle liste qui deviendra récursivement plus grande jusqu'à ce que nous atteignions soit `n = 0`, soit que nous manquions d'éléments dans la liste :

```haskell
take' :: Int -> [a] -> [a]
take' 0 []     = []
take' 0 (x:xs) = []
take' n []     = []
take' n (x:xs) = x : take' (n-1) xs
```

Maintenant, nous pouvons simplifier l'expression :

- Si `n = 0`, nous ne nous soucions pas de la liste. Nous retournerons une liste vide de toute façon.
- Si la liste est vide, nous ne nous soucions pas du nombre. Nous retournerons une liste vide de toute façon.

Traduit en code :

```haskell
take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs
```

Nous avons le même problème qu'avec `drop`. Intuitivement, prendre un nombre négatif d'éléments devrait avoir le même effet que prendre zéro élément. Cela ne devrait pas renvoyer la liste entière.

Heureusement, nous savons déjà comment résoudre ce problème, de la même manière que pour la définition de `drop` :

```haskell
take' :: Int -> [a] -> [a]
take' n _      | n <= 0 = []
take' _ []              = []
take' n (x:xs)          = x : take' (n-1) xs
```

Exemples :

```haskell
take' 3 [1,2,3,4,5]  -- [1,2,3]
take' (-3) [1,2,3,4,5] -- []
```

### `map'` : Une fonction d'ordre supérieur qui applique une fonction à chaque élément d'une liste

Comme toujours, commençons par le type. Nous aurons une fonction et une liste en entrée, et nous retournerons une liste. Comme nous ne savons pas à l'avance quelle fonction sera passée en argument, nous utilisons des variables de type polymorphiques :

```haskell
map' :: (a -> b) -> [a] -> [b]
```

Décomposons les cas possibles :

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs
```

Exemples :

```haskell
map' (+1) [1,2,3,4] -- [2,3,4,5]
map' (++"!") ["Hey","Ho","Let's go"] -- ["Hey!","Ho!","Let's go!"]
```

C'est une fonction extrêmement utile que vous utiliserez très souvent !

### `filter'` : Filtrer les éléments d'une liste qui ne satisfont pas un prédicat

Cette fonction prend un prédicat et une liste, et retourne une liste ne contenant que les éléments qui satisfont ce prédicat.

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs
```

Exemples :

```haskell
filter' (==True) [True,False,True,True,False] -- [True,True,True]
filter' ('!' `elem`) ["Hey!", "How are you?"] -- ["Hey!"]
filter' (\x -> x**2 < 37) [1,2,3,4,5,6,7,8,9,10] -- [1.0,2.0,3.0,4.0,5.0,6.0]
```

Et voilà ! Vous pouvez filtrer comme bon vous semble.

---

### Extraction du motif `foldr`

Regardons ces fonctions définies précédemment :

```haskell
sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: [Int] -> Int
product' []     = 1
product' (x:xs) = x * product' xs

and' :: [Bool] -> Bool
and' []     =  True
and' (x:xs) =  x && and' xs
```

Un motif se répète dans chacune de ces fonctions :

1. Il y a un cas de base pour une liste vide qui retourne une valeur non récursive.
2. Il y a un cas récursif où l'on prend le premier élément de la liste et on applique une fonction pour le combiner avec un appel récursif traitant le reste de la liste.

Nous allons appeler cette abstraction `foldr` :

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v [] =  v
foldr f v (x:xs) = f x (foldr f v xs)
```

Nous pouvons maintenant réécrire nos fonctions en utilisant `foldr` :

```haskell
sum' :: [Int] -> Int
sum' = foldr (+) 0

product' :: [Int] -> Int
product' = foldr (*) 1

and' :: [Bool] -> Bool
and' = foldr (&&) True
```

Un autre exemple : la fonction `length'` :

```haskell
length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0
```

Et pour `reverse'` :

```haskell
reverse' :: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []
```

Cependant, cette implémentation est inefficace pour de grandes listes à cause de `++`. Nous devons utiliser `foldl` :

---

### `foldl` : Une autre approche du repliement

Contrairement à `foldr`, `foldl` traite la liste de gauche à droite :

```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ v [] = v
foldl f v (x:xs) = foldl f (f v x) xs
```

Cela nous permet d'écrire une version plus efficace de `reverse` :

```haskell
reverse'' :: [a] -> [a] 
reverse'' = foldl (flip (:)) []
```

L'exécution est bien plus rapide :

```haskell
sum . reverse' $ [1..10000]  -- Lent
sum . reverse'' $ [1..10000] -- Rapide
```

---

### `foldl'` : Une version optimisée de `foldl`

L'utilisation de `foldl` peut provoquer un débordement de pile pour de très grandes listes. `foldl'` (défini dans `Data.List`) résout ce problème en évaluant strictement l'accumulateur à chaque étape :

```haskell
import Data.List (foldl')

foldl' (+) 0 [1,2,3,4,5] -- Évaluation immédiate, évite la pile d'appels
```

---

Et voilà ! Une introduction complète aux fonctions récursives, à `map`, `filter`, et aux fonctions de repliement (`foldr`, `foldl` et `foldl'`). 

Vous avez maintenant une base solide pour programmer efficacement en Haskell ! 🚀
