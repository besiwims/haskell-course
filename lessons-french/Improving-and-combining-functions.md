Improving-and-combining-functions

Amélioration et combinaison de fonctions  
**Plan**  
- Fonctions d'ordre supérieur  
- `filter`  
- `any`  
- Fonctions lambda  
- Priorité et associativité  
- Fonctions curryfiées  
- Application partielle  
- Application et composition de fonctions  
- L'opérateur `$`  
- L'opérateur `.`  
- Style sans point (point-free)  

## Fonctions d'ordre supérieur  
Une fonction d'ordre supérieur est une fonction qui prend d'autres fonctions en paramètre ou qui retourne une fonction en résultat.  

Puisque nous pouvons passer des fonctions en entrée, les retourner en sortie et les assigner à des variables, elles sont considérées comme des valeurs à part entière. Nous disons donc que les fonctions sont des citoyens de première classe.  

Prenons un exemple classique. Imaginons que vous ayez une fonction que vous appliquez généralement deux fois :  

```haskell
complexFunc1 :: Int -> Int
complexFunc1 x = x + 1

func1 :: Int -> Int
func1 x = complexFunc1 (complexFunc1 x)

complexFunc2 :: Int -> Int
complexFunc2 x = x + 2

func2 :: Int -> Int
func2 x = (complexFunc2 (complexFunc2 x)) + (complexFunc2 (complexFunc2 x))
```

C'est un exemple exagéré, mais on peut voir émerger un schéma : vous utilisez toujours `complexFunc1` et `complexFunc2` deux fois !  

Dès que nous identifions ce motif, nous réalisons que nous pouvons améliorer cela. Et si nous créions une fonction qui prend en paramètre une fonction et une valeur, puis applique la fonction à cette valeur deux fois ?  

Nous pouvons le faire avec :  

```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
```

Ici, la signature de type est différente des précédentes. La partie `(a -> a)` indique que le premier paramètre est une fonction prenant une valeur de type `a` et retournant une valeur du même type. Le second paramètre est une valeur de type `a`, et la fonction `applyTwice` retourne aussi une valeur de type `a`.  

Dans le corps de la fonction, nous appliquons `f` à `x`, puis nous appliquons encore `f` au résultat. Nous appliquons donc `f` deux fois.  

Et voilà ! Nous avons créé une fonction d'ordre supérieur !  

Nous pouvons maintenant simplifier notre code précédent :  

```haskell
func1' :: Int -> Int
func1' x = applyTwice complexFunc1 x

func2' :: Int -> Int
func2' x = (applyTwice complexFunc2 x) + (applyTwice complexFunc2 x)
```

Ceci est un exemple simple, mais les fonctions d'ordre supérieur sont une fonctionnalité extrêmement puissante. Elles sont omniprésentes en Haskell !  

---

## Fonction `filter`  

Commençons avec la fonction `filter` :  

```haskell
:t filter
filter :: forall a. (a -> Bool) -> [a] -> [a]
```

Cette fonction prend un prédicat (`a -> Bool`) et une liste `[a]` et filtre les éléments de la liste selon le prédicat.  

Exemple : filtrer les nombres pairs d'une liste de 1 à 20 :  

```haskell
filter even [1..20]
[2,4,6,8,10,12,14,16,18,20]
```

Un exemple plus complexe : filtrer les fruits contenant la lettre 'a' :  

```haskell
fruitWithA = filter tempFunct ["Apple", "Banana", "Pear", "Grape", "Wood"]
                where tempFunct x = 'a' `elem` x
fruitWithA
["Banana","Pear","Grape"]
```

---

## Fonction `any`  

La fonction `any` vérifie si au moins un élément d'une liste satisfait un prédicat donné :  

```haskell
any :: (a -> Bool) -> [a] -> Bool
```

Exemple : vérifier si un élément de la liste est supérieur à 4 :  

```haskell
biggerThan4 x = x > 4
any biggerThan4 [1,2,3,4]
False
```

Un exemple plus concret : vérifier si une liste de voitures en vente contient encore des modèles disponibles :  

```haskell
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]
biggerThan0 (_,x) = x > 0
any biggerThan0 cars
True
```

---

## Fonctions lambda  

Une fonction lambda (ou fonction anonyme) est une fonction qui n'a pas de nom.  

Exemple :  

```haskell
\x y -> x * y
```

Les fonctions lambda sont particulièrement utiles pour éviter de nommer des fonctions utilisées une seule fois.  

Exemple avec `any` :  

```haskell
any (\x -> x > 4) [1,2,3,4]
False
```

Ou encore avec `filter` :  

```haskell
filter (\x -> 'a' `elem` x) ["Apple", "Banana", "Pear", "Grape", "Wood"]
["Banana","Pear","Grape"]
```

---

## Priorité et associativité  

Haskell attribue une priorité (de 0 à 9) aux opérateurs.  

Exemple :  

```haskell
:i (+)  -- infixl 6 +
:i (*)  -- infixl 7 *
```

Puisque `*` a une priorité plus haute que `+`, l'expression suivante :  

```haskell
1 + 2 * 3
```

est évaluée comme `1 + (2 * 3)`, donnant `7`.  

L'associativité indique si un opérateur s'évalue de gauche à droite (`infixl`) ou de droite à gauche (`infixr`).  

---

## Fonctions curryfiées  

En Haskell, toutes les fonctions sont curryfiées, c'est-à-dire qu'elles prennent un seul argument et retournent une fonction qui attend les arguments restants.  

Exemple :  

```haskell
add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
```

est en réalité :  

```haskell
add3 :: Int -> (Int -> (Int -> Int))
add3 = \x -> \y -> \z -> x + y + z
```

---

## Application partielle  

L'application partielle permet de fixer certains paramètres d'une fonction pour créer une nouvelle fonction.  

Exemple :  

```haskell
createEmail :: String -> String -> String -> String
createEmail domain name lastName = name ++ "." ++ lastName ++ "@" ++ domain

createEmailTeckel = createEmail "teckel-owners.com"
createEmailSCL = createEmail "secret-cardano-lovers.com"

createEmailTeckel "Robertino" "Martinez"
"Robertino.Martinez@teckel-owners.com"
```

---

## Application et composition de fonctions  

### L'opérateur `$`  

L'opérateur `$` réduit l'usage des parenthèses :  

```haskell
show $ (2**) $ max 3 $ 2 + 2
```

équivaut à :  

```haskell
show ((2**) (max 3 (2 + 2)))
```

---

### L'opérateur `.` (composition de fonctions)  

L'opérateur `.` permet de composer des fonctions :  

```haskell
(complicatedF x) = any even . filter (>25) . tail . take 10 $ x
```

Ceci remplace plusieurs appels imbriqués et rend le code plus lisible.

---

