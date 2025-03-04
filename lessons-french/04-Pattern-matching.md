04-Pattern-matching

### Correspondance de motifs et expressions `case`

#### Plan
- Correspondance de motifs dans les fonctions
- Motifs génériques (`catch-all`)
- Analyse approfondie des listes
- Correspondance de motifs :
  - Listes
  - Tuples
- Expressions `case`
- Style de déclaration VS style d'expression

---

### Correspondance de motifs
La correspondance de motifs consiste à faire correspondre des données (valeurs, types, etc.) à un motif, en liant éventuellement des variables aux correspondances réussies.

Nous allons étudier la correspondance de motifs dans trois cas :
1. La correspondance de motifs dans la définition des fonctions.
2. La correspondance de motifs avec les listes.
3. La correspondance de motifs avec les tuples.

Cela peut sembler compliqué, mais une fois que vous aurez compris, ce sera très intuitif. Tout deviendra limpide avec quelques exemples.

Commençons par faire correspondre des fonctions !

---

### Correspondance de motifs dans les fonctions
Vous vous souvenez de la fonction `specialBirthday` de la leçon précédente ?

```haskell
specialBirthday :: Int -> [Char]
specialbirthday age =
  if age == 1
    then "Premier anniversaire !"
    else
      if age == 18
        then "Tu es adulte !"
        else
          if age == 60
            then "Enfin, je peux arrêter de m'intéresser au nouveau jargon !"
            else "Rien de spécial"
```

On sait que cette version pourrait être améliorée avec des gardes (`guards`). Mais nous allons aller encore plus loin et la réécrire en utilisant la correspondance de motifs.

Lorsque l'on fait correspondre des motifs dans la définition d'une fonction, on peut simplement définir la même fonction plusieurs fois avec des valeurs spécifiques en guise de paramètres :

```haskell
specialBirthday :: Int -> [Char]
specialBirthday 1   = "Premier anniversaire !"
specialBirthday 18  = "Tu es adulte !"
specialBirthday 60  = "Enfin, je peux arrêter de m'intéresser au nouveau jargon !"
```

Notre fonction est bien plus claire qu'avant !

Haskell va essayer d'apparier la valeur de `age` avec la première définition. Si `age` n'est pas `1`, il essaiera la deuxième. Si `age` n'est pas `18`, il passera à la troisième, et ainsi de suite jusqu'à ce qu'une correspondance soit trouvée.

Mais il y a un problème évident : que se passe-t-il si nous passons une valeur comme `29` ? Pour cela, nous devons utiliser des motifs génériques (`catch-all patterns`) !

---

### Motifs génériques (`catch-all patterns`)
L'en-tête de notre fonction indique que l'on peut passer n'importe quel `Int`. Mais nous n'avons pas défini de comportement pour des valeurs comme `14` ! Si on tente de l'utiliser, Haskell ne saura pas quoi faire et le programme plantera 💥.

On pourrait définir la fonction pour toutes les valeurs possibles, mais ce n'est pas réaliste. La solution ? Un motif générique :

```haskell
specialBirthday :: Int -> [Char]
specialBirthday 1   = "Premier anniversaire !"
specialBirthday 18  = "Tu es adulte !"
specialBirthday 60  = "Enfin, je peux arrêter de m'intéresser au nouveau jargon !"
specialBirthday age = "Rien de spécial"
```

Ainsi, toute valeur non définie explicitement retournera `"Rien de spécial"`.

⚠️ **Attention** : Haskell fait la correspondance du haut vers le bas. Si on écrit :

```haskell
specialBirthday :: Int -> [Char]
specialBirthday age = "Rien de spécial"
specialBirthday 1   = "Premier anniversaire !"
specialBirthday 18  = "Tu es adulte !"
specialBirthday 60  = "Enfin, je peux arrêter de m'intéresser au nouveau jargon !"
```

La première définition captera **toutes** les valeurs, et les autres définitions ne seront jamais exécutées.

On peut également utiliser la valeur capturée :

```haskell
specialBirthday :: Int -> [Char]
specialBirthday 1   = "Premier anniversaire !"
specialBirthday 18  = "Tu es adulte !"
specialBirthday 60  = "Enfin, je peux arrêter de m'intéresser au nouveau jargon !"
specialBirthday age = "Rien de spécial, tu as juste " ++ show age
```

Si on appelle `specialBirthday 22`, on obtient :

```
"Rien de spécial, tu as juste 22"
```

---

### Analyse approfondie des listes

Haskell voit les listes comme une suite d'éléments chaînés avec `:` :

```haskell
[1,2,3,4] == 1:2:3:4:[]
"Hello!" == 'H':'e':'l':'l':'o':'!':[]
```

Nous pouvons utiliser cette structure pour faire correspondre des motifs.

---

### Correspondance de motifs avec les listes

```haskell
whatsInsideThisList :: [Int] -> String
whatsInsideThisList []         = "C'est vide !"
whatsInsideThisList [x]        = "Un seul élément : " ++ show x
whatsInsideThisList [x, y]     = "Deux éléments : " ++ show x ++ " et " ++ show y
whatsInsideThisList (x:y:z:[]) = "Trois éléments : " ++ show [x,y,z]
whatsInsideThisList (x:rest)   = "Le premier élément est : " ++ show x ++ ", et il y en a d'autres !"
```

Exemples :

```haskell
whatsInsideThisList []           -- "C'est vide !"
whatsInsideThisList [1, 2]       -- "Deux éléments : 1 et 2"
whatsInsideThisList [1, 2, 3, 4] -- "Le premier élément est : 1, et il y en a d'autres !"
```

On peut ignorer des valeurs avec `_` :

```haskell
firstAndThird :: [Bool] -> String
firstAndThird (x:_:z:_) = "Les premier et troisième éléments sont : " ++ show x ++ " et " ++ show z
firstAndThird _ = "Ils ne sont pas là !"
```

---

### Correspondance de motifs avec les tuples

On peut extraire des éléments de tuples avec :

```haskell
firstOfThree :: (a, b, c) -> a
firstOfThree (x, _, _) = x
```

Ou créer une nouvelle structure à partir d'un tuple :

```haskell
pairFromFour :: (a, b, c, d) -> (b, d)
pairFromFour (_, x, _, y) = (x, y)
```

---

### Expressions `case`

Les expressions `case` permettent d'évaluer une expression en fonction d'un motif.

```haskell
case <Exp> of
  <Pattern1> -> <Result1>
  <Pattern2> -> <Result2>
  <Pattern3> -> <Result3>
```

Exemple :

```haskell
checkForZeroes :: (Int, Int, Int) -> String
checkForZeroes tuple3 = case tuple3 of
  (0, _, _) -> "Le premier est zéro !"
  (_, 0, _) -> "Le deuxième est zéro !"
  (_, _, 0) -> "Le troisième est zéro !"
  _         -> "Tout va bien !"
```

Les expressions `case` peuvent être utilisées **partout**, pas seulement dans les définitions de fonctions.

---

### Style de déclaration VS Style d'expression

| Style déclaration            | Style expression               |
|------------------------------|--------------------------------|
| `where`                      | `let`                          |
| Pattern matching direct      | `case`                         |
| `if` avec `guards`           | `if then else`                 |
| Argument explicite en paramètre | Fonction lambda `\x -> x*x` |

---

### Résumé

- La correspondance de motifs simplifie l'écriture des fonctions et l'extraction des valeurs.
- Les `catch-all` patterns sont essentiels pour éviter les erreurs.
- `case` est une alternative aux définitions multiples.
- Deux styles existent : **déclaration** et **expression**, à utiliser selon votre préférence.

Haskell vous donne le choix ! 🎉
