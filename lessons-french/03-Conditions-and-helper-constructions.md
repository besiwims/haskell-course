03-Conditions-and-helper-constructions

Voici la traduction en français :  

---

## Conditions et constructions auxiliaires  

### Plan  
- Expressions if-then-else  
- Garde (Guards)  
- Expressions let  
- where  
- Dois-je utiliser let ou where ?  
- Points à garder à l'esprit  

---

### Expressions if-then-else  
Souvent, dans votre code, vous devez faire un choix. Il existe plusieurs façons d'exprimer des conditions. En Haskell, nous utilisons le plus souvent les expressions if-then-else :  

```haskell
if <Condition>  
  then <Expression1>  
  else <Expression2>
```
Où **Condition** est une expression logique qui donne True ou False, **Expression1** est utilisée si Condition est True, et **Expression2** est utilisée si Condition est False.  

La fonction suivante `checkLocalHost` vérifie si l'argument est localhost et l'indique à l'utilisateur :

```haskell
checkLocalhost :: String -> String
checkLocalhost ip =
    if ip == "127.0.0.1"
        then "C'est localhost !"
        else "Non, ce n'est pas localhost."
```

```haskell
checkLocalhost "127.0.0.1"
"C'est localhost !"
```

Ici, `checkLocalhost` prend un argument de type `String` et retourne un autre `String`. Il compare l'adresse IP fournie à `"127.0.0.1"`. Si elles sont identiques, il retourne `"C'est localhost !"`, sinon `"Non, ce n'est pas localhost."`.  

Dans les langages impératifs, `else` n'est pas toujours obligatoire, mais en Haskell, **il l'est toujours** ! Cela s'explique par le fait qu'en Haskell, chaque fonction doit retourner une valeur, donc nous devons fournir un résultat du même type pour les cas `then` et `else`.  

---

### Garde (Guards)  
Imaginons maintenant que nous souhaitions faire un test plus complexe, par exemple, vérifier si l'âge d'anniversaire a une signification spéciale. Nous pourrions utiliser des if imbriqués comme ceci :  

```haskell
specialBirthday :: Int -> [Char]
specialBirthday age =
  if age == 1
    then "Premier anniversaire !"
    else
      if age == 18
        then "Tu es adulte !"
        else
          if age == 60
            then "Enfin, je peux arrêter de suivre les nouvelles expressions à la mode !"
            else "Rien de spécial"
```

C'est assez difficile à lire. Heureusement, Haskell propose une meilleure approche : **les gardes**.  

#### Utilisation des gardes  

Les gardes fonctionnent comme des if-else mais permettent d'écrire plusieurs conditions de manière plus lisible :  

```haskell
func arg
  | <Condition1> = <Résultat1>
  | <Condition2> = <Résultat2>
  | <Condition3> = <Résultat3>
  ...
```

Voici la fonction `specialBirthday` réécrite avec des gardes :  

```haskell
specialBirthday :: Int -> [Char]
specialBirthday age
  | age == 1 = "Premier anniversaire !"
  | age == 18 = "Tu es adulte !"
  | age == 60 = "Enfin, je peux arrêter de suivre les nouvelles expressions à la mode !"
  | otherwise = "Rien de spécial"
```

Ici, `otherwise` est un alias pour `True`, permettant d'écrire une dernière condition qui capture tous les autres cas.  

---

### let et where  

Les constructions `let` et `where` permettent de stocker des résultats intermédiaires et de lier des variables locales.  

#### Expressions let  
`let` permet d'associer des expressions à des variables locales comme ceci :  

```haskell
func arg =
    let <BIND_1>  
        <BIND_2>  
    in  <EXPR qui utilise BIND_1 et/ou BIND_2>
```

Exemple : Comparer deux températures (Celsius et Fahrenheit) et retourner la plus chaude en Kelvin.  

```haskell
hotterInKelvin' :: Double -> Double -> Double
hotterInKelvin' c f =
  let fToC t = (t - 32) * 5 / 9
      cToK t = t + 273.16
      fToK t = cToK (fToC t)
   in if c > fToC f then cToK c else fToK f
```

#### where  
`where` permet de définir des variables en fin de fonction :  

```haskell
hotterInKelvin'' :: Double -> Double -> Double
hotterInKelvin'' c f = if c > fToC f then cToK c else fToK f
  where
    fToC t = (t - 32) * 5 / 9
    cToK t = t + 273.16
    fToK t = cToK (fToC t)
```

---

### Dois-je utiliser let ou where ?  

- **`let`** est utile pour diviser des expressions complexes en blocs réutilisables dans une **même expression**.  
- **`where`** est plus adapté lorsqu'on veut **partager** des variables dans plusieurs conditions (guards).  

Exemple avec `where` pour analyser un cylindre :  

```haskell
analyzeCylinder :: Float -> Float -> String
analyzeCylinder diameter height
       | volume < 10 = "Le cylindre est un verre."
       | volume < 100 = "Le cylindre est un seau."
       | volume < 1000 = "Le cylindre est un réservoir."
       | otherwise = "Qu'est-ce que c'est que ce truc énorme ?!"
    where
        volume = pi * diameter^2 * height / 4
```

### Points à garder à l'esprit  

- Les variables définies avec `where` **ne sont accessibles** qu'à l'intérieur du corps de la fonction.  
- Les variables définies avec `let` **existent uniquement dans leur expression**.  

Exemple d'initiales avec `let` :  

```haskell
initials :: String -> String -> String
initials name lastName = if name == "" || lastName == ""
                         then "Quel est ton nom déjà ?"
                         else let x = head name
                                  y = head lastName
                              in [x] ++ "." ++ [y] ++ "."
```

---

### Résumé  

Dans cette leçon, nous avons vu :  

✔ Les expressions `if-then-else` et pourquoi `else` est obligatoire en Haskell.  
✔ Comment utiliser les **gardes** (`|`) pour éviter les if imbriqués.  
✔ Comment utiliser `let` et `where` pour stocker des calculs intermédiaires et **écrire un code plus clair**.  
✔ Quand utiliser `let` et quand utiliser `where`.  

Entraînez-vous et choisissez l'approche qui vous convient le mieux ! 🚀
