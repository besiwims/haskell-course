# La fonction filter

La fonction `filter` est une autre fonction d'ordre supérieur utilisée pour travailler avec les listes. Comme son nom l'indique, elle permet de filtrer une liste en ne sélectionnant que les éléments qui satisfont un prédicat défini par la fonction passée en paramètre. Comme `map`, nous pourrions aussi définir `filter` en utilisant une compréhension de liste :

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [x | x <- xs, f x]
```

Contrairement à `map`, la fonction `filter` doit recevoir une fonction qui retourne un booléen en tant qu'argument, et la liste résultante de `filter` sera toujours du même type que la liste passée en entrée, car nous ne faisons que sélectionner des éléments de cette liste sans en générer de nouveaux. Nous pouvons filtrer une liste en utilisant notre fonction `squareGt100` vue précédemment afin d'obtenir les éléments pour lesquels `squareGt100` renvoie `True` :

```haskell
ghci> filter squareGt100 [7..12]
[11,12]
```

Voici quelques autres exemples d'utilisation de `filter` avec des fonctions prédéfinies :

```haskell
ghci> filter odd [1..5] -- obtenir tous les nombres impairs d'une liste
[1,3,5]

ghci> filter (\x -> length x > 2) ["a", "abc"] -- éléments dont la longueur est supérieure à 2
["abc"]

ghci> filter (\(x:xs) -> x == 'a') ["cardano", "ada"] -- éléments commençant par 'a'
["ada"]
```

