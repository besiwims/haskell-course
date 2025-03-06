Handling-Errors

Voici la traduction en français :

---

# Gestion des erreurs en Haskell 🧑‍🚒🧯💨 🔥

Haskell est un langage compilé, ce qui signifie que nous pouvons subdiviser toutes les erreurs possibles en deux catégories :

✅ **Erreurs à la compilation** 🙌  
Les erreurs à la compilation sont géniales ! Nous ADORONS les erreurs à la compilation. Parce que cela signifie que notre programme contient une erreur, et que notre compilateur l'a trouvée et nous l'a signalée AVANT qu'elle n'atteigne l'utilisateur final. Au bout du compte, l'objectif principal est que nos utilisateurs apprécient notre logiciel et ne rencontrent pas :

❌ **Erreurs à l'exécution** 🫣  
Les erreurs à l'exécution sont les pires ! Comme elles surviennent pendant l'exécution du programme, elles peuvent affecter directement les utilisateurs finaux. Ces derniers deviennent alors frustrés, arrêtent de payer, nous mettent une note de 1 étoile, etc.

Comparé à la plupart des autres langages de programmation, Haskell est exceptionnellement bon pour éviter les erreurs à l'exécution. Au point que beaucoup disent : **"En Haskell, si ça compile, ça fonctionne."**  

Cela est en grande partie dû à sa pureté et à son puissant système de types, qui permet d'attraper ces erreurs dès la compilation, transformant ainsi de nombreuses erreurs d'exécution en erreurs de compilation.

Toutefois, cela ne signifie pas que nous pouvons complètement oublier les erreurs à l'exécution. Comme nous l'avons mentionné dans les leçons précédentes, même si nous pouvions écrire un code parfait (ce qui est impossible), nous l'exécutons toujours dans le monde réel. Et dans ce monde réel, les ordinateurs manquent de mémoire, des fichiers censés exister sont introuvables, les connexions Internet échouent, etc. Et en plus de tout cela, les utilisateurs font des choses inimaginables avec notre logiciel.

Dans cette leçon, nous allons donc apprendre à gérer les erreurs d'exécution et à utiliser le système de types pour déplacer certaines de ces erreurs vers la compilation, afin que le compilateur puisse nous aider à les détecter.

Plus précisément :

---

### Plan  
- Il y a toujours des exceptions à la règle  
- Gestion express des exceptions avec une voiture autonome 🤖 🚗  
- Je suis une exception, car j’ai une classe 😎  
- Lancez toutes les exceptions que vous voulez, je les attraperai toutes !  
- Peut-être me donner une valeur ? 🙏  
  - Avantages des valeurs optionnelles  
- OK, soit tu me donnes une valeur, soit une raison pour laquelle tu ne l’as pas fait !  
  - Des exceptions aux valeurs optionnelles  
- Les compromis  
- Alors, que devrais-je utiliser ?  

---

### Il y a toujours des exceptions à la règle  
Écrivons un programme simple pour calculer la vitesse d’un objet à partir d’un nombre écrit dans un fichier et d’une valeur saisie par un utilisateur :

```haskell
calcVel :: IO ()
calcVel = do
  d <- readFile "aNumber.txt"
  putStrLn "Distance parcourue chargée."
  putStrLn "Veuillez entrer le temps écoulé :"
  t <- getLine
  let v = (read d :: Int) `div` read t
  putStrLn $ "La vitesse de l'objet est d'environ : " ++ show v
  putStrLn "Merci d'utiliser ce programme !"
```

Ce programme lit un fichier contenant la distance parcourue par un objet et demande à l'utilisateur de fournir un nombre représentant le temps écoulé. Ensuite, nous utilisons la fonction `read` pour analyser ces valeurs en `Int` et appliquons la fonction `div` pour obtenir la vitesse. Enfin, nous affichons le résultat et un message de remerciement.

Le programme compile et fonctionne si nous avons un fichier `aNumber.txt` contenant un nombre et si l'utilisateur saisit un nombre valide.

Mais la réalité est plus compliquée, et des choses inattendues peuvent arriver. Quand cela se produit, nous obtenons une **exception**. Voici quelques-unes des exceptions possibles :

- Si le fichier `aNumber.txt` n'existe pas :  
  ```haskell
  *** Exception: aNumber.txt: openFile: does not exist (No such file or directory)
  ```
- Si nous ne fournissons pas des nombres valides :  
  ```haskell
  *** Exception: Prelude.read: no parse
  ```
- Si nous entrons `0` comme temps écoulé :  
  ```haskell
  *** Exception: divide by zero
  ```

Actuellement, toutes ces exceptions entraînent l'arrêt brutal de notre programme. Mais un programme robuste ne doit pas s'effondrer face à l'imprévu. Heureusement, nous avons plusieurs moyens pour gérer ces exceptions.

---

### Gestion express des exceptions avec une voiture autonome 🤖 🚗  
La gestion des exceptions en Haskell est un sujet vaste. Mais pour cette leçon, nous allons couvrir les bases de manière rapide et efficace.

Imaginons que nous voulons créer une **IA pour une voiture autonome**. Pour simplifier, cette voiture se contente d’avancer en ligne droite et de réagir aux feux de signalisation.

```haskell
dumbAICar :: IO ()
dumbAICar = do
  putStrLn "Quelle est la couleur du feu de signalisation ?"
  color <- getLine
  putStrLn $ "\nAlors, je vais " ++ nextMove color
  dumbAICar
```

Nous allons être les capteurs de notre voiture et saisir la couleur du feu de signalisation. En fonction de cette couleur, nous définirons la réaction de la voiture via la fonction `nextMove` :

```haskell
data TrafficLight = Red | Yellow | Green deriving (Show, Read)

nextMove :: String -> String
nextMove color = case read color of
  Red    -> "S'arrêter !"
  Yellow -> "Attendre..."
  Green  -> "Avancer !"
```

Problèmes :
1. Si nous écrivons une couleur qui n'est pas reconnue, le programme plante.
2. Le message d’erreur n’est pas très clair :
   ```haskell
   *** Exception: Prelude.read: no parse
   ```

---

### Je suis une exception, car j’ai une classe 😎  
Nous allons créer une exception personnalisée en définissant un type de données :

```haskell
data TrafficLightException = TrafficLightIsOff | WrongColor String
    deriving (Show)
```

Puis, nous faisons de ce type une instance de `Exception` :

```haskell
instance Exception TrafficLightException
```

Nous pouvons maintenant **lever** des exceptions avec `throwIO` :

```haskell
nextMove :: String -> IO String
nextMove color = case color of
  "Red"    -> return "S'arrêter !"
  "Yellow" -> return "Attendre..."
  "Green"  -> return "Avancer !"
  "Black"  -> throwIO TrafficLightIsOff
  _        -> throwIO . WrongColor $ color ++ " n'est pas une couleur valide !"
```

Résultat :

```haskell
>> nextMove "Black"
*** Exception: TrafficLightIsOff

>> nextMove "Arc-en-ciel"
*** Exception: WrongColor "Arc-en-ciel n'est pas une couleur valide !"
```

---

### Lancez toutes les exceptions que vous voulez. Je les attraperai toutes !  
Nous utilisons `catch` pour gérer ces exceptions :

```haskell
dumbAICar :: IO ()
dumbAICar = do
  putStrLn "Quelle est la couleur du feu de signalisation ?"
  color    <- getLine
  response <- nextMove color `catch` handler
  putStrLn $ "Je vais " ++ response
  dumbAICar

  where
    handler :: TrafficLightException -> IO String
    handler e = do
      putStrLn $ "ATTENTION : " ++ show e
      case e of
        TrafficLightIsOff -> return "Avancer prudemment."
        WrongColor _      -> return "Arrêter la voiture immédiatement !"
```

---

### Peut-être me donner une valeur ? 🙏  
Une autre approche pour éviter les erreurs d'exécution est d'utiliser le type **Maybe** :

```haskell
safeDiv :: Integral a => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
```

Ainsi, nous évitons l'exception **"divide by zero"** dès le départ.

---

### Conclusion  
Nous avons vu deux méthodes principales pour gérer les erreurs en Haskell :  
✅ Exceptions (avec `throwIO` et `catch`)  
✅ Valeurs optionnelles (`Maybe`)  

L'utilisation de `Maybe` est souvent préférable, car elle **évite** les erreurs dès le départ, rendant le code plus sûr et plus idiomatique. 🚀
