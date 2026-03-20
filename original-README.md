# Reductor
### Użycie
```
Reductor file [-s|--search] [-v|--verbose INT] [--no-color] 
                [-d|--depth DEPTH]
```
## Opis
Rozwiązanie korzysta z monady `logict` - obliczeń z nawrotami (*jak w programowniu w logice*).

Dostępnych jest kilka możliwości wypisywania wyjścia, domyślnie wypisywane są kroki realnie zastosowanych podstawień.

Możliwe jest wypisywanie pośrednich kroków redukcji za pomocą opcji `--verbose 1`, typu:
``` haskell
{add two two}
{add two} two
{add} two two
...
```
Jak również możliwe jest odnotowywanie dokładnych kroków redukcji wymuszonych, w tym samego sprawdzenia wzorca (opcja `--verbose 2`):
``` hs
{add} two two
add {two} two
add {S (S Z)} two
add ({S} (S Z)) two
...
```
### Drzewo poszukiwań
Jak przystało na dobry program z nawrotami, możliwe jest obejrzenie całego procesu *poszukiwań* rozwiązania.

Za pomocą opcji `--search` możemy włączyć wypiswanie **wszystkich-wszystkich** kroków redukcji:
```
goal: reduce add two two
{add two two}
{add two} two
{add} two two
goal: add [Z*,n] -> n
goal: Z* == two
goal: Forced reduction to Z
add {two} two
goal: two -> S (S Z)
success. (two -> S (S Z))
add {S (S Z)} two
fail. (Forced reduction to Z)
redo.
goal: add [S (m),n] -> S (add m n)
goal: S (m) == two
goal: Forced reduction to S
add {two} two
goal: two -> S (S Z)
success. (two -> S (S Z))
add {S (S Z)} two
add ({S} (S Z)) two
success. (Forced reduction to S)
```

### Kolorowe wyjście
Domyślnie program używa kolorowego wyjścia (nieoptymalnie zaimplementowanewgo, ale... :)) przy użyciu `ANSI ESC Codes`.

* Kolorem niebieskim zaznaczane są aktualne elementy wyrażenia rozważane w redukcji.
* Kolorem żółtym zaznacze są elementy wyrażenia rozważane w *redukcji wymuszonej* (przez dopasowywanie do wzorca).
* Kolorem zielonym zaznaczone są elementy wyrażenia *pasującego konstruktora*

Gdyby ten charakter wyjścia stwarzał problemy, możemy wyłączyć kolory opcją `--no-color`.

Disclaimer: *Ścieżka poszkiwań jest zawsze wyświetlana z uzyciem kolorów*

### Opcje
Wszystkie opcje można poznać korzystając z opcji `--help`.

