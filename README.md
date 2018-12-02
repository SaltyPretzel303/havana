Reprezentacije table za igru:

(
  (0 ( (0 value) (1 value) ... (mat_dim value) ))
  (1 ( (0 value) (1 value) ... ((mat_dim+1) value) ))
  .
  .
  .
  ((2 * (mat_dim - 1)) ( (mat_dim value) ((mat_dim+1) value) ... ((2*(mat_dim-1)) value) )  )
)


file: game.lisp
  function: (game player)
    Funkcija koja od korisnika zahteva unos dve vrednosti koje predstavljaju polje na kome će biti postavljen prosleđeni simbol (player), odigrava zadati potez (postavlja simbol) i proverava da li je došlo do kraja igre odnosno da li je kreiran 'bridge', 'fork' ili 'ring'. Ukoliko je ispunjen neki od uslova za kraj igre, prikazje se prikladna poruka i igra se zavrsava dok se u suprotnom rekurzivno poziva funkcija game za suprotnog igrača.

  function: (next_player player)
    Vraća narednog igrača u odnosnu na trenutnog (prosleđenog).

file: end_game.lisp
  Na samom početku file-a u globalnu promenljivu 'corners' se postavlja lista koordinata uglova.

  function:



file: printer.lisp
  function: (gen_print_matrix mat)
    Na osnovu prosleđene matrice 'mat' koja predstavlja tablu za igru u originalnom obliku kreira novu matricu koja je spremna za prikaz (dodaje prazan prostor pre početka svakog reda, index reda zamenjuje karakterom i dodaje index kolone na kraju redova gornje polovine matrice). Novokreirana matrica se postavlja u globalnu promenljivu print_mat koja takođe predstavlja i povratnu vrednost funkcije.
    Nad svakim izdvojenim redom, korišćenjem 'mapcar' funkcije, pozivaju se funkcije 'construct_lower_half' ili 'construct_upper_half' u zavisnosti da li se radi o redu sa gornje ili donje polovine matrice. Na kraju redova sa gornje plovine matrice dodaju se indeksi kolone.
