Havana

tim Camila Cabello
Danilo Vulović
Dušan Zamahajev
Nemanja Milosavljević
Reprezentacije table za igru:

(
  ------------ jedan red table za igru --------------
 (0 ( (0 value) (1 value) ... (mat_dim value) ))
                                               -jedno polje na tabli-
  (1 ( (0 value) (1 value) ... ((mat_dim+1) value) ))
  .
  .
  .
  ((2 * (mat_dim - 1)) ( (mat_dim value) ((mat_dim+1) value) ... ((2*(mat_dim-1)) value) )  )
)

=================================================================================

File:  creator.lisp
    
Function: (create_matrix dim)
    Kreira matricu (tablu za igru) zadate dimenzije pozivom funkcije (gen_row row dim) koja se dalje rekurzivno poziva kreirajući redove matrice.

Function: (get_range row dim)
Za zadati red i dimenziju polja za igru vraća listu u kojoj je prvi element indeks prvog elementa u redu dok je drugi element indeks poslednjeg elementa u redu.

=================================================================================
File: ascii.lisp
    Na pocetku fajla se u globalnu promenljivu ‘ascii’ postavlja lista ascii karatkera počevši od karaktera  ‘A’ do ‘Z’ koja će se kasnije koristiti za dobijanje karaktera sa početka reda na osnovu indeksa istog.

    Function: (get_ascii index)
    Ukoliko je index u granicama dimenzije liste ‘ascii’ funkcija vraca vrednost sa pozicije index liste ‘ascii’.

Function: (get_number ascii)
Na osnovu prosleđenog karaktera vraća njegov index u listi ‘ascii’.
=================================================================================
File: printer.lisp

Function: (gen_print_matrix mat)
    Na osnovu prosleđene matrice 'mat' koja predstavlja tablu za igru u originalnom obliku kreira novu matricu koja je spremna za prikaz (dodaje prazan prostor pre početka svakog reda, index reda zamenjuje karakterom i dodaje index kolone na kraju redova gornje polovine matrice). Novokreirana matrica se postavlja u globalnu promenljivu print_mat koja takođe predstavlja i povratnu vrednost funkcije.
    Nad svakim izdvojenim redom, korišćenjem 'mapcar' funkcije, pozivaju se funkcije 'construct_lower_half' ili 'construct_upper_half' u zavisnosti da li se radi o redu sa gornje ili donje polovine matrice. Na kraju redova sa gornje plovine matrice dodaju se indeksi kolone.


Function: (extract_values row)
    Od zadatog reda kreira listu uzimajući samo drugi element svakog polja (X ili O, prvi element je indeks kolone)

Function: (calc_padding_size row dim)
    Za zadati red (row) i dimenziju polja za igru (dim) racuna broj praznih mesta koje je potrebno dodati na pocetku reda.

Function: (create_padding size)
Za zadatu velicinu (size) kreira niz blanko znakova (space) koji će biti postavljeni na početku reda.

Function: (range sInd eInd)
    Kreira listu celih brojeva počevđi od sInd do eInd. Koristi se za kreiranje prvog reda koji predstavlja indekse redova.

Function: (add_frist_row mat)
    Prosleđenoj matrici dodaje prvi red sa indeksima redova.

Function: (print_matrix)
    Poziva funkciju generate_matrix za generisanje matrice za prikaz na osnovu trenutnog stanja na tabli a zatim koristeći funckiju mapcar prolazi kroz redove novokreirane matrice i stampa iste.

=================================================================================

File: move.lisp

Function: (is_valid row column)
        Proverava da li je zadato polje na validnim koordinatama.

    Function: (make_move symbol row column)
        Na poziciju određenu sa row i column postavlja prosleđeni symbol.


Function: (make_move_sample sample)
    Menja trenutnu matricu (stanje na tabli) prosleđenom (sample).

Function: (possible_moves prev_state symbol)
    Za prosleđeno stanje i simbol kreira listu matrica pri čemu svaka predstavlja po jednu moguću poziciju na kojoj se može odigrati datim simbolom.

    Function: (next_state prev_state symbol row column)
Na osnovu prosleđene matrice, reda i kolone, kreira (i vraća) novu matricu sa odigranim potezom na polju određenom sa ‘row’ i ‘column’.

    Function: (compute_next_move player)
        Za datog igraca izračunava najbolji mogući potez.
        Trenutna implementacija vraća random potez kako bi bilo moguće igrati igru sa računarom.

=================================================================================
File: game.lisp

Function: (player_part_game player)
    Funkcija koja od korisnika zahteva unos dve vrednosti koje predstavljaju polje na kome će biti postavljen prosleđeni simbol (player), (ukoliko su unete validne koordinate) odigrava zadati potez (postavlja simbol) i proverava da li je došlo do kraja igre odnosno da li je kreiran 'bridge', 'fork' ili 'ring'. Ukoliko je ispunjen neki od uslova za kraj igre, prikazuje se prikladna poruka i igra se zavrsava dok se u suprotnom poziva funkcija zadužena za potez računara.

    Function: (coputer_part_game player)
    Funkcija koja na osnovu trenutnog stanja table zahteva izračunavanje najboljeg mogućeg poteza za igrača određenog argumentom ‘player’, dobijeno stanje postavlja na mesto trenutnog stanja a zatim proverava da li je došlo do ispunjenja nekog od uslova za kraj igre. Ukoliko je igra završena prikazuje prikladnu poruku dok se u suprotnom poziva funkciju za igru drugog igrača.

Function: (next_player player)
    Vraća narednog igrača u odnosnu na trenutnog (prosleđenog).

=================================================================================
File: end_game.lisp

Na samom početku file-a u globalnu promenljivu 'corners' se postavlja lista koordinata temena heksagona. Takođe, globalne promenljive ‘upper_left_wall’, ‘upper_right_wall’, ‘upper_wall’, ‘down_left_wall’, ‘down_right_wall’ i ‘down_wall’, sadrže liste koordinata polja gornjeg levog, gornjeg desnog, gornjeg, donjeg levo, donjeg desno i donjeg zida respektivno. Za njihovu inicijalizaciju se koriste funkcije go_down_left, go_down_right, go_right.

      Function: (go_down_left start end)
    Vraća listu koordinata polja koja se nalaze od start-a do end-a uključujući start a bez end-a tako što kreće od start-a i ide kroz tablicu krećući se dole levo dok ne stigne do end-a.

Function: (go_down_right start end)
Vraća listu koordinata polja koja se nalaze od start-a do end-a uključujući start a bez end-a tako što kreće od start-a i ide kroz tablicu krećući se dole desno dok ne stigne do end-a.

Function: (go_right start end)
    Vraća listu koordinata polja koja se nalaze od start-a do end-a uključujući start a bez end-a tako što kreće od start-a i ide kroz tablicu krećući se desno dok ne stigne do end-a.

Function: (check_bridge player)
    Proverava da li je igrač koji je upravo igrao uspešno povezao bar 2 temena heksagona i vraća
T ukoliko jeste i ‘() ukoliko nije.

Function: (has_bridge ls)
    Vraća potvrdu da li su bilo koja 2 temena međusobno povezana.

Function: (fields_with_symbol symbol ls)
    Filtrira listu koordinata polja i ostavlja samo polja koja imaju prosleđeni znak (symbol).

Function: (fields_without_symbol symbol ls)
    Filtrira listu koordinata polja i ostavlja samo polja koja nemaju prosleđeni znak (symbol).

Function: (get_neighbours row column)
    Vraća listu koordinata svih suseda polja koje se nalazi na prosleđenim koordinatama.

Function: (bridge_traversal start end visited)
Funkcija vraća potvrdu da li postoji put od start-a do end-a tako što koristi bfs prolaz kroz tablicu.

Function: (check_fork played)
Na osnovu upravo odigranog poteza (played je lista koordinata odigranog poteza), proverava da li postoji fork i vraća T ako postoji.

Function: (get_fork_tree played)
Vraća listu koordinata svih polja do kojih je moguće doći od upravo odigranog polja.

Function: (fork_traversal start visited)
Vraća listu svih čvorova do kojih je moguće doći rekurzivnim dfs prolazom.

Function: (number_of_walls_hit ls)
Vraća broj zidova kod kojih se bar jedan element javlja u listu ls.       

Function: (check_ring played)
Vraća potvrtdu da li postoji ring na osnovu upravo odigranog poteza.



Function (ring_traversal start visited symbol)
Prolazi kroz polja koja nisu jednaka symbol-u i vraća T ukoliko nijedan zid nije dotaknut, u suprotnom vraća ‘().           

=================================================================================
