# Analiza profili na Sympatii

Repo zawiera skrytpy (nie jest to ideał kodu) oraz zebrane dane z profili kobiet i mężczyzn z serwisu [Sympatia.pl](http://sympatia.onet.pl).

Zawartość repo wykorzystana została w analizach opublikowanych [na blogu](http://prokulski.net):

* część I: [kobiety](http://prokulski.net/index.php/2018/07/06/dziewczyny-z-sympatii/)
* część II: [mężczyźni](http://prokulski.net/index.php/2018/07/20/chlopaki-z-sympatii/)


# Do czego są odpowiednie pliki?

#### get_profiles.R

zebranie listy linków do profili i danych z profili, w zmiennych 

* sympatia_login
* sympatia_pass

powinny być dane do logowania do serwisu Sympatia.pl


#### plots.R

funkcje do rysowania wykresów 


#### profile_links_all_*.RDS

linki do profili - kobiet i mężczyzn


#### grabbed_profiles_all_*.RDS

dane zebrane z profili - kobiet i mężczyzn
