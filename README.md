# Analiza profili na Sympatii

Repo zawiera skrytpy (nie jest to ideał kodu) oraz zebrane dane z profili kobiet i mężczyzn z serwisu [Sympatia.pl](http://sympatia.onet.pl).

Zawartość repo wykorzystana została w analizach opublikowanych [na blogu](http://prokulski.net):

* część I: [kobiety](http://prokulski.net/index.php/2018/07/06/dziewczyny-z-sympatii/)
* część II: [mężczyźni](http://prokulski.net/index.php/2018/07/20/chlopaki-z-sympatii/)
* część III: [co widać na zdjęciach kobiet](http://prokulski.net/index.php/2018/07/30/dziewczyny-z-sympatii-czesc-2)
* część IV: [deklaracje i oczekiwania](http://prokulski.net/index.php/2018/08/02/sympatia-porownanie-pan-i-panow/)


# Do czego są odpowiednie pliki?

#### blog_post_*.Rmd

Treści wpisów na blogu


#### boys_vs_girls.R

Porównanie oczekiwań i deklaracji (źródła dla części IV)


#### face_api.RDS

Dane zebrane z Face API


#### faces.R

Skrypty do części III.


#### get_profiles.R

zebranie listy linków do profili i danych z profili, w zmiennych 

* sympatia_login
* sympatia_pass

powinny być dane do logowania do serwisu Sympatia.pl


#### grabbed_profiles_all_*.RDS

dane zebrane z profili - kobiet i mężczyzn


#### grafy.R

Próbka z grafami, nie wykorzystane


#### omnie.R

Próbka z analizą opisów z profili - nie wykorzystane


#### photos_by_faceapi.R

Skrypt do pobierania danych z Face API


#### plots.R

funkcje do rysowania wykresów na potrzeby części I i II


#### profile_links_all_*.RDS

linki do profili - kobiet i mężczyzn

