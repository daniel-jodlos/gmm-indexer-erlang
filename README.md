## Group Membership Management Indexation schema implementation in Erlang (polish version below)

This project is based on Rebar3. Hex has been included and the following libraries included:
- Cowboy
- Eredis
- Jiffy

Project template is [Cowboy Rebar3 template](https://github.com/sfinnie/rebar3_cowboy).


## Instrukcja uruchomienia testów skalowalności

1. Sklonować repozytoria z serwisu GitHub: 
  * daniel-jodlos/gmm-indexer-erlang
  * MrPaulMarshall/agh-gmmf-prototype

2. Założyć konto na DockerHub (na potrzeby tego dokumentu nazwa użytkownika *abc*) i skonfigurować narzędzie DockerHub z lokalnym systemem (w szczególności dane logowania).
3. Należy zainstalować narzędzie *rebar3* wraz z *Erlang OTP 24*. 
4. W repozytorium *gmm-indexer-erlang* należy uruchomić następujące komendy:
  * `sudo docker build -t abc/gmm-indexer-erlang:latest .`
  * `sudo docker image push abc/gmm-indexer-erlang:latest`
5. W repozytorium *agh-gmmf-prototype* należy dokonać następujących modyfikacji w plikach:
  * *Makefile* zmienić pierwszą część nazw obrazów (konto użytkownika) na konto utworzone na platformie DockerHub (np. *kjarosz/agh-gmmf-prototype-tester:latest* na *abc/agh-gmmf-prototype-tester:latest*).
  * *tests-k8s/k8s/job-test-kube.yml* zmienić wartości własności *image* z *kjarosz/agh-gmmf-prototype-tester:latest* na *abc/agh-gmmf-prototype-tester:latest* (najprawdopodobniej będzie to w 21. linii kodu).
6. Należy zainstalować narzędzie Maven oraz Java 11.
7. Jeżeli chcesz zadeklarować na których node'ach mają być uruchamiane poszczególne instancje, to należy odkomentować zakomentowane linii w funkcji `buildDeployment` w pliku *agh-gmmf-prototype/zone-simulator/src/main/java/com/github/kjarosh/agh/pp/k8s/K8sZone.java* oraz ewentualnie zmodyfikować wartości w mapie `nodeMap`.
8. W repozyorium *agh-gmmf-prototype* należy uruchomić komendy:
  * `mvn package`
  * `sudo make push`
9. Zależnie od tego czy w trakcie testu mają być wygenerowany graf oraz operacje należy dokonać następujących zmian w repozytorium *agh-gmmf-prototype*:
  * jeśli chcesz, wygenerować nowy graf i operacje (zakładam że tak), to w pliku *tests-k8s/k8s/kustomization.yaml* zakomentuj dwie ostatnie linie (te z graph.json oraz queries.json.gz)
  * jeśli chcesz istniejącego grafu lub operacji umieść te pliki w katalogu *tests-k8s/k8s/config/* z nazwami *graph.json* i *queries.json.gz*.
10. W celu zmodyfikowania parametrów testowych (np. rozmiar grafu, ilość instancji, obciążenie) należy podać odpowiednie wartości w pliku *tests-k8s/k8s/config/test-config.conf* w repozytorium *agh-gmmf-prototype*:
  * COUNT_ZONES -> liczba instancji w grafie,
  * inter_zone_levels -> współczynnik ilości krawędzi pomiędzy instancjami do liczby wszystkich krawędzi,
  * spaces_per_zone -> liczba wierzchołków typu *Space* na jedną instancję, wprowadzenie wartosci N spowoduje wygenerowanie grafu z około 40 * N wierzchołkami na instancję,
  * loads -> ilość zapytań wysyłanych przez klienta do systemu na sekundę (można podaćkilka wartości liczbowych pooddzielanych spacjami),
  * naive -> true/false, od tego parametru zależy czy użyta będzie wersja naiwna czy z indeksacją,
  * WARMUP_TIME -> liczba sekund działania systemu od uruchomienia, która nie będzie uwzględniania w obliczaniu wyników (służy do "rozgrzania" systemu),
  * TEST_TIME -> liczba sekund działania systemu, podczas ktorych będą zbierane statystyki,
  * REPETITIONS -> liczba powtórzeń testu (zalecana liczba to 1 - w wyniku wcześniejszych modyfikacji ta opcja nie jest wspierana),
  * ZONE_IMAGE -> nazwa obrazu instancji z serwisu DockerHub (którą chcemy testować), należy podać np. `abc/agh-gmmf-prototype:latest`
11. Należy zainstalować narzędzie *kubectl* służące do komunikacji z platformą Kubernetes oraz skonfigurować to narzędzie:
  * od członków zespołu OneData należy uzyskać plik z konfiguracją naszego konta na ich platformie Kubernetes.
  * zdefiniować zmienną środowiskową *KUBECONFIG* przechowującą ścieżkę do wyżej wspomnianego pliku z konfiguracją.
12. W repozytorium *agh-gmmf-prototype* nalezy wykonać komendę: `kubectl apply -k tests-k8s/k8s/` (uruchomi ona przeprowadzany test).
13. Aby uzyskać listę podów działających w klastrze należy wykonać komendę `kubectl get pod`. Należy zapamiętać dokładną nazwę podu `gmm-tester-<ID>`
14. Aby uzyskać logi z trwającego obecnie testu należy wykonać komendę: `kubectl logs gmm-tester-<ID>`.
15. Po ukończeniu testu w logach będzie można zobaczyć komunikat *Finished!*, a powyżej niego komendy potrzebne do pobrania wyników.
16. Wszystkie zasoby z pominięciem podu gmm-tester są usuwane automatycznie, aby go usunąć po zakończonych testach należy wykonać komendę: `kubectl delete job gmm-tester`.
17. W przypadku błędu wszystkie pody gmm-tester oraz instancje pozostaną w celu diagnozy problemu.


## Instrukcja uruchomienia testów czasów wykonania
1. Sklonować repozytoria z serwisu GitHub:
	- daniel-jodlos/gmm-indexer-erlang
	- MrPaulMarshall/agh-gmmf-prototype
i wygenerować dla nich obrazy Dockera (analogicznie, jak w poprzedniej części README)

2. analogicznie jak w poprzedniej części należy także zainstalować *kubectl* i je skonfigurować

3. W sklonowanym repozytorium MrPaulMarshall/agh-gmmf-prototype:

* Wygenerować graf (przy użyciu klasy GraphGeneratorMain) i listy zapytań dla niego (przy użyciu klasy QuerySequenceGeneratorMain) i umieścić je w katalogu głównym
parametry do generacji list zapytań odpowiednich typów:
	- MEMBERS: -t members -g graph.json -n 200000 -o queries_members.json -e 1
	- EFFECTIVE PERMISSIONS: -t ep -g graph.json -n 200000 -o queries_ep.json -e 1
	- REACHES (existing): -t reaches -g graph.json -n 200000 -o queries_reaches_exist.json -e 1
	- REACHES: (nonexisting): -t reaches -g graph.json -n 200000 -o queries_reaches_exist.json -e 0

	parametr -n oznacza ilość zapytań, jakie chcemy wygenerować, a -g ścieżkę do danego grafu

* utworzyć N podów z wybranym obrazem serwera oraz jeden pod klienta przy użyciu klasy KubernetesClient
	- jGMMF: -z 10 -c C:\Users\48512\.kube\student-k8s-cyf.yaml -n gmm-awojciak -i pmarszal/agh-gmmf-prototype:latest -a pmarszal/agh-gmmf-prototype:latest
	- eGMMF: -z 10 -c C:\Users\48512\.kube\student-k8s-cyf.yaml -n gmm-awojciak -i pmarszal/gmm-indexer-erlang:latest -a pmarszal/agh-gmmf-prototype:latest

	parametry: -z (ilość zon), -i (obraz dla podów serwerów) -a (obraz dla poda klienta) -c (ścieżka do configu kubernetesa) -n (namespace w klastrze)

* w katalogu głównym  wykonać polecenie "./tests-k8s/tests-queries-withuout-generating.sh <<N>>", gdzie N oznacza liczbę wszystkich utworzonych podów (np. dla powyższych przykładów - 11)

* czekać aż w podzie klienta (ten o numerze N - dla przykładów wyżej nazwa będzie zaczynać się od "zone10") pojawi się plik "results.txt" zawierający uśrednione rezultaty wszystkich wywołań QueryClientMaina dla wszystkich zapytań (powinno to zająć ok. 4 godzin)
