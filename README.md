## Group Membership Management Indexation schema implementation in Erlang (polish version below)

This project is based on Rebar3. Hex has been included and the following libraries included:
- Cowboy
- Eredis
- Jiffy

Project template is [Cowboy Rebar3 template](https://github.com/sfinnie/rebar3_cowboy).


## Instrukcja uruchomienia testów

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
10. W celu zmodyfikoawnia parametrów testowych (np. rozmiar grafu, ilość instancji, wysyłanie obciążenie) należy podać odpowiednie wartości w pliku *tests-k8s/k8s/config/test-config.conf* w repozytorium *agh-gmmf-prototype*:
11. Należy zainstalować narzędzie *kubectl* służące do komunikacji z platformą Kubernetes oraz skonfigurować to narzędzie:
  * od członków zespołu OneData należy uzyskać plik z konfiguracją naszego konta na ich platformie Kubernetes.
  * zdefiniować zmienną środowiskową *KUBECONFIG* przechowującą ścieżkę do wyżej wspomnianego pliku z konfiguracją.
12. W repozytorium *agh-gmmf-prototype* nalezy wykonać komendę: `kubectl apply -k tests-k8s/k8s/` (uruchomi ona przeprowadzany test).
13. Aby uzyskać listę podów działających w klastrze należy wykonać komendę `kubectl get pod`. Należy zapamiętać dokładną nazwę podu *gmm-tester-#*
14. Aby uzyskać logi z trwającego obecnie testu należy wykonać komendę: `kubectl logs gmm-tester-#`.
15. Po ukończeniu testu w logach będzie można zobaczyć komunikat *Finished!*, a powyżej niego komendy potrzebne do pobrania wyników.
16. Wszystkie zasoby z pominięciem podu gmm-tester są usuwane automatycznie, aby go usunąć po zakończonych testach należy wykonać komendę: `kubectl delete job gmm-tester`.
17. W przypadku błędu wszystkie pody gmm-tester oraz instancje pozostaną w celu diagnozy problemu.



