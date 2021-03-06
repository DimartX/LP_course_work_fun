# Реферат
## по курсу "Логическое программирование"

### студент: Артемьев Д.И.

## Логическое программирование при создании современных информационных систем

## Результат проверки

| Преподаватель     | Дата         |  Оценка       |
|-------------------|--------------|---------------|
| Сошников Д.В. |              |               |
| Левинская М.А.|              |               |

> *Комментарии проверяющих (обратите внимание, что более подробные комментарии возможны непосредственно в репозитории по тексту программы)*

### Введение

С тех пор, как появились компьютеры, люди мечтали об изобретении машины, понимающей человеческую речь и самостоятельно решающей поставленные задачи. Эта задача и в наши дни остаётся нерешённой, однако есть некоторые концепции, приближающие нас к достижению мечты. Примером такой концепции, когда для того, чтобы программа решала задачу самостосятельно, а человек занимался только формализацией условия, является концепция декларативного прогаммирования. 

Для формализации задач лучше всего подходит математическая логика, достаточно формализованная для восприятия компьютерными системами, а кроме того позволяющая человеку вполне естественно формулировать многие свойства. Парадигма, основанная на формальной логике называется логическое программирование. К семействам языков этой парадигмы обычно относят Prolog, Mercury, answer set programming (ASP), Datalog.

В наше время наиболее распространёнными являются императивная и объектно-ориентированная парадигма, представленные большинством современных популярных языков, среди них C, C++, Python, Java, C#, Rust. Кроме того, наблюдается тенденция популяризации функциональной парадигмы, её встраивание в уже перечисленные языки, а также всё большее использование собственно функциональных языков, таких как Lisp, Haskell, F#.

При всём при этом парадигма логического программирования, казалось, остаётся не у дел. Да и вообще, возможно ли написать хоть сколько-то реально полезную программу на пресловутом Прологе? Могут ли программы на прологе конкурировать по скорости с программами на мейнстримных языках? 

Эти, и многие другие вопросы будут раскрыты в следующих разделах. 

### Использование логического программирования в прошлом

Логическое программирование, каким оно является сейчас, прослеживается ещё в дискуссии конца 1960-х, начала 1970-х годов о том, декларативное или процедурное представление знаний должно быть присуще искусственному интеллекту. А так как, по крайней мере раньше из языков логического программирования наиболее используемым был Пролог, то использование данной парадигмы мы проследим именно по нему. 

Разработка языка Prolog началась в 1970 году Аланом Колмерауэром и Филиппом Русселом. Они хотели создать язык, который мог бы делать логические выводы на основе заданного текста. Название Prolog является сокращением от «PROgramming in LOGic». Этот язык был разработан в Марселе в 1972 году. Принцип SL-резолюции казался подходящей моделью, на основе которой можно было разработать механизм логических выводов. С ограничением резолюции на дизъюнкт Хорна унификация привела к эффективной системе, где непреодолимый недетерминизм обрабатывался с помощью процесса отката, который мог быть легко реализован. Алгоритм резолюции позволял создать выполняемую последовательность, необходимую для реализации спецификаций, подобных приведенному выше отношению.

Первая реализация языка Prolog с использованием компилятора Вирта ALGOL-W была закончена в 1972 году, а основы современного языка были заложены в 1973. Использование языка Prolog постепенно распространялось среди тех, кто занимался логическим программированием, в основном благодаря личным контактам, а не через коммерциализацию продукта. В настоящее время существует несколько различных, но весьма схожих между собой версий. Хотя стандарта языка Prolog не существует, однако, версия, разработанная в Эдинбургском университете, стала наиболее широко используемым вариантом. Недостаток разработок эффективных приложений Prolog сдерживал его распространение вплоть до 1980 года.

Стоит заметить, что во многих областях применение Пролога может сократить время разработки программ, так как в целом писать, отлаживать и читать программы на нём при должном умении проще, чем на традиционных языках. Вследствие этого Пролог успешно применялся в таких областях, как символьное решение уравнений, планирование, базы данных, автоматическое решение задач, машинное макетирование, реализация языков программирования, дискретное и аналоговое моделирование, архитектурное проектирование, машинное обучение, распознавание естественного языка, экспертные системы и другие задачи искусственного интелекта. На настоящий момент почти все эти ниши заняты другими, более современными, в большинстве своём не логическими языками программирования. 

Пролог был взят за основу проекта компьютеров пятого поколения - правительственной программы Японии. К сожалению, он не поддерживал параллельные вычисления, и им пришлось разрабатывать свой язык. Было предложено несколько вариантов, но у каждого из них были свои ограничения, и проблема "параллельного Пролога" так и не была решена. В конечном итоге эта крупномасштабная программа закончилась провалом, и мы не имеет суперкомпьютеров, поддерживающих как основную парадигму логического программирования.

#### "Буран"

Интересным применением логического программирования является написание программ для советского орбитального корабля-ракетоплана "Буран" на языке специально созданном проблемно-ориентированном языке программирования реального времени ПРОЛ2, во многом повторяющем Пролог, но построенный на базе русских служебных слов. Аппарат совершил свой полёт 15 ноября 1988 года. 

> Для управления процессом посадки, помимо наземных средств контроля и управления, использовалась собственная БЦВМ (Бортовая цифровая вычислительная машина) «Бурана» «Бисер-4». Военный заказ определил архитектуру БЦВМ — она была реализована в виде четырех параллельных независимых вычислительных каналов и компаратора, который непрерывно сравнивал результаты на выходе каналов. В случае отклонения результатов какого-либо из каналов от трех остальных, он отключался и БЦВМ продолжала работать в штатном режиме. Таким же образом мог быть отключен еще один поврежденный вычислительный канал, чем достигалось автоматическое резервирование и отказоустойчивость БЦВМ. Вычислительные каналы (или ядра, в современной терминологии) работали на частоте 4 МГц и имели 128 КБайт оперативной памяти и 16 КБайт постоянной программной памяти. Подобная архитектура позволяла БЦВМ управлять процессом посадки «Бурана» даже в условиях ядерной войны (это входило в ТЗ по требованию военных).

> Для программирования процесса посадки был выбран метод конечного приближения. В каждом цикле работы БЦВМ строила прогноз «попадания» корабля в заданную точку в зависимости от его текущего положения, скорости, состояния атмосферы на трассе посадки и множества других параметров, и если результаты прогноза расходились с необходимой для успешной посадки точкой — выдавались команды управления для того, чтобы скорректировать траекторию. Такой цикл повторялся вплоть до выхода корабля на финальную точку этапа посадки.

> «Буран» приближался к аэродрому несколько правее оси посадочной полосы, все шло к тому, что он будет «рассеивать» остаток энергии на юго-восточном подходе. Так думали специалисты и летчики-испытатели, дежурившие на объединенном командно-диспетчерском пункте. Однако при выходе в ключевую точку с высоты 20 км «Буран» «заложил» маневр, повергший в шок всех находившихся в командном пункте. Вместо ожидавшегося захода на посадку с юго-востока с левым креном корабль энергично отвернул влево, и стал заходить на ВПП с северо-восточного направления с креном 45º на правое крыло.

> Послеполетный анализ показал, что вероятность выбора такой траектории была менее 3%, однако в сложившихся условиях это было самое правильное решение бортовых компьютеров корабля. [^1]
    
### Использование концепций логического программирования в наши дни

В наши дни широкое применение аспектов логического программирования можно увидеть не только на примере Пролога. Например, SQL - это декларативный язык запросов, а поиском ответов занимается СУБД. Другим примером могут являться ASP-системы, современный подход, при котором пользователь фокусируется на декларативном представлении своей проблемы, а задача решается самой программой. Зачастую он применяется для решения NP-полных задач. Кроме того есть интересные вариации Пролога: ProbLog - вероятностная реализация, где предикат может оказаться верным или ложным с определённой вероятностью. 

Что касается времени исполнения программ, в настоящее время существуют решения, позволяющие компилировать Пролог-код, вследствие чего скорость выполнения улучшается. Конечно, они всё также будут уступать по скорости программам на более быстрых языках. Кроме того, иногда вместо того, чтобы долго придумывать как реализовать переборное решение на императивном языке, можно использовать подход логического программирования и за сравнительно короткое время написать прототип программы на Прологе. Затем его можно будет переписать на любой другой язык, обладая лучшим пониманием того, как подходить к данной задаче.

#### Веб-приложения Пролога

В настоящее время большая часть компьютерных технологий завязана на использовании интернета. Приложения, не знающие о том, что такое "сеть", не обретут популярность на рынке. Следовательно современному языку программирования необходимо уметь поддерживать стек сетевых технологий. 

Маркус Триска, автор книги "The power of Prolog" утверждает, что этот Пролог является подходящим языком для разработки веб-приложений. Веб-страницы естественно представляются термами Пролога и могут легко создаваться, проверяться и рекурсивно обрабатываться. Есть разные диалекты этого языка, поставляющие библиотеки для веб-разработки. Таковым, к примеру, является реализация SWI-Prolog.

##### HTTP клиенты

Клиенты получают страницы с серверов. На SWI-Prolog можно получить страницу по HTTP или HTTPS используя http_open/3. Результатом является открытие потока на чтение веб-страницы. Затем можно распарсить html-код в термы Пролога, которые отображают html-структуру. Это можно сделать посредством предиката load_html/3. Далее страницу можно обрабатывать используя стандартные средства языка. Однако для решения даже этой задачи есть реализованный предикат из library(xpath): xpath/3.

##### HTTP серверы

На SWI-Prolog также легко писать HTTP и HTTPS серверы. 

Для примера, можно написать элементарный HTTP-сервер таким образом:
```prolog
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_unix_daemon)).

:- http_handler(/, handle_request, []).

handle_request(_Request) :-
    format("Content-type: text/plain~n~n"),
    format("Hello!").
```
В этом коде используется библиотека демонов Unix для реализации сервера, который просто отвечает `Hello!` на каждый запрос. Эта библиотека дополняет код опциями, которые можно использовать в командной строке. 

Узнать больше об опциях можно, запустив файл с этим кодом в терминале: 
```bash
swipl server.pl --help
```

Например, можно запустить сервер на 3040 порте следующим образом:
```bash
swipl server.pl --port=3040 --interactive
```

Когда сервер будет запущен, можно проверить его работу, перейдя в браузере по адресу http://127.0.0.1:3040.

В реальном HTTP-сервере ответ должен зависеть от фактического запроса клиента. Библиотеки сервера предоставляют проанализированный запрос в виде списка элементов Name(Value), которые могут быть легко обработаны в Прологе.

Используя различные дополнительные директивы http_handler/3, можно изменить поведение сервера. Например, чтобы обслуживать файлы из текущего каталога, нужно использовать этого следующие директивы:
```prolog
:- use_module(library(http/http_files)).
:- http_handler(root(.), http_reply_from_files('.', []), [prefix]).
```

Есть несколько вариантов для вывода html-страниц:
* Можно просто отправить любой ответ на стандартный вывод, как в примере выше. Это зачастую подверженный багам подход, так как позволяет легко забыть написать закрывающие теги и т.п.
* Можно отправить содержимое существующего HTML-файла с помощью http_reply_file/3.
* Можно использовать библиотеку (http/html_write) для создания HTML-страниц из термов Пролога.

##### Клиент-серверные приложения

[Pengines][6] позволяет реализовывать мощные клиент-серверные приложения на SWI-Prolog. Используя Pengines можно обращаться к удалённому серверу на Прологе так, как если бы он был локальным, используя его результаты в клиентских Пролог-программах или веб-страницах с JavaScript.
Важно отметить, что логика приложения полностью находится на сервере. 

В 2018 году вышла книга, посвящённая веб-программированию на Прологе.[^2]
 

#### Применение пролога для целей бизнеса

##### Пролог как база данных

Один из основных вариантов применения Пролога - реализация базы данных. Под этим подразумеваются приложения, которые хранят данные, доступные для запросов. 

Пролог хороше подходит для таких приложений, так как можно динамически добавлять и удалять данные, а также полагаться на автоматическую индексацию для достижения хорошей производительности во многих ситуациях. Кроме того, можно легко редактировать и обрабатывать Пролог-программы, что делает экспорт, импорт и обработку накопленных данных в целом очень удобным. Также формулирование запросов на Прологе часто более естественно и просто, чем формулирование запросов SQL. По этим причинам Пролог является хорошей альтернативой другим системам баз данных. 

Существует диалект Пролог, которое называется Datalog, особенно подходящее для использования в таких случаях. Кроме того можно использовать Constraint Query Language CQL для беспрепятственного доступак к базам данных SQL используя Пролог.

##### Анализ событий и аномалий

Лог-файл записывает события. Например, можно настроить Пролог-сервер HTTPS для хранения информации о запросах, которые он обрабатывает.

Когда вы локально тестируете такой сервер, выбирая страницу /prolog, он может написать в своем лог-файле текст, аналогичный следующему:
```prolog
/*Sun Apr  9 23:01:19 2017*/
request(45, 1491771679.213,
         [peer(ip(127,0,0,1)),
          method(get),request_uri('/prolog'),path('/prolog'),
          http_version(1-1),host(localhost),port(3037),
          connection('keep-alive'),upgrade_insecure_requests('1'),
          if_modified_since('Wed, 05 Apr 2017 23:24:08 GMT')]).
```

Эта запись описывает запрос клиента.

После обработки запроса сервер может написать дополнительную запись, которая описывает, как был обработан запрос. Например, он может добавить:
```prolog
completed(45, 0.0009790000000000076, 0, 304, not_modified).
```
Ключевым моментом является то, что вы можете настроить сервер таким образом, чтобы файл журнала представлял собой серию фактов Пролога. Следовательно, весь лог-файл может быть легко проанализирован с помощью Пролога!

Одного вызова предиката Пролога достаточно для загрузки всего файла, и в то же время он готов к публикации произвольных запросов к накопленным данным.

Например, в приведенном выше случае мы можем спросить: есть ли какие-либо клиентские запросы, которые не были обработаны? В Прологе это становится:
```prolog
?- request(R, _, _), \+ completed(R, _, _, _, _).
false.
```

В нашем случае это означает, что сервер по крайней мере каким-то образом обрабатывал каждый запрос и не игнорировал один из них.

Конечно, не все лог-файлы могут быть проанализированы напрямую с помощью Пролога. Однако чем больше вы используете синтаксис Пролога в своей организации, тем больше вы сможете получить возможность отправлять запросы на такие данные. Кроме того, вы обычно можете легко преобразовать любой формат файла журнала в действительные факты Пролога, а затем применить те же рассуждения.

##### Малоизвестный язык

Иногда, когда вы вводите Пролог в организации, люди отвергают язык, потому что никогда не слышали о ком-либо, кто его использует. Тем не менее, треть всех авиабилетов обрабатывается системами, в которых работает SICStus Prolog. НАСА использует SICStus Prolog для системы голосового управления на борту Международной космической станции. Windows NT использовала встроенный интерпретатор Prolog для настройки сети. Доминирующая фондовая система Новой Зеландии написана на Прологе и CHR.

#### Некоторые интересные применения

Примером современного использования логического программирования в реальных проектах может послужить система контроля версий [Gerrit][1]. Правила обработки запросов на изменения (change) и сливания кода (merge) в ней реализованы на языке Пролог.

Логическое программирование применияется также для систем проверки доказательств: многие Proof Assistant, например [Isabelle][2], используют внутри вариант логического программирования.

### Современные реализации языков логического программирования 

1. [SWI-Prolog][3] - одна из лучших реализаций классического Пролога на настояший момент. Эта реализация предоставляет богатый набор возможностей, библиотеки для constraint logic programming, многопоточности, юнит-тестирования, GUI, интерфейс к языку программирования Java, ODBC и т. д., поддерживает литературное программирование, содержит реализацию веб-сервера, библиотеки для SGML, RDF, RDFS, средства разработчика (включая IDE с графическими отладчиком и профилировщиком), и обширную документацию.

2. [Visual Prolog][4] - объектно-ориентированное расширение языка программирования PDC Prolog, развивавшегося из Turbo Prolog (Borland), семейства Prolog, а также система визуального программирования датской фирмы Prolog Development Center.

3. [λProlog][5] - это язык логического программирования, основанный на интуиционистском фрагменте простой теории типов Черча, который включает полиморфную типизацию, модульное программирование, абстрактные типы данных, поддержку синтаксиса лямбда-дерева и программирование более высокого порядка.

### Выводы

Несмотря на то, что парадигма логического программирования в наше время является менее распространённой, чем та же функциональная, она тоже развивается в сторону поддержки разработки современных систем. Да, времена когда на Прологе и Лиспе хотели писать искуственный интеллект прошли, однако он остаётся применимым в других областях современных компьютерных технологий, такие как веб-программирование или разработка клиент-серверных приложений. На нём всё так же удобно писать разборы грамматик, вследствии чего просто реализовывать экспертные системы, а также множество других задач по обработке языковых конструкций.

Следует отметить, что развитие Пролога происходит в основном в академическом кругу. Таким образом, статьи по применению его в промышленном программировании довольно редкое явление, поэтому Прологу не суждено снискать популярность в коммерческих проектах. 

На этом всё. Спасибо за путешествие в мир логического программирования.

### Список литературы
1. Парадигма логического программирования. Сошников Д.В. (2006)
2. The Power of Prolog. Introduction to modern Prolog. Markus Triska (2005)
3. Программирование на языке Пролог для искусственного интеллекта. И. Братко (1990)
4. Logic Programming. Frank Pfenning. (2007)
5. The early years of logic programming. Robert A. Kowalski. (1988)
6. Статьи с Википедии. https://en.wikipedia.org/

### Ссылки
[1]: https://gerrit-documentation.storage.googleapis.com/Documentation/2.16.4/prolog-cookbook.html#HowToWriteSubmitRules "Gerrit. How to write submit rules."
[2]: https://isabelle.in.tum.de/
[3]: https://www.swi-prolog.org/
[4]: https://www.visual-prolog.com/
[5]: http://www.lix.polytechnique.fr/~dale/lProlog/
[6]: http://eu.swi-prolog.org/pldoc/doc_for?object=section%28%27packages/pengines.html%27%29
### Сноски
[^1]: https://habr.com/en/post/86876/
[^2]: https://github.com/Web-Prolog/swi-web-prolog/blob/master/book/web-prolog.pdf
