---
title: Battre les moyennes
desc: Une traduction française de l'article bien connu de Paul Graham.
date:
  published: December 31, 2019
from_lang: en
to_lang: fr
original: http://paulgraham.com/avg.html
---

*(Cet article est dérivé d'un discours donné au 2001 Franz Developer
symposium.)*

À l'été 1995, mon ami Robert Morris et moi avons lancé une startup nommée
Viaweb. Notre plan était de développer un logiciel permettant aux
utilisateurs de créer des boutiques en ligne. La nouveauté de ce logiciel, à
l'époque, est qu'il fonctionnait sur notre serveur en utilisant des pages
Web ordinaires comme interface.

Beaucoup de monde aurait pu avoir cette idée à ce moment-là, mais pour
autant que je sache, Viaweb était la première application Web-based. Cette
idée nous a semblé une idée si innovante que nous avons baptisé notre
société d'après elle : Viaweb, parce que notre logiciel marchait via le Web,
au lieu de fonctionner sur votre ordinateur de bureau.

Une autre chose inhabituelle concernant ce logiciel était le fait qu'il a
été écrit dans un langage de programmation appelé Lisp. En fait, c'était une
des premières grandes applications à être écrite en Lisp, qui jusque-là
avait été utilisé principalement par les universités et les laboratoires de
recherche[1](footnote:1).

## L'arme secrète

Eric Raymond a rédigé un essai intitulé *Comment devenir un Hacker* dans
leqeul, entre autres, il dit aux hackers potentiels quels langages ils
doivent apprendre. Il suggère de commencer par Python et Java, car ils sont
faciles à apprendre. Le hacker sérieux voudra aussi apprendre C, pour hacker
Unix, et Perl pour l'administration des systèmes et des scripts CGI. Enfin,
le hacker véritablement sérieux doit considérer d'apprendre Lisp :

> Lisp vaut la peine d'être appris pour l'expérience de l'illumination
  profonde qu'on aura quand on l'a finalement appris ; cette expérience fera
  de vous un meilleur programmeur pour le reste de vos jours, même si vous
  n'utiliserez jamais beaucoup Lisp.

C'est le même argument dont vous avez tendance à entendre parler de
l'apprentissage du Latin. Ça ne vous donnera pas de travail, sauf peut-être
en tant que professeur de lettres classiques, mais il améliora votre esprit
et fera de vous un meilleur rédacteur dans les langues que vous voulez
utiliser, comme l'anglais.

Néanmoins la comparaison s'arrête là. La raison pour laquelle le latin ne
vous permettra pas de trouver un emploi est que personne ne le parle. Si
vous écrivez en latin, personne ne peut vous comprendre. Mais Lisp est un
langage de programmation, et les ordinateurs parlent n'importe quel langage
que vous leur dites de parler.

Donc si Lisp fait de vous un meilleur programmeur, pourquoi ne voudriez-vous
pas l'utiliser ? Si un peintre se voyait offrir un pinceau qui pourrait
faire de lui un meilleur peintre, il me semble qu'il voudrait l'utiliser
pour toutes ses peintures, n'est-ce pas ? Je n'essaie pas de me moquer
d'Eric Raymond. Dans l'ensemble, ses conseils sont bons. Ce qu'il dit de
Lisp est plutôt empreint de sagesse conventionnelle. Mais il y a une
contradiction dans la sagesse conventionnelle : Lisp fera de vous un
meilleur programmeur et pourtant vous n'allez pas l'utiliser.

Pourquoi pas ? Les langages de programmation ne sont que des outils, après
tout. Si Lisp produit vraiment de meilleurs programmes, vous devez
l'utiliser. Et sinon, qui en a besoin ?

Ce n'est pas juste une question théorique. Le développement de logiciels est
une activité très compétitive, sujet aux monopoles naturels. Une entreprise
qui écrit des logiciels plus rapidement et mieux, toutes choses égales par
ailleurs, mettra ses concurrents en faillite. Et lorsque vous démarrez une
startup, vous le ressentez très nettement. Les startups ont tendance à être
un jeu de quitte ou double. Soit vous devenez riche, soit vous n'obtenez
rien. Dans une startup, si vous misez sur la mauvaise technologie, vos
concurrents vous écraseront.

Robert et moi connaissions bien Lisp, et nous ne pouvions voir aucune raison
de ne pas faire confiance à notre instinct et ne pas travailler avec Lisp.
On savait que tous les autres écrivaient leurs logiciels en C++ et Perl.
Mais on savait aussi que ça n'avait pas d'importance. Si vous choisissiez la
technologie de cette façon, vous utiliseriez Windows. Quand on choisit la
technologie, il faut ignorer ce que les autres font et considérer seulement
ce qui fonctionnera le mieux.

C'est particulièrement vrai dans les startups. Dans une grande entreprise on
peut faire ce que les autres grandes entreprises font. Mais une startup ne
peut pas faire ce que les autres startups font. Je ne crois pas que beaucoup
de gens s'en rendent compte, même dans les startups.

La grande entreprise moyenne grandit d'environ 10% par an. Donc si vous
dirigez une grande entreprise et faites tout comme une grande entreprise
moyenne le fait, vous pouvez vous attendre à faire aussi bien — à savoir
croître d'environ 10% par an.

Bien sûr, la même chose arrivera si vous dirigez une startup. Si vous faites
tout comme une startup moyenne, vous pouvez anticiper une performance
moyenne. Le problème est qu'une performance moyenne signifie que vous ferez
faillite. Le taux de survie des startups est largement inférieur à cinquante
pour cent. Ça veut dire que si vous dirigez une startup, vous feriez mieux
de faire quelque chose de non conventionnel. Sinon, vous avez des problèmes.

En 1995, nous savions quelque chose que je ne crois pas que nos concurrents
comprenaient, et que peu comprennent même maintenant : quand vous écrivez un
logiciel qui ne va fonctionner que sur vos propres serveurs, vous pouvez
utiliser le langage de programmation de votre choix. Quand vous écrivez un
logiciel de bureau, il y a un fort biais pour l'écrire dans le même langage
que le système d'exploitation. Il y a dix ans, écrire des applications
signifiait écrire des applications en C. Mais avec un logiciel basé sur le
Web, surtout si vous avez le code source du langage et du système
d'exploitation, vous pouvez utiliser le langage de votre choix.

Cette nouvelle liberté est cependant une arme à double tranchant. Maintenant
que vous pouvez utiliser n'importe quel langage, vous devez déterminer
lequel utiliser. Les entreprises qui essayent de prétendre que rien n'a
changé risquent de découvrir que leurs concurrents ne le pensent pas.

Si vous pouvez utiliser n'importe quel langage, quel langage utiliser ? Nous
avions choisi Lisp. D'un côté, il était évident qu'un développement rapide
serait important sur ce marché. Nous tous partions de zéro ; ainsi, une
entreprise qui pourrait obtenir de nouvelles fonctionnalités avant ses
concurrents aurait un gros avantage. On sait que Lisp était un langage
vraiment pertinent pour écrire un logiciel vite, et les applications basées
sur des serveurs amplifient l'effet de vitesse de développement car vous
pouvez publier un logiciel dans la minute suivant sa conception.

Si les autres entreprises n'utilisaient pas Lisp, tant mieux. Cela pourrait
nous donner un avantage technologique, et on avait besoin de toute l'aide
qu'on pouvait obtenir. Quand nous avons lancé Viaweb, nous n'avons aucune
expérience en affaires. On ne savait rien à propos du marketing, ou
recrutement, ou levée de fonds, ni de l'acquisition de clients. Aucun de
nous n'avait même jamais eu ce que vous appelleriez un vrai travail. La
seule chose dans laquelle nous étions bons était l'écriture de logiciels. On
espérait que ça nous sauverait. Tout avantage que nous pourrions obtenir en
matière d'ingénierie logicielle serait bon à prendre.

On pourrait donc dire que Lisp était une expérience. Notre hypothèse était
que si nous écrivons notre logiciel en Lisp, nous pourrions obtenir des
fonctionnalités plus rapidement que nos concurrents et faire des choses
qu'ils ne pouvaient pas faire. Et puisque Lisp est de si performant, nous
n'aurions pas besoin d'une grande équipe de développement, donc nos coûts
seraient plus bas. Si c'était le cas, nous pourrions offrir un meilleure
produit à moindre coût, et malgré tout être profitable. Nous finirions par
capter tous les utilisateurs, nos concurrents n'en auraient aucun, et
finiraient par faire faillite. C'est ce qu'on espérait.

Quels ont été les résultats de cette expérience ? Assez étonnamment, cela a
fonctionné. Nous avons finalement eu beaucoup de concurrents, de l'ordre de
vingt à trente, mais aucun de leurs logiciels ne pouvait rivaliser avec le
nôtre. Nous avions un éditeur WYSIWYG de boutique en ligne qui fonctionnait
sur le serveur et pourtant donnait l'impression d'être une application de
bureau. Nos concurrents avaient des scripts CGI. Nous étions toujours loin
devant eux en termes de fonctionnalités. Des fois, en désespoir de cause,
les concurrents tentaient d'introduire des fonctionnalités qu'on n'avait
pas. Mais avec Lisp notre cycle de développement était si rapide qu'on
pouvait des fois dupliquer une nouvelle fonctionnalité dans un délai d'un
jour ou deux après un communiqué de presse. Au moment où les journalistes
couvrant le communiqué de presse nous appelaient, nous avionsn nous aussi la
nouvelle fonctionnalité.

Nos concurrents ont dû penser que nous avions une sorte d'arme secrète — que
nous décodions leur trafic Enigma ou quelque chose du genre. Nous avions
effectivement une arme secrète, mais elle était plus évidente qu'ils
pensaient. Personne ne faisait part de leurs découvertes. Nous pouvions tout
simplement développer des logiciels plus rapidement que quiconque ne le
pensait.

Lorsque j'avais 9 ans, je suis arrivé à obtenir une copie de *The Day of the
Jackal* de Frederick Forsyth. Le personnage principal est un assassin engagé
pour tuer le président de la France. L'assassin doit passer devant la police
pour se rendre dans un appartement qui surplombe la route du président. Il
passe à côté d'eux, déguisé en vieillard avec des béquilles, sans qu'ils le
soupçonnent jamais.

Notre arme secrète était similaire. Nous écrivions notre logiciel dans un
langage AI étrange, avec une syntaxe bizarre pleine de parenthèses. Pendant
des années, cela m'énervait d'entendre Lisp ainsi décrit. Mais maintenant,
cela a fonctionné à notre avantage. En affaires, rien ne vaut plus qu'un
avantage technique que vos concurrents ne comprennent pas. En affaires,
comme en guerre, la surprise vaut autant que la force.

Ainsi, je suis un peu gêné d'admettre que je n'avais jamais rien dit
publiquement à propos de Lisp pendent que nous travaillions sur Viaweb. Nous
n'en parlions jamais à la presse, et si vous cherchiez une mention de Lisp
sur notre site Web, vous ne trouviez que les titres de deux livres dans ma
bio. Ce n'était pas un accident. Une startup doit donner à ses concurrents
le moins d'informations possible. S'ils ne savaient pas dans quel langage
notre logiciel était écrit ou ne s'en souciaient pas, je voulais qu'il en
reste ainsi[2](footnote:2).

Ce sont nos clients qui comprenaient le mieux notre technologie. Ils se
fichaient du langage dans lequel Viaweb était écrit, mais ils ont remarqué
qu'il fonctionnait très bien. Ça leur a permis de créer de superbes
boutiques en ligne en quelques minutes. Et donc, essentiellement grâce à
bouche à oreille surtout, nous avons eu de plus d'utilisateurs. Fin 1996,
nous avions environ 70 boutiques en ligne. Fin 1997, nous en avions 500. Six
mois après, quand Yahoo nous a acheté, nous avions 1070 utilisateurs.
Aujourd'hui, dénommé Yahoo Store, ce logiciel continue de dominer son
marché. C'est l'une des branches les plus rentables de Yahoo, et les
boutiques construits avec sont la base de Yahoo Shopping. Ayant quitté Yahoo
en 1999, et donc je ne sais pas exactement combien d'utilisateurs ils ont
actuellement, mais la dernière fois que j'entendais parler, il en était fait
mention d'environ 20 000.

## Le paradoxe de Blub

En quoi Lisp est-il si génial ? Et si Lisp est si génial, pourquoi tout le
monde ne l'utilise pas ? Ces questions ont l'air rhétorique, mais en fait,
il y a des réponses simples. Lisp est si génial pas tant du fait d'une
quelconque faculté magique qui ne serait visible des passionnés, mais parce
qu'il est simplement le langage le plus puissant disponible. Et la raison
pour laquelle tout le monde ne l'utilise pas, c'est que les langages de
programmation ne sont pas simplement des technologies, mais également des
habitudes de réflexion, et rien ne change plus lentement. Ces deux réponses
ont sans doute besoin d'explications.

Je vais commencer par une déclaration scandaleusement controversée : les
langages de programmation varient en puissance.

Peu contesteront, tout au moins, que les langages de haut niveau sont plus
puissants que le langage machine. Aujourd'hui, la plupart des programmeurs
conviendront qu'il n'est pas souhaitable, en règle générale, de programmer
en langage machine. Il est plutôt préférable de programmer dans un langage
de haut niveau et demander à un compilateur de le traduire en langage
machine pour vous. Cette idée fait même maintenant partie intégrante du
hardware : depuis les années 1980, les jeux d'instructions ont été conçus
pour les compilateurs plutôt que pour les programmeurs.

Tout le monde sait que c'est une erreur d'écrire tout son programme à la
main en langage machine. Ce qui est moins souvent compris, c'est qu'il y a
un principe plus général : si vous avez le choix entre plusieurs langages,
toutes choses égales par ailleurs, c'est une erreur de programmer dans tout
autre langage que le plus puissant[3](footnote:3).

Il y a de nombreuses exceptions à cette règle. Si vous devez écrire un
programme qui va travailler en étroite collaboration avec un programme écrit
en dans un langage donné, ce peut être une bonne idée d'écrire le nouveau
programme dans ce même langage. Si vous écrivez un programme qui doit faire
quelque chose de simple, comme du calcul numérique ou de la manipulation de
bits, vous pouvez aussi utiliser un langage moins abstrait, d'autant plus
qu'il peut être légèrement plus rapide. Et si vous écrivez un programme
court et jetable, il est préférable d'utiliser simplement le langage qui a
les meilleures fonctions pour la tâche attendue. Mais en général, pour les
logiciels d'application, vous voulez utiliser le langage le plus puissant
(raisonnablement efficace) possible, et utiliser un autre langage est une
erreur, exactement du même genre, mais peut-être à un moindre degré, que la
programmation en langage machine.

Il est admis que le langage machine est de très bas niveau. Mais, au moins
comme une sorte de convention sociale, les langages de haut niveau sont
souvent tous traités comme équivalents. Ils ne le sont pas. Techniquement,
le terme « langage de haut niveau » ne signifie rien de très précis. Il n'y
a pas de ligne de démarcation entre les langages machine d'un côté et tous
les langages de haut niveau de l'autre. Les langages s'inscrivent dans un
continuum[4](footnote:4) d'abstraction, des plus puissants aux langages
machine, dont la puissance varie.

Prenons le cas de Cobol. Cobol est un langage de haut niveau, dans le sens
où il est compilé en langage machine. Quelqu'un pourrait-il sérieusement
affirmer que Cobol a un pouvoir équivalent à, disons, Python ? Il est
probablement plus proche du langage machine que Python.

Ou qu'en serait-il de Perl 4 ? Entre Perl 4 et Perl 5, les fermetures
lexicales ont été ajoutées au langage. La plupart des hackers Perl
conviendront que Perl 5 est plus puissant que Perl 4. Mais une fois cela
admis, il est établi qu'un langage de haut niveau peut être plus puissant
qu'un autre. Et il s'ensuit inexorablement que, sauf dans des cas
particuliers, vous devez utiliser le plus puissant possible.

Cependant, cette idée est rarement suivie jusqu'à sa conclusion. Après un
certain âge, les programmeurs changent rarement de langages volontairement.
Quel que soit le langage auquel les gens sont habitués, ils ont tendance à
le considérer juste suffisant.

Les programmeurs sont très attachés à leurs langages préférés, et je ne veux
blesser personne, donc pour expliquer ce point je vais utiliser un langage
hypothétique appelé Blub. Blub tombe au milieu du continuum de
l'abstraction. Il n'est pas le langage le plus puissant, mais il est plus
puissant que Cobol ou le langage machine.

En fait, notre programmeur Blub hypothétique n'utiliserait aucun d'eux. Bien
sûr, il ne programme pas en langage machine. C'est à cela que servent les
compilateurs. Et quant à Cobol, il ne sait pas comment on peut arriver à
quoi que ce soit avec ça. Cobol n'a même pas X (fonctionnalité de Blub de
votre choix).

Tant que notre hypothétique programmeur Blub regarde vers le bas du
continuum de puissance, il sait qu'il regarde vers le bas. Les langages
moins puissants que Blub sont évidemment moins puissants, car il leur manque
une fonctionnalité à laquelle il est habitué. Mais quand notre hypothétique
programmeur Blub regarde dans l'autre sens, vers le haut du continuum de
puissance, il ne se rend pas compte de ce qu'il regarde. Il ne voit que des
langages étranges. Il les considère probablement comme équivalents en
puissance à Blub, mais avec tous ces autres trucs bizarres ajoutés. Blub est
assez bon pour lui, car il pense Blub.

Lorsque nous passons au point de vue d'un programmeur utilisant un des
langages plus haut dans le continuum de puissance, cependant, nous
constatons qu'à son tour, il méprise Blub. Comment peut-on arriver à quoi
que soit avec Blub ? Il n'a même pas Y.

Par induction, les seuls programmeurs en mesure de voir toutes les
différences de pouvoir entre les différents langages sont ceux qui
comprennent le plus puissant (c'est probablement ce que Eric Raymond voulait
dire à propos de Lisp qui ferait de vous un meilleur programmeur.) Vous ne
pouvez pas faire confiance aux avis des autres, à cause du paradoxe Blub :
ils sont satisfaits du langage qu'ils utilisent, car cela dicte leur façon
de penser aux programmes.

Je le sais de ma propre expérience, comme un lycéen qui écrivait ses
programmes en Basic. Ce langage ne supportait même pas la récursivité. Il
est difficile d'imaginer écrire des programmes sans utiliser la récursivité,
mais ça ne m'a pas manqué à l'époque. Je pensais en Basic. Et j'étais un
génie. Maître de tout ce que j'ai exploré.

Les cinq langages que Eric Raymond recommande aux hackers tombent à
différents points du continuum de puissance. Où ils tombent les uns par
rapport aux autres est un sujet sensible. Cependant, je pense que Lisp est
au sommet. Pour soutenir cette affirmation, je vais vous parler d'une des
choses que je trouve manquantes lorsque je regarde les quatre autres
langages. Comment peut-on arriver à quoi que ce soit avec eux, sans
macros[5](footnote:5) ?

Plusieurs langages ont quelque chose appelé une « macro ». Mais les macros
de Lisp sont uniques. Et croyez-le ou non, ce qu'ils font est lié aux
parenthèses. Les concepteurs de Lisp n'ont pas mis toutes ces parenthèses
dans le langage juste pour être différents. Pour le programmeur Blub, le
code Lisp semble bizarre. Mais ces parenthèses sont là pour une raison. Elle
sont la preuve explicite d'une différence fondamentale entre Lisp et les
autres langages.

Le code Lisp est constitué d'objets de données Lisp. Et pas dans le sens
trivial que les fichiers source contiennent des caractères, et les strings
sont un des types de données soutenus par le langage. Le code Lisp, après
avoir été lu par l'analyseur, est constitué de structures de données qu'on
peux parcourir.

Si vous comprenez comment fonctionnent les compilateurs, ce qui se passe en
fait n'est pas tant que Lisp a une syntaxe étrange mais que Lisp n'a pas de
syntaxe. Vous écrivez des programmes dans les arbres d'analyse qui sont
générés dans le compilateur lorsque d'autres langages sont analysés. Mais
ces arbres d'analyse sont entièrement accessibles à vos programmes. Vous
pouvez écrire des programmes qui les manipulent. En Lisp, ces programmes
s'appellent macros. Ils sont les programmes qui écrivent des programmes.

Des programmes qui écrivent des programmes ? Quand voudriez-vous faire ça ?
Pas très souvent si vous pensez en Cobol. Tout le temps, si vous pensez en
Lisp. Ce serait utile si je pouvais donner un exemple d'une marco puissante
et dire voilà ! Comment donc ? Mais si je le faisais, ça ressemblerait à du
charabia pour quiconque ne connaîtrait pas Lisp. Il n'y a pas de place ici
pour expliquer tout ce que vous devez savoir pour comprendre ce que cela
signifie. Dans [ANSI Common Lisp][ansi-common-lisp] je tentais de faire
avancer les choses aussi vite que possible, et même ainsi, je n'ai pu
arriver aux macros qu'à la page 160.

Mais je pense que je peux donner une sorte d'argument qui pourrait être
convaincant. Le code source de l'éditeur de Viaweb consistait sans doute en
environ 20 à 25% de macros. Les macros sont plus difficiles à écrire que les
fonctions ordinaires, et il n'est pas jugé bon de les utiliser quand elles
ne sont pas nécessaires. Chaque macro dans est donc présente dans ce code
est là car elle doit y être. Cela signifie qu'au moins 20 à 25% du code de
ce programme fait de choses que vous ne pouvez pas facilement faire dans un
autre langage. Aussi sceptique que le programmeur Blub puisse être sur mes
prétentions concernant les pouvoirs mystérieux de Lisp, cela devrait le
rendre curieux. Nous n'écrivions pas ce code pour notre propre plaisir. Nous
étions une toute petite startup, programmant aussi dur que possible pour
mettre des barrières techniques entre nous et nos concurrents.

Une personne suspicieuse pourrait commencer à se demander s'il y avait là
une corrélation. Une grande partie de notre code faisait des choses qui sont
très difficiles à faire dans d'autres langages. Le logiciel en résultant
faisait des choses que les logiciels de nos concurrents ne pouvaient pas
faire. Peut-être qu'il y avait un lien de quelque sorte. Je vous encourage à
suivre ce fil. Ce n'est peut-être que la partie visible de l'iceberg.

## Aïkido pour Startups

Mais je ne m'attends pas à convaincre quiconque ([plus de 25 ans][scheme])
d'apprendre Lisp. Le but de cet article n'est pas de change d'avis, mais de
rassurer les personnes déjà intéressées par l'utilisation de Lisp — les
personnes qui savent que Lisp est un langage puissant, mais s'inquiètent
parce qu'il n'est pas largement utilisé. Dans une situation concurrentielle,
c'est un avantage. La puissance de Lisp est multipliée par le fait que vos
concurrents ne la comprennent pas.

Si vous songez à utiliser Lisp dans une startup, vous ne devriez pas vous
inquiéter qu'il ne soit pas largement compris. Vous devriez espérer que cela
reste ainsi. Et c'est probable. C'est la nature des langages de
programmation de rendre la plupart de gens satisfaits de tout ce qu'ils
utilisent actuellement. Le hardware change tellement plus rapidement que les
habitudes personnelles que la pratique de la programmation a généralement
dix à vingt ans en retard par rapport au processeur. Dans les institutions
comme le MIT, les gens écrivaient des programmes dans des langages haut
niveau au début des années 1960, mais de nombreuses entreprises ont continué
à écrire du code en langage machine jusque dans les années 1980. Je parie
que beaucoup de gens ont continué à écrire du langage machine jusqu'à ce que
le processeur, comme un barman désireux de fermer et rentrer chez lui, les
ait finalement expulsés en passant à un ensemble d'instructions RISC.

Habituellement, la technologie change rapidement. Mais les langages de
programmation sont différents : les langages de programmation ne sont pas
seulement des technologies, mais ce en quoi les programmeurs pensent. Ils
sont à moitié des technologie et à moitié des religions[6](footnote:6). Et
donc le langage médian, c'est-à-dire le langage utilisé par le programmeur
médian, se déplace aussi lentement qu'un iceberg.

Les ramasse-miettes, introduite par Lisp vers 1960 sont maintenant largement
considéré comme une bonne chose. Le typage au runtime, de même, gagne en
popularité. Les fermetures lexicales, introduites par Lisp au début des
années 1970, sont maintenant, à peine, sur l'écran radar. Les macros,
introduites par Lisp au milieu des années 60, sont toujours terra incognita.

Évidemment, le langage médian a un élan énorme. Je ne propose pas que vous
puissiez combattre cette force puissante. Ce que je propose est exactement
le contraire : comme un pratiquant d'Aïkido, vous pouvez l'utiliser contre
vos adversaires.

Si vous travaillez pour une grande entreprise, cela peut ne pas être facile.
Vous aurez du mal à convaincre le pointy-haired boss (???) de vous laisser
construire des choses en Lisp quand il vient de lire dans le journal qu'un
autre langage s'apprête à conquérir le monde, comme Ada était il y a vingt
ans. Mais si vouz travaillez pour une startup qui n'a pas encore de
pointy-haired bosses, vous pouvez, comme nous l'avons fait, mettre à profit
le paradoxe de Blub : vous pouvez utiliser la technologie que vos
concurrents, collés immobiles au langage médiane, ne pourront jamais égaler.

Si jamais vous vous trouvez à travailler pour une startup, voici un conseil
pratique pour évaluer les concurrents. Lisez leurs listes d'emploi. Tout le
reste sur leur site peut être des photos de stock ou l'équivalent en prose,
mais les offres d'emploi doivent être précises, sinon ils trouveront les
mauvais candidats.

Pendant les années où nous avons travaillé sur Viaweb, j'ai lu beaucoup de
descriptions de poste. Un nouveau concurrent semblait émerger de menuiserie
chaque mois environ. La première chose que je fesais, après avoir vérifié
s'ils avaient une démo en ligne en direct, était de consulter leurs listes
d'emploi. Après quelques années, je pouvais savoir de quelles entreprises se
soucier et de quelles ne pas se soucier. Plus les descriptions de poste
avaient une saveur informatique, mois l'entreprise était dangereuse. Les
types les plus saufs étaient ceux qui voulaient une expérience Oracle. Vous
ne deviez jamais vous soucier de ceux-ci. Vous étiez également en sécurité
s'ils disaient qu'ils voulaient des développeurs C++ ou Java. S'ils
voulaient des programmeurs Perl ou Python, c'était un peu effrayant — cela
commence à ressembler à une entreprise où le côté technique, au moins, est
géré par de vrais hackers. Si j'avais déjà vu une offre d'emploi à la
recherche de hackers Lisp, j'aurais été très inquiet.

## Notes

> footnotes

  1. Au début, Viaweb comportait deux parties : l'éditeur, écrit en Lisp,
     que les gens utilisaient pour construire leurs sites, et le système de
     commandes, écrit en C, qui gérait les commandes. La première version
     était surtout en Lisp, parce que le système de commandes était petit.
     Plus tard, nous avons ajouté deux modules supplémentaires : un
     générateur d'images écrit en C, et un responsable back-office écrit
     surtout en Perl.

     En janvier 2003, Yahoo a publié une nouvelle version de l'éditeur écrit
     en C++ et Perl. Cependant, c'est difficile à dire si le programme n'est
     plus écrit en Lisp, parce que pour le traduire en C++ ils devaient
     littéralement écrire un interpréteur de Lisp : les fichiers source de
     tous les modèles générateurs de pages sont toujours, pour autant que je
     sache, du code Lisp (voir la [dixième règle de Greenspun][quotes]).

  2. Robert Morris dit que je n'avais pas besoin d'être si secret, parce que
     même si nos concurrents savaient qu'on utilisaient Lisp, ils n'aurait
     pas compris pourquoi : « s'ils étaient si intelligents, ils auraient
     déjà programmé en Lisp ».

  3. Tous les langages sont également puissants dans le sens où ils sont
     Turing-équivalentes, mais ce n'est pas le sens du mot auquel les
     programmeurs s'intéressent. (Personne ne veut programmer une machine
     Turing.) Le type de pouvoir qui intéresse les programmeurs peut ne pas
     être formellement définissable, mais une façon de l'expliquer serait de
     dire qu'il réfère à des fonctionnalités que vous ne pouvez obtenir dans
     le langage moins puissant qu'en écrivant un interpréteur pour le
     langage plus puissant. Si le langage A a un opérateur pour la
     suppression d'espaces des strings et langage B n'en a pas, ça ne rend
     probablement pas A plus puissant, parce que vous pouvez sûrement écrire
     un sous-programme pour le faire en B. Mais si A supporte, disons,
     récursivité et B ne la supporte pas, ce n'est pas probable que
     quelqu'un puisse le corriger en écrivant des fonctions de bibliothèque.

  4. Note aux nerds : ou peut-être le treillis se rétrécissant vers le haut
     ; ce n'est pas la forme qui compte ici mais l'idée qu'il y a au moins
     un ordre partiel.

  5. C'est un peu déroutant de traiter les macros comme une fonctionnalité
     distincte. En pratique, leur utilité est grandement améliorée par les
     autres caractéristique de Lisp comme fermetures lexicales et les
     paramètres rest.

  6. Par conséquent, les comparaisons des langages de programmation prennent
     la forme de guerres de religion ou de manuels de premier cycle si
     résolument neutres qu'ils sont vraiment des œuvres d'anthropologie. Les
     gens qui apprécient leur paix ou veulent la permanence, évitent le
     sujet. Mais la question n'est qu'à moitié religieuse ; il y a quelque
     chose qui mérite d'être étudié, surtout si vous voulez concevoir de
     nouveaux langages.

[ansi-common-lisp]: http://paulgraham.com/acl.html
[scheme]: http://www.trollope.org/scheme.html
[quotes]: http://paulgraham.com/quotes.html
