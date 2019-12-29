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

En été 1995, mon ami Robert Morris et moi avons lancé une startup qui
s'appelait Viaweb. Notre plan était de développer un logiciel permettant aux
utilisateurs de créer des magasins en ligne. Ce qui était nouveau pour ce
logiciel, à l'époque, c'est qu'il fonctionnait sur notre serveur en
utilisant des pages Web ordinaires comme interface.

Beaucoup de monde aurait pu avoir cette idée à ce moment-là, mais pour
autant que je sache, Viaweb était la première application Web-based. Cela
nous a semblé une idée si innovante que nous avons nommé notre société
d'après elle : Viaweb, parce que notre logiciel marchait via le Web, au lieu
de fonctionner sur votre ordinateur de bureau.

Une autre chose inhabituelle par rapport à ce logiciel était le fait qu'il a
été écrit en un langage de programmation qui s'appelle Lisp. En fait,
c'était une des premières grandes applications à être écrite en Lisp, qui
jusque-là avait été utilisé principalement par les universités et les
laboratoires de recherche[1](footnote:1).

## L'arme secrète

Eric Raymond a rédigé un essai intitulé *Comment devenir un Hacker* et dans
celui-ci, entre autres, il dit aux hackers potentiels quels langages ils
doivent apprendre. Il suggère de commencer par Python et Java, car ils sont
faciles à apprendre. Le hacker sérieux voudra aussi apprendre C, pour hacker
Unix, et Perl pour l'administration des systèmes et des scripts CGI. Enfin,
le hacker véritablement sérieux doit considérer d'apprendre Lisp :

> Lisp vaut la peine d'être appris pour l'expérience de l'illumination
  profonde qu'on aura quand on l'a finalement appris ; cette expérience fera
  de vous un meilleur programmeur pour le reste de vos jours, même si vous
  n'utiliserez jamais beaucoup Lisp.

C'est le même argument que vous avez tendance à entendre parler de
l'apprentissage du Latin. Ça ne vous donnera pas de travail, sauf peut-être
en tant que professeur de lettres classiques, mais il améliora votre esprit
et ferra de vous un meilleur écrivain en langues que vous voulez utiliser,
comme l'anglais.

Mais attendez un moment. Cette métaphore ne va pas si loin. La raison pour
laquelle le latin ne vous trouvera pas un emploi est que personne ne le
parle. Si vous écrivez en latin, personne ne peut vous comprendre. Mais Lisp
est un langage de programmation, et les ordinateurs parlent quel que soit le
langage que vous leur dites de parler.

Donc si Lisp fait de vous un meilleur programmeur, pourquoi ne voudriez-vous
pas l'utiliser ? Si un peintre se voyait offrir un pinceau qui pourrait
faire de lui un meilleur peintre, il me semble qu'il veuille l'utiliser dans
toutes leurs peintures, n'est-ce pas ? Je n'essaie pas de me moquer d'Eric
Raymond. Dans l'ensemble, ses conseils sont bons. Ce qu'il dit de Lisp est
plutôt sagesse conventionnelle. Mais il y a une contradiction dans la
sagesse conventionnelle : Lisp ferra de vous un meilleur programmeur et
pourtant vous n'allez pas l'utiliser.

Pourquoi pas ? Les langages de programmation ne sont que des outils, après
tout. Si Lisp produit vraiment de meilleurs programmes, vous devez
l'utiliser. Et sinon, qui en a besoin ?

Ce n'est pas juste une question théorique. Le développement de logiciels est
une activité très compétitive, sujet aux monopoles naturels. Une entreprise
qui écrit des logiciels plus rapidement et mieux, toutes choses égales par
ailleurs, mettra ses concurrents en faillite. Et lorsque vous démarrez une
startup, vous le ressentez très fort. Les startups ont tendance à être une
proposition tout ou rien. Soit vous devenez riche, soit vous n'obtenez rien.
Dans une startup, si vous misez sur la mauvaise technologie, vos concurrents
vous écraseront.

Robert et moi connaissions bien Lisp, et nous ne pouvions voir aucune raison
de ne pas faire confiance à notre instinct et ne pas aller avec Lisp. On
savait que tous les autres écrivaient leurs logiciels en C++ et Perl. Mais
on savait aussi que ça ne faisait rien. Si vous choisissiez la technologie
de cette façon, vous utiliseriez Windows. Quand on choisit la technologie,
il faut ignorer ce que les autres font et considérer seulement ce qui
fonctionnera le mieux.

C'est particulièrement vrai dans les startups. Dans une grande entreprise on
peut faire ce que les autres grandes entreprises font. Mais une startup ne
peut pas faire ce que les autres startups font. Je ne crois pas que beaucoup
de gens s'en rendent compte, même dans les startups.

Le grande entreprise moyenne grandit d'environ 10% par an. Donc si vous
dirigez une grande entreprise et faites tout comme une grande entreprise
moyenne le fait, vous pouvez vous attendre à faire aussi bien que le grande
entreprise moyenne — c'est-à-dire à grandir par d'environ 10% par an.

Bien sûr, la même chose arrivera si vous dirigez une startup. Si vous faites
tout comme une startup moyen, vous pouvez anticiper performance moyenne. Le
problème est, la performance moyenne signifie que vous ferrez faillite. Le
taux de survie pour startups est beaucoup moins de cinquante pourcent. Ça
veut dire que si vous dirigez une startup, vous ferriez mieux de faire
quelque chose de bizarre. Sinon, vous avez des problèmes.

En 1995, nous savions quelque chose que je ne crois pas que nos concurrents
comprenaient, et peu comprennent même maintenant : quand vous écrivez
logiciel qui ne va fonctionner que sur vos propres serveurs, vous pouvez
utiliser un langage de programmation de votre choix. Quand vous écrivez
logiciel de bureau, il y a un fort biais pour l'écrire en le même langage
que le système d'exploitation. Il y a dix ans, écrire des applications
signifiait écrire des applications en C. Mais avec un logiciel basé sur le
Web, surtout si vous avez code source du langage et du système
d'exploitation, vous pouvez utiliser un langage de votre choix.

Cette nouvelle liberté est cependant une arme à double tranchant. Maintenant
que vous pouvez utiliser n'importe quel langage, vous devez déterminer
lequel utiliser. Les entreprises qui essayent de prétendre que rien n'a
changé risquent de trouver que leurs concurrents ne le pensent pas.

Si vous pouvez utiliser n'importe quel langage, quel langage vous utilisez?
Nous avions choisi Lisp. D'un côté, c'était évident que développement rapide
serait important sur ce marché. Nous tous partions de zéro, ainsi, une
entreprise qui pourrait obtenir de nouvelles fonctionnalités avant ses
concurrents aurait un gros avantage. On sait que Lisp était un langage
vraiment bien pour écrire logiciel vite, et les applications basés sur des
serveurs amplifient l'effet de développement rapide car vous pouvez publier
un logiciel la minute il est fait.

Si les autres entreprises n'utilisaient pas Lisp, tant mieux. Cela pourrait
nous donner un avantage technologique, et on avait besoin de toute aide
qu'on pouvait obtenir. Quand on a lancé Viaweb, on n'a aucune expérience en
affaires. On ne savait rien à propos de marketing, ou embauche, ou collecte
de fonds, ou obtention de clients. Aucun de nous n'avait même jamais eu ce
que vous appelleriez un travail vrai. La seule chose dans laquelle nous
étions bons était l'écriture de logiciels. On espérait que ça nous
sauverait. Tout avantage que nous pourrions obtenir dans le département de
logiciel, nous prendrions.

Donc, vous pourriez dire que Lisp était une expérience. Note hypothèse était
que si on écrivait notre logiciel en Lisp, on pourrait obtenir des
fonctionnalités plus rapidement que nos concurrents et faire des choses
qu'ils ne pouvaient pas faire. Et car Lisp est si haut niveau, nous
n'aurions pas besoin d'une grande équipe de développement, donc nos coûts
seraient plus bas. Si c'était le cas, on pourrait offrir un meilleure
produit pour moins d'argent, et toujours faire un profit. Nous finirions par
avoir tous les utilisateurs, nos concurrents n'en auraient aucun, et
finiraient par faire faillite. C'est ce qu'on espérait.

Quels ont été les résultats de cette expérience ? Assez étonnamment, cela a
fonctionné. Nous avons finalement eu beaucoup de concurrents, de l'ordre de
vingt à trente d'eux, mais aucun de leurs logiciels ne pourrait rivaliser
avec le nôtre. Nous avions un WYSIWYG constructeur de magasin en ligne qui
fonctionnait sur le serveur et pourtant ressentait comme un application de
bureau. Nos concurrents avaient des scripts CGI. Nous étions toujours loin
devant eux en termes de fonctionnalités. Des fois, en désespoir de cause,
les concurrents tentaient d'introduire des fonctionnalités qu'on n'avait
pas. Mais avec Lisp notre cycle de développement était si rapide qu'on
pouvait des fois dupliquer une nouvelle fonctionnalité dans un délai d'un
jour ou deux après un communiqué de presse. Au moment où les journalistes
couvrant le communiqué de presse nous appelaient, nous aurions aussi la
nouvelle fonctionnalité.

Cela a dû sembler à nos concurrents que nous avions une sorte d'arme secrète
— que nous décodions leur trafic Enigma ou quelque chose du genre. En effet,
on avait une arme secrète, mais elle était plus simple qu'ils pensaient.
Personne ne nous faisait part de leurs nouvelles. Nous pouvions simplement
développer des logiciels plus rapidement que quiconque ne le pensait.

Lorsque j'avais 9 ans, je suis arrivé à obtenir une copie de *The Day of the
Jackal* de Frederick Forsyth. Le personnage principale est un assassin qui
est engagé pour tuer le président de la France. L'assassin doit passer
devant la police pour se lever à un appartement qui surplombe la route du
président. Il passe à côté d'eux, déguisé en vieillard avec des béquilles,
et ils ne le soupçonnent jamais.

Notre arme secrète était similaire. Nous écrivions notre logiciel en un
langage AI étrange, avec une syntaxe bizarre pleine de parenthèses. Pendant
des années, cela m'énervait d'entendre parler que Lisp soit décrit comme ça.
Mais maintenant, cela a fonctionné à notre avantage. En affaires, rien ne
vaut plus qu'un avantage technique que vos concurrents ne comprennent pas.
En affaires, comme en guerre, la surprise vaut autant que la force.

Et donc, je suis un peu gêné de dire que je n'avait jamais rien dit
publiquement à propos de Lisp pendent que nous travaillions sur Viaweb. On
n'en parlait jamais à la presse, et si vous cherchiez Lisp sur notre site
Web, vous ne trouveriez que les titres de deux livres dans ma bio. Ce
n'était pas un accident. Une startup doit donner ses concurrents le moins
d'informations possible. S'ils ne saivaient pas en quel langage notre
logiciel était écrit ou ne s'en souciaient pas, je voulais que ça reste
ainsi[2](footnote:2).

Ce sont nos clients qui comprenaient le mieux notre technologie. Ils se
fichaient du langage en lequel Viaweb était écrit, mais ils ont remarqué
qu'il fonctionnait très bien. Ça leur a permis de créer de superbes magasins
en ligne en quelques minutes. Et donc, de bouche à oreille surtout, nous
avions eu plus d'utilisateurs. À la fin de 1996, nous avions environ 70
magasins en ligne. À la fin de 1997, nous avions 500. Six moins après, quand
Yahoo nous a acheté, nous avions 1070 utilisateurs. Aujourd'hui, comme Yahoo
Store, ce logiciel continue de dominer son marché. C'est une des pièces les
plus rentables de Yahoo, et les magasins construits avec ça sont la base de
Yahoo Shopping. J'ai quitté Yahoo en 1999, et donc je ne sait pas exactement
combien utilisateurs ils ont au moment, mais la dernière fois que j'ai
entendu parler, il y en avait environ 20 000.

## Le paradoxe de Blub

En quoi Lisp est-il si génial ? Et si Lisp est si génial, pourquoi tout le
monde ne l'utilise pas ? Celles questions-ci ont l'air rhétorique, mais en
fait, il y a des réponses simples. Lisp est tellement génial non pas à cause
d'une certaine qualité magique qui est visible uniquement aux passionnés,
mais parce qu'il est simplement le langage le plus puissant disponible. Et
la raison pour laquelle tout le monde ne l'utilise pas, c'est que les
langages de programmation ne sont pas simplement des technologies, mais
habitudes d'esprit, et rien ne change plus lentement. Sans doute, les deux
réponses ont besoin d'explications.

Je vais commencer par une déclaration scandaleusement controversée: les
langages de programmation varient en puissance.

Peu contesteraient, au moins, que les langages de haut niveau soient plus
puissant que le langage machine. Aujourd'hui, la plupart des programmeurs
conviendraient que vous ne voulez pas, en règle générale, programmer en
langage machine. Au lieu de cela, vous devez programmer dans un langage de
haut niveau et demander à un compilateur de le traduire en langage machine
pour vous. Cette idée fait même maintenant partie intégrante de hardware :
depuis des années 1980, les jeux d'instructions ont été conçus pour les
compilateurs plutôt que pour les programmeurs.

Tout le monde sait que c'est une erreur d'écrire tout votre programme à la
main en langage machine. Ce qui est moins souvent compris, c'est qu'il y a
un principe plus général : si vous avez le choix entre plusieurs langages,
toutes choses étant égales par ailleurs, c'est une erreur de programmer en
tous les langages sauf le plus puissant[3](footnote:3).

Il y a de nombreuses exceptions à cette règle. Si vous devez écrire un
programme qui va travailler en étroite collaboration avec un programme
écrite en un certain langage, ça peut être une bonne idée d'écrire le
nouveau programme en le même langage. Si vous écrivez un programme qui doit
faire quelque chose de simple, comme le calcul des nombres ou la
manipulation des bits, vous pouvez aussi utiliser un langage moins abstrait,
d'autant plus qu'il peut être légèrement plus rapide. Et si vous écrivez un
programme court et jetable, vous pouvez être mieux en utilisant simplement
le langage qui a les meilleurs fonctions pour la tâche. Mais en général,
pour les logiciels d'application, vous voulez utiliser le langage le plus
puissant (raisonnablement efficace) possible, et utiliser un autre langage
est une erreur, exactement du même genre, mais peut-être à une moindre
degré, que la programmation en langage machine.

Vous pouvez voir que le langage machine est de très bas niveaux. Mais, au
moins comme une sorte de convention sociale, les langages de haut niveau
sont souvent tous traités comme équivalents. Ils ne sont pas. Techniquement,
le terme « le langage de haut niveau » ne signifie rien de très précis. Il
n'y a pas de ligne de démarcation avec les langages machine d'un côté et
tous les langages de haut niveau de l'autre. Les langages s'inscrivent dans
un continuum[4](footnote:4) d'abstraction, des plus puissants aux langages
machine, dont la puissance varie.

Considérez Cobol. Cobol est un langage de haut niveau, dans le sens où il
est compilé en langage machine. Quelqu'un pourrait-il sérieusement affirmer
que Cobol a un pouvoir équivalent à, disons, Python ? Il est probablement
plus proche au langage machine que Python.

Ou que diriez-vous de Perl 4 ? Entre Perl 4 et Perl 5, les fermetures
lexicales ont été ajoutée au langage. La plupart des hackers Perl
conviendraient que Perl 5 est plus puissant que Perl 4. Mais une fois que
vous l'avez admis, vous avez admis que qu'un langage de haut niveau peut
être plus puissant qu'un autre. Et il s'ensuit inexorablement que, sauf dans
des cas particuliers, vous devez utiliser le plus puissant possible.

Cependant, cette idée est rarement suivie jusqu'à sa conclusion. Après un
certain âge, les programmeurs changent rarement de langages volontairement.
Quel que soit le langage auquel les gens sont habitués, ils ont tendance à
le considérer juste assez bon.

Les programmeurs sont très attachés à leurs langages préférés, et je ne veux
blesser personne, donc pour expliquer ce point je vais utiliser un langage
hypothétique appelé Blub. Blub tombe au milieu du continuum de
l'abstraction. Il n'est pas le langage le plus puissant, mais il est plus
puissant que Cobol ou le langage machine.

En fait, notre programmeur Blub hypothétique n'utiliserait aucun d'eux. Bien
sûr, il ne programme pas en langage machine. C'est à cela que servent les
compilateurs. Et quant à Cobol, il ne sait pas comment on peut arriver à
rien avec ça. Cobol n'a même pas X (fonctionnalité de Blub de votre choix).

Tant que notre hypothétique programmeur Blub regarde vers le bas du
continuum de puissance, il sait qu'il regarde vers le bas. Les langages
moins puissants que Blub sont évidemment moins puissants, car il leur manque
une fonctionnalité à laquelle il est habitué. Mais quand notre hypothétique
programmeur Blub regarde dans l'autre sens, vers le haut du continuum de
puissance, il ne se rend pas compte de ce qu'il regarde. Ce qu'il voit n'est
que des langages étranges. Il les considère probablement comme équivalents
en puissance à Blub, mais avec tous ces autres trucs bizarres ajoutés. Blub
est assez bon pour lui, car il pense à Blub.

Lorsque nous passons au point de vue d'un programmeur utilisant un des
langages plus haut dans le continuum du puissance, cependant, nous
constatons qu'il à son tour, méprise Blub. Comment on peut arriver à rien
rien avec Blub ? Il n'a même pas Y.

Par induction, les seuls programmeurs en mesure de voir toutes les
différences de pouvoir entre les différents langages sont ceux qui
comprennent le plus puissant. (C'est probablement ce que Eric Raymond
voulait dire à propos de Lisp qui faisait de vous un meilleur programmeur.)
Vous ne pouvez pas faire confience aux avis des autres, à cause du paradoxe
Blub : ils sont satisfaits du langage qu'ils utilisent, car cela dicte leur
façon de penser aux programmes.

Je le sait de ma propre expérience, comme un lycéen qui écrivait ses
programmes en Basic. Ce langage ne supportait même pas la récursivité. C'est
difficile d'imaginer écrire des programmes sans utiliser la récursivité,
mais je ne l'ai pas manqué à l'époque. Je pensait en Basic. Et j'étais un
génie. Maître de tout ce que j'ai interrogé.

Les cinq langages que Eric Raymond recommande aux hackers tombent à
différents points du continuum de puissance. Où les tombent les uns par
rapport aux autres est un sujet sensible. Cependant, je pense que Lisp est
au sommet. Pour soutenir cette affirmation, je vais vous parler d'une des
choses que je trouve manquantes lorsque je regarde les quatre autres
langages. Je pense à moi : comment on peut arrive à rien avec eux, sans
macros[5](footnote:5) ?

Plusieurs langages ont quelques choses qui s'appellent une « macro ». Mais
les macro de Lisp sont uniques. Et croyez-le ou non, ce qu'ils font est lié
aux parenthèses. Les concepteurs de Lisp n'ont pas mis toutes ces
parenthèses dans le langage juste pour être différents. Pour le programmeur
Blub, le code Lisp semble bizarre. Mais ces parenthèses sont là pour une
raison. Elle sont la preuve extérieure d'une différence fondamentale entre
Lisp et les autres langages.

Le code Lisp est constitué d'objets de données Lisp. Et pas dans le sens
trivial que les fichiers source contiennent des caractères, et les strings
sont un des types de données soutenus par le langage. Le code Lisp, après
avoir été lu par l'analyseur est constitué de structures de données que vous
pouvez parcourir.

Si vous comprenez comment fonctionnent les compilateurs, ce qui se passe
vraiment n'est pas tant que Lisp a une syntaxe étrange mais que Lisp n'a pas
de syntaxe. Vous écrivez des programmes dans les arbres d'analyse qui sont
générés dans le compilateur lorsque d'autres langages sont analysés. Mais
ces arbres d'analyse sont entièrement accessibles à vos programmes. Vous
pouvez écrire des programmes qui les manipulent. En Lisp, ces programmes
s'appellent macros. Ils sont les programmes qui écrivent des programmes.

Les programmes qui écrivent des programmes ? Quand voudriez-vous faire ça ?
Pas très souvent si vous pensez en Cobol. Tout le temps, si vous pensez en
Lisp. Ce serait utile si je pourrais donner un exemple d'un marco puissant
et dire voilà ! comment ça ? Mais si je le faisais, ça ressemblerait à du
charabia à quelqu'un qui ne connaissait pas Lisp. Il n'y a pas de place ici
pour expliquer tout ce que vous devez savoir pour comprendre ce que cela
signifie. Dans [ANSI Common Lisp][ansi-common-lisp] je tentais de faire
avancer les choses aussi vite que possible, et même ainsi, je n'ai pas pu
arriver aux macros jusqu'à la page 160.

Mais je pense que je peux donner une sorte d'argument qui pourrait être
convaincant. Le code source de l'éditeur de Viaweb consistait sans doute
d'environ 20 à 25% de macros. Macros sont plus difficiles à écrire que les
fonctions ordinaires, et il est considéré un mauvais style de les utiliser
quand ils ne sont pas nécessaires. Donc, chaque macro dans ce code est là
car il doit y être. Cela signifie qu'au moins 20 à 25% du code de ce
programme fait de choses que vous ne pouvez pas facilement faire dans un
autre langage. Aussi sceptique que le programmeur Blub puisse être sur mes
prétentions pour les pouvoirs mystérieux de Lisp, cela devrait le rendre
curieux. Nous n'écrivions pas ce code pour notre propre plaisir. Nous étions
une tout petite startup, programmant aussi dur que possible pour mettre des
barrières techniques entre nous et nos concurrents.

Une personne suspecte pourrait commencer à se demander s'il avait une
corrélation ici. Une grande partie de notre code faisait des choses qui sont
très difficiles à faire dans d'autres langues. Le logiciel résultant faisait
des chose que les logiciels de nos concurrents ne pouvaient pas faire.
Peut-être qu'il y avait une sorte de lien. Je vous encourage à suivre ce
fil. Il y a peut-être plus que ne discerne l'œil.

## Aïkido pour Startups

Mais je ne m'attends pas à convaincre quiconque ([plus de 25 ans][scheme])
d'apprendre Lisp. Le but de cet article n'est pas de change d'avis, mais de
rassurer les personnes déjà intéressées par l'utilisation de Lisp — les
personnes qui savent que Lisp est un langage puissant, mais s'inquiètent
parce que il n'est pas largement utilisé. Dans une situation
concurrentielle, c'est un avantage. La puissance de Lisp est multipliée par
le fait que vos concurrents ne la comprennent pas.

Si vous songez à utiliser Lisp dans une startup, vous ne devriez pas vous
inquiéter qu'il ne soit pas largement compris. Vous devriez espérer que cela
reste ainsi. Et c'est probable. C'est la nature des langages de
programmation de rendre la plupart de gens satisfaits de tout ce qu'ils
utilisent actuellement. Le hardware change tellement plus rapidement que les
habitudes personnelles que la pratique de la programmation a généralement
dix à vingt ans en retard par rapport au processeur. Dans les intitutions
comme le MIT, les gens écrivaient des programmes dans des langages haut
niveau au début des années 1960, mais de nombreuses entreprises ont continué
à écrire du code en langage machine jusque dans les années 1980. Je parie
que beaucoup de gens ont continué à écrire du langage machine jusqu'à ce que
le processeur, comme un barman désireux de fermer et rentrer chez lui, les
ait finalement expulsés en passant à un ensemble d'instructions RISC.

Habituellement, la technologie change rapidement. Mais les langages de
programmation sont différents : les langages de programmation ne sont pas
seulement une technologie, mais ce en quoi les programmeurs pensent. Ils
sont à moitié une technologie et à moitié une religion[6](footnote:6). Et
donc le langage médian, c'est-à-dire quel que soit le langage utilisé par le
programmeur médian, se déplace aussi lentement qu'un iceberg.

Ramasse-miettes, introduite par Lisp vers 1960 est maintenant largement
considéré comme une bonne chose. Runtime typing, idem, gagne en popularité.
Les fermetures lexicales, introduites par Lisp au début des années 1970,
sont maintenant, à peine, sur l'écran radar. Les macros, introduits par Lisp
au milieu des années 60, sont toujours terra incognita.

Évidemment, le langage médian a un élan énorme. Je ne propose pas que vous
puissiez combattre cette force puissante. Ce que je propose est exactement
contraire : comme un pratiquant d'Aïkido, vous pouvez l'utiliser contre vos
adversaires.

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
chaque moins environ. La première chose que je ferais, après avoir vérifié
s'ils avaient une démo en ligne en direct, était de consulter leurs listes
d'emploi. Après quelques années, je pourrais savoir desquelles entreprises
se soucier et desquelles ne pas se soucier (LAID!). Plus des descriptions de
poste avaient une saveur informatique, mois l'entreprise était dangereuse.
Les types les plus saufs étaient ceux qui voulaient une expérience Oracle.
Vous ne deviez jamais vous soucier de ceux-ci. Vous étiez également en
sécurité s'ils disaient qu'ils voulaient des développeurs C++ ou Java. S'ils
voulaient des programmeurs Perl ou Python, ce serait un peu effrayant — cela
commence à ressembler à une entreprise où le côté technique, au moins, est
géré par de vrais hackers. Si j'avais déjà vu une offre d'emploi à la
recherche de hackers Lisp, j'aurais été très inquiet.

## Notes

> footnotes

  1. Au début, Viaweb comportait deux parties : l'éditeur, écrit en Lisp,
     que les gens utilisaient pour construire leurs sites, et le système de
     commandes, écrit en C, qui gérait les commandes. La première version
     était surtout Lisp, parce que le système de commandes était petit. Plus
     tard, nous avons ajouté deux modules supplémentaires : un générateur
     d'images écrit en C, et un responsable back-office écrit surtout en
     Perl.

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

  3. Tous les langages sont également puissants dans le sens d'être
     équivalentes à Turing, mais ce n'est pas le sens du mot auquel les
     programmeurs se intéressent. (Personne ne veut programmer une machine
     Turing.) Le type de pouvoir qui intéresse les programmeurs peut ne pas
     être formellement définissable, mais une façon de l'expliquer ce serait
     de dire qu'il réfère à des fonctionnalités que vous ne pouvez obtenir
     dans le langage moins puissant que en écrivant un interpréteur pour le
     langage plus puissant. Si le langage A a un opérateur pour suppression
     d'espaces des strings et langage B n'en a pas, ça ne rend probablement
     pas A plus puissant, parce que vous pouvez sûrement écrire un
     sous-programme pour le faire en B. Mais si A soutien, disons,
     récursivité et B ne la soutien pas, ce n'est pas probable que quelqu'un
     peut le corriger en écrivant des fonctions de bibliothèque.

  4. Note aux nerds : ou peut-être le treillis se rétrécissant vers le haut
     ; ce n'est pas la forme qui compte ici mais l'idée qu'il y a au moins
     un ordre partiel.

  5. C'est un peu déroutant de traiter les macros comme une fonctionnalité
     distincte. En pratique, leur utilité est grandement améliorée par les
     autres caractéristique de Lisp comme fermetures lexicales et les rest
     paramètres.

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
