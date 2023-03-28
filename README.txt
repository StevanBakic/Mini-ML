{\rtf1\ansi\ansicpg1252\cocoartf2638
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;\f1\fswiss\fcharset0 Helvetica-BoldOblique;\f2\fswiss\fcharset0 Helvetica-Bold;
}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww27300\viewh15220\viewkind0
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural\partightenfactor0

\f0\fs24 \cf0 \

\f1\i\b PROJET MINI-ML STEVAN BAKI\uc0\u262  ET THOMAS SINAPI
\f0\i0\b0 \
\
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural\partightenfactor0

\f2\b \cf0 \ul \ulc0 Organisation du travail:
\f0\b0 \ulnone \
\
Nous avons choisi la deuxi\'e8me m\'e9thode de progression en travaillant dans un premier temps sur les fichiers lexer\'a0et\'a0parser pour l\'92analyse lexicale et syntaxique des fragments de codes. Nous avons donc travailler dans un second temps sur les fichiers typechecker et interpreter pour la v\'e9rification des types et l\'92interpr\'e9tation.\
\
De plus, nous avons eu un probl\'e8me de compilation et pour build sur la machine (Macbook) d\'92un des membres du bin\'f4me qui n\'92a pas pu \'eatre r\'e9gl\'e9 en s\'e9ance de TD. Nous avons donc travaill\'e9 ensemble en permanence sur une machine. L\'92avancement du projet a pu se faire en travaillant sur deux machines et en s\'92envoyant les morceaux de codes \'e0 faire compiler.\
Nous nous sommes malheureusement mal organis\'e9s en d\'e9but de projet, ce qui nous \'e0 mit dans une situation o\'f9 nous avions manqu\'e9 de temps dans les derni\'e8res semaines puisque nous avions nos examens en m\'eame temps. \
Nous aurions aim\'e9 aller plus loin.\
 \
\
Nous pouvons analyser, v\'e9rifier les types et interpr\'e9ter les fichiers : arith.mml ; compare.ml ; if.mml ; if.mml ; let.mml; fun.mml\
\
Malgr\'e9 de longues recherches, le fichier fact.mml nous donne une erreur et nous n\'92arrivons pas \'e0 comprendre pourquoi.\
On a \'e9galement essay\'e9 d\'92impl\'e9menter program mais nous n\'92avons r\'e9ussi \'e0 avoir rien de concret. \
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural\partightenfactor0
\cf0 \
\
\

\f2\b \ul Fichiers : 
\f0\b0 \ulnone \
\

\f1\i\b mml.ml :
\f0\i0\b0 \
\
Types utilis\'e9s dans le programme, pour les repr\'e9sentations du programme global, des fonctions, des valeurs et expressions manipul\'e9es par le programme.\
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\tx14007\tx14358\tx15616\tx15721\tx16263\tx16470\pardirnatural\partightenfactor0
\cf0 \
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural\partightenfactor0

\f1\i\b \cf0 mmllexer.ml : 
\f0\i0\b0 \
\
Analyseur lexical. \
Le fichier identifie les diff\'e9rents mots-cl\'e9s et symboles \'e0 reconna\'eetre\
\

\f1\i\b mmlparseur.ml : 
\f0\i0\b0 \
\
Analyseur syntaxique.\
Liste des lex\'e8mes \'e0 reconna\'eetre (d\'e9finis dans mmllexer.ml) et des ordres de priorit\'e9s\
D\'e9clarations des types, expressions, simples expressions, symboles unaires et binaires.\
\

\f1\i\b typechercker.ml :\
\

\f0\i0\b0 V\'e9rification des types pour les op\'e9rations binaires et unaires, les instructions( Let, Fun, If, App, Fix, Seq)\
\

\f1\i\b interpreter.ml : \
\

\f0\i0\b0 Interpretations des op\'e9rations binaires et unaires, les instructions( Let, Fun, If, App, Fix, Seq)\

\f1\i\b \
}
