CREATE TABLE lignes(
  "id" TEXT,
  "nomcourt" TEXT,
  "nomlong" TEXT,
  "nomfamillecommerciale" TEXT,
  "idparcoursprincipalaller" TEXT,
  "idparcoursprincipalretour" TEXT,
  "idbillettique" TEXT,
  "couleurligne" TEXT,
  "couleurtexteligne" TEXT
);
CREATE TABLE dessertes(
  "idparcours" TEXT,
  "libellecourtparcours" TEXT,
  "idligne" TEXT,
  "nomcourtligne" TEXT,
  "idarret" TEXT,
  "nomarret" TEXT,
  "ordre" NUMBER,
  "estmonteeautorisee" BOOLEAN,
  "estdescenteautorisee" BOOLEAN
);
CREATE TABLE parcours(
  "id" TEXT,
  "code" TEXT,
  "idligne" TEXT,
  "nomcourtligne" TEXT,
  "sens" TEXT,
  "senscommercial" TEXT,
  "type" TEXT,
  "libellelong" TEXT,
  "idarretdepart" TEXT,
  "nomarretdepart" TEXT,
  "idarretarrivee" TEXT,
  "nomarretarrivee" TEXT,
  "estaccessiblepmr" BOOLEAN,
  "parcours" TEXT,
  "longueur" TEXT,
  "couleurtrace" TEXT,
  "geo_point_2d" TEXT
);
CREATE TABLE passages(
  "idligne" TEXT,
  "nomcourtligne" TEXT,
  "sens" TEXT,
  "destination" TEXT,
  "idarret" TEXT,
  "nomarret" TEXT,
  "coordonnees" TEXT,
  "arriveetheorique" TEXT,
  "departtheorique" TEXT,
  "arrivee" TEXT,
  "depart" TEXT,
  "idcourse" TEXT,
  "idbus" TEXT,
  "numerobus" TEXT,
  "precision" TEXT
);
