# Invoca Jags desde R
##
#library (rjags)
library (runjags)
library (foreign)
library (car)
library (gtools)
library (MCMCpack)
#library (snowfall)
#library (rlecuyer)
#library(arm)

rm(list = ls())
##
workdir <- c("~/Dropbox/data/rollcall/dipMex")
#workdir <- c("d:/01/Dropbox/data/rollcall/dipMex")
#workdir <- c("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/rollcall/dipMex")
#workdir <- c("C:/Documents and Settings/emm/Mis documentos/My Dropbox/data/rollcall/dipMex")
setwd(workdir)
##
set.seed(1970)

load(file = paste(workdir, "votesForWeb", "rc61.RData", sep = "/"))
dipdat$id <- as.character(dipdat$id)
dipdat$part <- as.character(dipdat$part)

### IMPORT DIPUTADO INFO (PREPARED IN EXCEL): MOST LIKELY REDUNDANT BEC rc60.RData HAS IT
#setwd(paste(workdir, "/diputados", sep=""))
#dipdat <- read.csv("dip61.csv", header=TRUE)
#setwd(workdir)

### UTIL PARA SABER QUIEN ES QUIEN
#ord	n	nom	id	part
# ord    n                                                  nom       id  part
#   1    1                          Arámbula López José Antonio   ags01p   pan
#   2  501                             Macías González Maricela   ags01s   pan
#   3    2                               Hernández Vallín David   ags02p   pri
#   4  502                            Verdugo Sarabia Guadalupe   ags02s   pri
#   5    3                           Cuadra García Raúl Gerardo   ags03p   pan
#   6  503            Díaz de León Macías María Matilde Maricel   ags03s   pan
#   7  301                              Gallegos Soto Margarita agsrp01p   pri
#   8  801                          Flemate Ramírez Julio Cesar agsrp01s   pri
#   9  302                                 González Ulloa Nancy agsrp02p   pan
#  10  802                              Sánchez Barba José Juan agsrp02s   pan
#  11  303                         Reynoso Femat Ma. de Lourdes agsrp03p   pan
#  12  803                               Del Conde Ugarte Jaime agsrp03s   pan
#  13    4                             Tolento Hernández Sergio    bc01p   pan
#  14  504                      Briceño Cinco Amintha Guadalupe    bc01s   pan
#  15    5                       Orduño Valdez Francisco Javier    bc02p   pan
#  16  505                                  Cano Valadez Mónica    bc02s   pan
#  17    6                               Mancillas Amador César    bc03p   pan
#  18  506                                  Marín Durán Alfredo    bc03s   pan
#  19    7                         Cortez Mendoza Jesús Gerardo    bc04p   pan
#  20  507                          Flores Álvarez Jesús Miguel    bc04s   pan
#  21    8                                   Luken Garza Gastón    bc05p   pan
#  22  508                                Barone Barrón Cecilia    bc05s   pan
#  23    9                          Osuna Millán Miguel Antonio    bc06p   pan
#  24  509                        Osuna Osuna Guillermo Antonio    bc06s   pan
#  25   10                              Ovando Patrón José Luis    bc07p   pan
#  26  510                        Carreño Castro Héctor Armando    bc07s   pan
#  27   11                           Arce Paniagua Óscar Martín    bc08p   pan
#  28  511                     Contreras Sánchez Belén Angélica    bc08s   pan
#  29  304                       Ponce Beltrán Esthela de Jesús  bcrp01p   pri
#  30  804                Galicia Ávila Víctor Manuel Anastasio  bcrp01s   pri
#  31  305                                   Lepe Lepe Humberto  bcrp02p   pri
#  32  805                               Valencia Alonso Corina  bcrp02s   pri
#  33  306                              Bahena Flores Alejandro  bcrp03p   pan
#  34  806                              Ramos Hernández Claudia  bcrp03s   pan
#  35  307                    Vega De Lamadrid Francisco Arturo  bcrp04p   pan
#  36  807                            Picazo Olmos María Olivia  bcrp04s   pan
#  37  308                                 Ledesma Romo Eduardo  bcrp05p  pvem
#  38  808                             San Román Flores Mariano  bcrp05s  pvem
#  39  309                         Pérez de Alva Blanco Roberto  bcrp06p panal
#  40  809                       Nevarez Pulido Isabel Cristina  bcrp06s panal
#  41   12                Covarrubias Villaseñor Marcos Alberto   bcs01p   prd
#  42  512                                Puppo Gastélum Silvia   bcs01s   prd
#  43   13                           Castro Cosío Víctor Manuel   bcs02p   prd
#  44  513                            Graciano Chávez Edilberto   bcs02s   prd
#  45  310                               González Cuevas Isaías bcsrp01p   pri
#  46  810                                 Porras Valles Gloria bcsrp01s   pri
#  47  311                        Meza Castro Francisco Armando bcsrp02p   prd
#  48  811                           León Mendivil José Antonio bcsrp02s   prd
#  49   14                        Pacheco Castro Carlos Oznerol   cam01p   pri
#  50  514                               Cuevas Sonia Jaqueline   cam01s   pri
#  51   15                           Rosas González Óscar Román   cam02p   pri
#  52  515   Curmina Cervera Margarita Beatriz de la Candelaria   cam02s   pri
#  53  312                      Kidnie de la Cruz Víctor Manuel camrp01p   pri
#  54  812                     Merino Capellini Giacomina María camrp01s   pri
#  55  313                      Márquez Zapata Nelly del Carmen camrp02p   pan
#  56  813                      Aguirre Montalvo Víctor Enrique camrp02s   pan
#  57  314                    Montalvo López Yolanda del Carmen camrp03p   pan
#  58  814                     Estrella Ramírez Víctor Santiago camrp03s   pan
#  59  315                            Seara Sierra José Ignacio camrp04p   pan
#  60  815                        Herrera Pérez Ileana Jannette camrp04s   pan
#  61   16                            Saracho Navarro Francisco   coa01p   pri
#  62  516                               Villarreal Pérez Sonia   coa01s   pri
#  63   17                        Martínez González Hugo Héctor   coa02p   pri
#  64  517                                Durán Piña Ana Isabel   coa02s   pri
#  65   18                         Sánchez de la Fuente Melchor   coa03p   pri
#  66  518                      Gutiérrez Burciaga Lilia Isabel   coa03s   pri
#  67   19                         Moreira Valdez Rubén Ignacio   coa04p   pri
#  68  519                         González Soto Diana Patricia   coa04s   pri
#  69   20                          Riquelme Solís Miguel Ángel   coa05p   pri
#  70  520                               Rodarte Ayala Josefina   coa05s   pri
#  71   21                             Fernández Aguirre Héctor   coa06p   pri
#  72  521                                Mata Vega Ilse Paloma   coa06s   pri
#  73   22                                  Franco López Héctor   coa07p   pri
#  74  522                       De la Rosa Cortés Lily Fabiola   coa07s   pri
#  75  316                        Flores Escalera Hilda Esthela coarp01p   pri
#  76  816                            Garza Flores Noé Fernando coarp01s   pri
#  77  317                                Medina Ramírez Tereso coarp02p   pri
#  78  817                   Cabrera Muñoz Ma. Dolores Patricia coarp02s   pri
#  79  318                                 Ramírez Rangel Jesús coarp03p   pan
#  80  818                         Valdés González Mayra Lucila coarp03s   pan
#  81  319                                Vives Preciado Tomasa coarp04p   pan
#  82  819                            Oyervides Thomas Fernando coarp04s   pan
#  83  320                       Guajardo Villarreal Mary Telma coarp05p   prd
#  84  820                      Cárdenas Cisneros Lydia Maribel coarp05s   prd
#  85  321                             Martínez Peña Elsa María coarp06p panal
#  86  821                       Medina Morales Gloria Angélica coarp06s panal
#  87   23                        Morán Sánchez Leoncio Alfonso   col01p   pan
#  88  523                                   Suárez Záizar Omar   col01s   pan
#  89   24                                  Cruz Mendoza Carlos   col02p   pri
#  90  524                                Carlos Pérez Patricia   col02s   pri
#  91  322                              Ceballos Llerenas Hilda colrp01p   pri
#  92  822                                 Ruíz Zubieta Maríano colrp01s   pri
#  93  323                         Cortés León Yulenny Guylaine colrp02p   pan
#  94  823                       Partida Valencia Jesús Alberto colrp02s   pan
#  95  324                                  Peralta Rivas Pedro colrp03p   pan
#  96  824                                Castell Ibáñez Amalia colrp03s   pan
#  97  325                                Vizcaíno Silva Indira colrp04p   prd
#  98  825                                 López Mejía Priscila colrp04s   prd
#  99   25                          López Fernández Juan Carlos   cps01p   prd
# 100  525                              Lara Aguilar Jorge Luis   cps01s   prd
# 101   26                        Orantes López Hernán de Jesús   cps02p   pri
# 102  526                                   Ruíz Gómez Armando   cps02s   pri
# 103   27                                  Hernández Cruz Luis   cps03p   prd
# 104  527                               Jiménez Jiménez Arturo   cps03s   prd
# 105   28                                Cortazar Ramos Ovidio   cps04p   pan
# 106  528                             Hernández Estrada Lesvia   cps04s   pan
# 107   29                                 Lobato García Sergio   cps05p   pri
# 108  529                                   Sántiz Ruíz Manuel   cps05s   pri
# 109   30                       Camacho Pedrero Mirna Lucrecia   cps06p   pan
# 110  530                      Rosales Franco Rodrígo Trinidad   cps06s   pan
# 111   31                         Marroquín Toledo José Manuel   cps07p   pan
# 112  531                    Hernández Gordillo José Francisco   cps07s   pan
# 113   32                      Albores Gleason Roberto Armando   cps08p   pri
# 114  532                                 Narváez Ochoa Moisés   cps08s   pri
# 115   33                                     Gómez León Ariel   cps09p   prd
# 116  533                               Miranda Borraz Bersaín   cps09s   prd
# 117   34                  Gutiérrez Villanueva Sergio Ernesto   cps10p   prd
# 118  534                      Cruz Magdaleno Flor del Rosario   cps10s   prd
# 119   35                             Martínez Martínez Carlos   cps11p   pan
# 120  535                            Tanus Piñasoria José Foad   cps11s   pan
# 121   36                                     David David Sami   cps12p   pri
# 122  536                        Toledo Zebadúa Alejandra Cruz   cps12s   pri
# 123  326                                 Rojas Ruiz Ana María cpsrp01p   pri
# 124  826                                 Nazar Morales Julián cpsrp01s   pri
# 125  327                       Santiago Ramírez César Augusto cpsrp02p   pri
# 126  827                             Benítez Tiburcio Mariana cpsrp02s   pri
# 127  328                            Luna Ruiz Gloria Trinidad cpsrp03p   pan
# 128  828                  Rodríguez Cal y Mayor César Augusto cpsrp03s   pan
# 129  329                         Ramos Cárdenas Liev Vladimir cpsrp04p   pan
# 130  829                        Calderón Orantes Eder Oswaldo cpsrp04s   pan
# 131  330                                   Gil Zuarth Roberto cpsrp05p   pan
# 132  830                             Valls Esponda Maricarmen cpsrp05s   pan
# 133  331                            Espinosa Morales Olga Luz cpsrp06p   prd
# 134  831                      Esquinca Cancino Carlos Enrique cpsrp06s   prd
# 135  332                              Ovalle Vaquera Federico cpsrp07p   prd
# 136  832                             Bagdadi Estrella Abraham cpsrp07s   prd
# 137  333                              Torres Abarca Magdalena cpsrp08p   prd
# 138  833                        Pola Figueroa Elvira de Jesús cpsrp08s   prd
# 139  334                      Espinosa Ramos Francisco Amadeo cpsrp09p    pt
# 140  834                         Roblero Gordillo Héctor Hugo cpsrp09s    pt
# 141   37                               Flores Castañeda Jaime   cua01p   pri
# 142  537                        Sáenz Calderón Maritza Olivia   cua01s   pri
# 143   38                    Murguía Lardizábal Héctor Agustín   cua02p   pri
# 144  538                           Zapata Lucero Ana Georgina   cua02s   pri
# 145   39                          Pérez Reyes María Antonieta   cua03p   pan
# 146  539                          Morán García Ernesto Alonso   cua03s   pan
# 147   40                              Terrazas Porras Adriana   cua04p   pri
# 148  540                                Tapia Cervantes César   cua04s   pri
# 149   41                     Márquez Lizalde Manuel Guillermo   cua05p   pri
# 150  541                            Sáenz Gabaldón Jesús José   cua05s   pri
# 151   42                                Ochoa Millán Maurilio   cua06p   pri
# 152  542                                  Aguilera García Liz   cua06s   pri
# 153   43                            Pérez Domínguez Guadalupe   cua07p   pri
# 154  543                               Le Baron González Alex   cua07s   pri
# 155   44                                Cano Ricaud Alejandro   cua08p   pri
# 156  544                            Soto Martínez María Adela   cua08s   pri
# 157   45                          Campos Villegas Luis Carlos   cua09p   pri
# 158  545                            Balderrama Quintana David   cua09s   pri
# 159  335                              Ortiz González Graciela cuarp01p   pri
# 160  835                          Silva Chacón Víctor Roberto cuarp01s   pri
# 161  336                      Aguilar Armendáriz Velia Idalia cuarp02p   pan
# 162  836                              Morales Mendoza Antonio cuarp02s   pan
# 163  337                                 Corral Jurado Javier cuarp03p   pan
# 164  837                       Cano Villegas Carmen Margarita cuarp03s   pan
# 165  338                               García Portillo Arturo cuarp04p   pan
# 166  838                               Cortez Palomares Hilda cuarp04s   pan
# 167   46                                  Jiménez López Ramón    df01p   prd
# 168  546                                Rubio Aldaran Eleazar    df01s   prd
# 169   47                                          Orozco Rosi    df02p   pan
# 170  547                           Quiroz Rocha Víctor Manuel    df02s   pan
# 171   48                                 Vargas Cortez Balfre    df03p   prd
# 172  548                                 Barba Méndez Ernesto    df03s   prd
# 173   49                       Cárdenas Gracia Jaime Fernando    df04p    pt
# 174  549                                Alonso Salgado Martín    df04s    pt
# 175   50                        González Madruga César Daniel    df05p   pan
# 176  550                        De Anda Martínez Erika Ivonne    df05s   pan
# 177   51                               Rebollo Vivero Roberto    df06p   pri
# 178  551                           Olvera Olguín Miguel Ángel    df06s   pri
# 179   52                             Norberto Sánchez Nazario    df07p   prd
# 180  552                               Borden Camacho Abraham    df07s   prd
# 181   53                               Llerenas Morales Vidal    df08p   prd
# 182  553                                 Quezada Ambriz Pablo    df08s   prd
# 183   54                               Damián Peralta Esthela    df09p   prd
# 184  554                            Nava Reyes Denise Lizbeth    df09s   prd
# 185   55                               Cuevas Barrón Gabriela    df10p   pan
# 186  555                                   Real Sánchez Jorge    df10s   pan
# 187   56                                    Piña Olmedo Laura    df11p conve
# 188  556                                   Vega Carrillo Sara    df11s conve
# 189   57                            Guerrero Castillo Agustín    df12p   prd
# 190  557                             González Cázares Agustín    df12s   prd
# 191   58                               Serrano Jiménez Emilio    df13p   prd
# 192  558                     Díaz Bustamante Angélica Adriana    df13s   prd
# 193   59                      Hernández Rodríguez Héctor Hugo    df14p   prd
# 194  559                               Pasalagua Olivar Ángel    df14s   prd
# 195   60                              Nava Vázquez José César    df15p   pan
# 196  560                            Tuachi Hurtado María Suad    df15s   pan
# 197   61                                 Robles Colín Leticia    df16p   pri
# 198  561                              García Cortés Francisca    df16s   pri
# 199   62                        Vázquez Camacho María Araceli    df17p   prd
# 200  562                               Ramírez Ortíz Gabriela    df17s   prd
# 201   63                             Mendoza Arellano Eduardo    df18p   prd
# 202  563                                   Muñoz Meza Rosario    df18s   prd
# 203   64                Fernández Noroña José Gerardo Rodolfo    df19p    pt
# 204  564                          Velázquez Rivera María Alma    df19s    pt
# 205   65                    Di Costanzo Armenta Mario Alberto    df20p    pt
# 206  565                       Mora Estrella Lilia del Carmen    df20s    pt
# 207   66                                Méndez Rangel Avelino    df21p   prd
# 208  566                               Sabas Rufino José Luis    df21s   prd
# 209   67                                Santana Alfaro Arturo    df22p   prd
# 210  567                     Olvera Palacios María del Carmen    df22s   prd
# 211   68                     Toledo Gutiérrez Mauricio Alonso    df23p   prd
# 212  568                           González Mata José Antonio    df23s   prd
# 213   69                             Rétiz Gutiérrez Ezequiel    df24p   pan
# 214  569                         De Jesús Sánchez Alin Nayely    df24s   pan
# 215   70                              Eguía Pérez Luis Felipe    df25p   prd
# 216  570                           Aguilar Meléndez Adalberto    df25s   prd
# 217   71                     Quiñones Cornejo María de la Paz    df26p   pri
# 218  571                       Guevara Rodríguez Miguel Ángel    df26s   pri
# 219   72                            Salgado Vázquez Rigoberto    df27p   prd
# 220  572                         Núñez Zúñiga Tereso de Jesús    df27s   prd
# 221  339                      Zambrano Grijalva José de Jesús  dfrp01p   prd
# 222  839                          Barraza Chávez Héctor Elías  dfrp01s   prd
# 223  340                             Báez Pinal Armando Jesús  dfrp02p   pri
# 224  840                  Guerra Díaz María del Rosario Elena  dfrp02s   pri
# 225  341                           García Ayala Marco Antonio  dfrp03p   pri
# 226  841                                Cerezo Bautista Adela  dfrp03s   pri
# 227  342                      Jiménez Case Fuensanta Patricia  dfrp04p   pri
# 228  842                     Gutiérrez de la Torre Cuauhtémoc  dfrp04s   pri
# 229  343                Lerdo de Tejada Covarrubias Sebastián  dfrp05p   pri
# 230  843                     Hernández Ledezma Laura Angélica  dfrp05s   pri
# 231  344                         Ruiz Massieu Salinas Claudia  dfrp06p   pri
# 232  844                             García Sarmiento Ernesto  dfrp06s   pri
# 233  345                    Castilla Marroquín Agustín Carlos  dfrp07p   pan
# 234  845                                Gómez Hernández Rocío  dfrp07s   pan
# 235  346                                Gutiérrez Cortina Paz  dfrp08p   pan
# 236  846                            Sáinz Martínez Juan Pablo  dfrp08s   pan
# 237  347                           Gutiérrez Fragoso Valdemar  dfrp09p   pan
# 238  847                      Marroquín Cisneros Digna Bertha  dfrp09s   pan
# 239  348                                  López Rabadán Kenia  dfrp10p   pan
# 240  848                            Saldaña Hernández Delfino  dfrp10s   pan
# 241  349                         Pérez Ceballos Silvia Esther  dfrp11p   pan
# 242  849                                  Tapia Zarate Gaspar  dfrp11s   pan
# 243  350                 Encinas Rodríguez Alejandro de Jesús  dfrp12p   prd  COORD PRD
# 244  850                             Mastache Mondragón Aarón  dfrp12s   prd
# 245  351                           Hernández Juárez Francisco  dfrp13p   prd
# 246  851                           Andalco López Luis Maríano  dfrp13s   prd
# 247  352                Incháustegui Romero Teresa del Carmen  dfrp14p   prd
# 248  852                                   Castro Corona Ruth  dfrp14s   prd
# 249  353                            Quezada Contreras Leticia  dfrp15p   prd
# 250  853                    Arciniega Álvarez Linda Guadalupe  dfrp15s   prd
# 251  354                          Uranga Muñoz Enoé Margarita  dfrp16p   prd
# 252  854                        Hinojosa Corona Claudia María  dfrp16s   prd
# 253  355                              Brindis Álvarez Rosario  dfrp17p  pvem
# 254  855               Rodríguez Botello Fierro Harry Gerardo  dfrp17s  pvem
# 255  356                               Escudero Morales Pablo  dfrp18p  pvem
# 256  856                            Arzaluz Alonso Alma Lucía  dfrp18s  pvem
# 257  357                             Salinas Sada Ninfa Clara  dfrp19p  pvem
# 258  857                        Buitrón González César Andrés  dfrp19s  pvem
# 259  358                          Castillo Juárez Laura Itzel  dfrp20p    pt
# 260  858                                Cervantes Vega Genaro  dfrp20s    pt
# 261  359                          Martínez Hernández Ifigenia  dfrp21p    pt
# 262  859                                 Hernández García Rey  dfrp21s    pt
# 263  360                                  Muñoz Ledo Porfirio  dfrp22p    pt
# 264  860                         Velasco Salazar Juan Alfonso  dfrp22s    pt
# 265  361                           Círigo Vasquez Víctor Hugo  dfrp23p conve
# 266  861                          Belaunzaran Méndez Fernando  dfrp23s conve
# 267  362                  Villarreal Benassini Karla Daniella  dfrp24p panal
# 268  862                             Del Mazo Morales Gerardo  dfrp24s panal
# 269  363                         Kahwagi Macari Jorge Antonio  dfrp25p panal  COORD PANAL
# 270  863                               Georgge Pérez Deyanira  dfrp25s panal
# 271   73                          López Pescador José Ricardo   dgo01p   pri
# 272  573                          Núñez Soto María del Carmen   dgo01s   pri
# 273   74                      Rebollo Mendoza Ricardo Armando   dgo02p   pri
# 274  574                         Guerrero Orona Isidra Oralia   dgo02s   pri
# 275   75                                  García Barrón Óscar   dgo03p   pri
# 276  575                             Aguilera Cháirez Maribel   dgo03s   pri
# 277   76                                Herrera Caldera Jorge   dgo04p   pri
# 278  576                                  Ávila Nevárez Pedro   dgo04s   pri
# 279  364                           De la Torre Valdez Yolanda dgorp01p   pri
# 280  864                                 Orozco Loreto Ismael dgorp01s   pri
# 281  365                        Estrada Rodríguez Laura Elena dgorp02p   pan
# 282  865                          Arrieta Monarres José Ramón dgorp02s   pan
# 283  366                             Herrera Rivera Bonifacio dgorp03p   pan
# 284  866                      Escobedo Maciel Carmen Patricia dgorp03s   pan
# 285  367                        Silerio Núñez María Guadalupe dgorp04p   prd
# 286  867                          Cruz Martínez Marcos Carlos dgorp04s   prd
# 287  368                                 Corona Valdés Lorena dgorp05p  pvem
# 288  868                                  Orozco Gómez Miguel dgorp05s  pvem
# 289  369                             Nava Pérez Anel Patricia dgorp06p    pt
# 290  869                       Ríos Vázquez Alfonso Primitivo dgorp06s    pt
# 291   77                                  Huerta Montero Juan   gua01p   pan
# 292  577                            Quintana Padilla Aranzazú   gua01s   pan
# 293   78                        Pascualli Gómez Juan de Jesús   gua02p   pan
# 294  578                          Agúndiz Pérez Laura Viviana   gua02s   pan
# 295   79                          Vera Hernández J. Guadalupe   gua03p   pan
# 296  579                         Alba Contreras Luz Margarita   gua03s   pan
# 297   80                         Lugo Martínez Ruth Esperanza   gua04p   pan
# 298  580           Carrillo Pérez Francisco José de Guadalupe   gua04s   pan
# 299   81                  Gallegos Camarena Lucila del Carmen   gua05p   pan
# 300  581                               Pérez Mendoza Ezequiel   gua05s   pan
# 301   82                                  Oliva Ramírez Jaime   gua06p   pan
# 302  582                         Murillo Chávez Janet Melanie   gua06s   pan
# 303   83                         Arévalo Sosa Cecilia Soledad   gua07p   pan
# 304  583                           Ramírez Arzola Juan Manuel   gua07s   pan
# 305   84                              Gutiérrez Ramírez Tomás   gua08p   pan
# 306  584                              Damiani García Virginia   gua08s   pan
# 307   85                            Zetina Soto Sixto Alfonso   gua09p   pan
# 308  585                            Villegas Méndez José Luis   gua09s   pan
# 309   86                          Orozco Torres Norma Leticia   gua10p  pvem
# 310  586                             González Martínez Miguel   gua10s  pvem
# 311   87                          Bermúdez Méndez José Erandi   gua11p   pan
# 312  587                              Zamora Ruíz María Elena   gua11s   pan
# 313   88                                  Rico Jiménez Martín   gua12p   pan
# 314  588                                Miyar Estrada Marcela   gua12s   pan
# 315   89                             Arellano Rodríguez Rubén   gua13p   pan
# 316  589                              Baeza Vallejo Alma Rosa   gua13s   pan
# 317   90                                     Merino Loo Ramón   gua14p   pan
# 318  590                              Arzate Cárdenas Ma. Luz   gua14s   pan
# 319  370                                  Rocha Aguilar Yulma guarp01p   pri
# 320  870                        Ruíz de Teresa Guillermo Raúl guarp01s   pri
# 321  371                               Sánchez García Gerardo guarp02p   pri
# 322  871                  Calderón González María del Refugio guarp02s   pri
# 323  372                        Arriaga Rojas Justino Eugenio guarp03p   pan
# 324  872                           Banda López María Gabriela guarp03s   pan
# 325  373                      De los Cobos Silva José Gerardo guarp04p   pan
# 326  873                            Calleja Villalobos Ofelia guarp04s   pan
# 327  374                      Reynoso Sánchez Alejandra Noemí guarp05p   pan
# 328  874                               Chaire Chavero Edgardo guarp05s   pan
# 329  375                                 Sánchez Romero Norma guarp06p   pan
# 330  875                            Leija Garza Eliacib Adiel guarp06s   pan
# 331  376                      Usabiaga Arroyo Javier Bernardo guarp07p   pan
# 332  876                               Lomelin Velasco Rebeca guarp07s   pan
# 333   91                            Salgado Romero Cuauhtémoc   gue01p   pri
# 334  591                     Hernández Solano José Concepción   gue01s   pri
# 335   92                             Albarrán Mendoza Esteban   gue02p   pri
# 336  592                              Flores Majul Omar Jalil   gue02s   pri
# 337   93                                   Ríos Piter Armando   gue03p   prd  COORD PRD
# 338  593                                  Blanco Sánchez Elia   gue03s   prd
# 339   94                             Carabias Icaza Alejandro   gue04p  pvem
# 340  594                 De los Santos Hernández José Antonio   gue04s  pvem
# 341   95                      Ramírez Hernández Socorro Sofío   gue05p   pri
# 342  595                          Villanueva de la Luz Moisés   gue05s   pri
# 343   96                     Zamora Villalva Alicia Elizabeth   gue06p   pri
# 344  596                            Ballesteros Hinojosa Juan   gue06s   pri
# 345   97                                   Moreno Arcos Mario   gue07p   pri
# 346  597                  Benítez Navarrete María del Socorro   gue07s   pri
# 347   98                                Aguirre Herrera Ángel   gue08p   pri
# 348  598                              Gatica Garzón Rodolfina   gue08s   pri
# 349   99                       Alvarado Arroyo Fermín Gerardo   gue09p   pri
# 350  599                        Rangel Miravete Óscar Ignacio   gue09s   pri
# 351  377                            Álvarez Santamaría Miguel guerp01p   pri
# 352  877                        Arenas Martínez Elvira Rebeca guerp01s   pri
# 353  378                       Becerra Pocoroba Mario Alberto guerp02p   pan
# 354  878                         Millán Sánchez Carlos Arturo guerp02s   pan
# 355  379                               Lobato Ramírez Ana Luz guerp03p   prd
# 356  879                              Trejo Trujillo Virginia guerp03s   prd
# 357  380                         Lozano Herrera Ilich Augusto guerp04p   prd
# 358  880                           Mendoza Flores Zeus Rafael guerp04s   prd
# 359  381                              Navarro Aguilar Filemón guerp05p   prd
# 360  881                               Cayetano Díaz Antonino guerp05s   prd
# 361  382                           Rosario Morales Florentina guerp06p   prd
# 362  882                              Tenango Salgado Carmina guerp06s   prd
# 363  383                               Arizmendi Campos Laura guerp07p conve
# 364  883                               Walton Álvarez Claudia guerp07s conve
# 365  100                                   Fayad Meneses Omar   hgo01p   pri
# 366  600                           Ramírez Sosa Martín Adolfo   hgo01s   pri
# 367  101                                Pedraza Olguín Héctor   hgo02p   pri
# 368  601                           Olguín Cuevas Rocío Marili   hgo02s   pri
# 369  102                            Rojo García de Alba Jorge   hgo03p   pri
# 370  602                                Zúñiga Fuentes Adelfa   hgo03s   pri
# 371  103                                  Penchyna Grub David   hgo04p   pri
# 372  603            Soto Plata Blanca Luz Purificación Dalila   hgo04s   pri
# 373  104                              Ramírez Valtierra Ramón   hgo05p   pri
# 374  604                              Vieyra Alamilla Marcela   hgo05s   pri
# 375  105                       Viggiano Austria Alma Carolina   hgo06p   pri
# 376  605                            Hernández Barros Federico   hgo06s   pri
# 377  106                                  Romero Romero Jorge   hgo07p   pri
# 378  606                             Licona Omaña Julio César   hgo07s   pri
# 379  384                       Hernández Olmos Paula Angélica hgorp01p   pri
# 380  884                             Serrano González Onésimo hgorp01s   pri
# 381  385                                Vázquez Góngora Canek hgorp02p   pri
# 382  885                          Cervantes Mandujano Beatríz hgorp02s   pri
# 383  386                                   Romero León Gloria hgorp03p   pan
# 384  886                        Ramírez Reyes Escobar Horacio hgorp03s   pan
# 385  387                             Acosta Naranjo Guadalupe hgorp04p   prd
# 386  887                              Cornejo Barrera Luciano hgorp04s   prd
# 387  107                              Téllez González Ignacio   jal01p   pan
# 388  607                                Jara Pinedo Rigoberto   jal01s   pan
# 389  108                               Guillén Padilla Olivia   jal02p   pri
# 390  608                            Frías González Óscar Raúl   jal02s   pri
# 391  109                              Iñiguez Gámez José Luis   jal03p   pan
# 392  609                               Navarro Aceves Rodolfo   jal03s   pan
# 393  110                                Zamora Jiménez Arturo   jal04p   pri
# 394  610                 Burgos Rivero Marlene de los Ángeles   jal04s   pri
# 395  111                              Cuevas García Juan José   jal05p   pan
# 396  611                                 Vargas Licea Maribel   jal05s   pan
# 397  112                 López Portillo Basave Jorge Humberto   jal06p   pri
# 398  612                         González Mora Esteban Adolfo   jal06s   pri
# 399  113                                    Arana Arana Jorge   jal07p   pri
# 400  613                                 Fonseca Gómez Sugeil   jal07s   pri
# 401  114                          Padilla López José Trinidad   jal08p   pri
# 402  614                            Villanueva Lomelí Ernesto   jal08s   pri
# 403  115                                     Gómez Caro Clara   jal09p   pri
# 404  615                                Corona Vargas Eduardo   jal09s   pri
# 405  116                       Ramírez Acuña Francisco Javier   jal10p   pan  COORD PAN
# 406  616                          Villaseñor Fernández Arturo   jal10s   pan
# 407  117                                Caro Cabrera Salvador   jal11p   pri
# 408  617                                 Meza Manjarrez Salma   jal11s   pri
# 409  118                                   González Díaz Joel   jal12p   pri
# 410  618                      López Villaseñor Gerardo Miguel   jal12s   pri
# 411  119                                Durán Rico Ana Estela   jal13p   pri
# 412  619                                Mayorquín Ruíz Germán   jal13s   pri
# 413  120                          Madrigal Díaz César Octavio   jal14p   pan
# 414  620                                Ávila Loreto Virginio   jal14s   pan
# 415  121                        Castellanos Flores Gumercindo   jal15p   pan
# 416  621                         León Martínez Juan Francisco   jal15s   pan
# 417  122                                Hernández Pérez David   jal16p   pri
# 418  622                              Hernández González Abel   jal16s   pri
# 419  123                        Rangel Vargas Felipe de Jesús   jal17p   pan
# 420  623                             Real Serrano Jorge David   jal17s   pan
# 421  124                         Meillón Johnston Carlos Luis   jal18p   pan
# 422  624                                   De Anda Licea Irma   jal18s   pan
# 423  125                             Esquer Gutiérrez Alberto   jal19p   pan
# 424  625                                Rubio Montaño Aurelio   jal19s   pan
# 425  388                 Scherman Leaño María Esther de Jesús jalrp01p   pri
# 426  888                                 Cortez Alvarado Raúl jalrp01s   pri
# 427  389                               Yerena Zambrano Rafael jalrp02p   pri
# 428  889                     Gómez Villalovos María de la Luz jalrp02s   pri
# 429  390                           González Hernández Gustavo jalrp03p   pan
# 430  890                               Estrada Luna Luz Elena jalrp03s   pan
# 431  391                         Novoa Mossberger María Joann jalrp04p   pan
# 432  891                          Valencia García Juan Carlos jalrp04s   pan
# 433  392                              Paredes Arciga Ana Elia jalrp05p   pan
# 434  892                            Ortega Zepeda Juan Carlos jalrp05s   pan
# 435  393                      Cinta Martínez Alberto Emiliano jalrp06p  pvem
# 436  893                             Pérez Uribe Edgar Javier jalrp06s  pvem
# 437  394                          Ibarra Pedroza Juan Enrique jalrp07p    pt
# 438  894                         Vázquez Castillo Julio César jalrp07s    pt
# 439  126                        Velasco Monroy Héctor Eduardo   mex01p   pri
# 440  626                              González Ledezma Aurora   mex01s   pri
# 441  127                                   Domínguez Rex Raúl   mex02p   pri
# 442  627                 Santillán Hernández Juanita de Jésus   mex02s   pri
# 443  128                              Chuayffet Chemor Emilio   mex03p   pri
# 444  628                     Arias Flores Marisol del Socorro   mex03s   pri
# 445  129                               Hernández García Elvia   mex04p   pri
# 446  629                    Mendieta Villagrán Carlos Alberto   mex04s   pri
# 447  130                             Borja Texocotitla Felipe   mex05p   pri
# 448  630                       Delgado del Valle Flor Orlanda   mex05s   pri
# 449  131                               Guevara Ramírez Héctor   mex06p   pri
# 450  631                        Sánchez Luna María de Lourdes   mex06s   pri
# 451  132                      Rojas San Román Francisco Lauro   mex07p   pri
# 452  632                            Bello Reyes Claudia Iveth   mex07s   pri
# 453  133                              Méndez Hernández Sandra   mex08p   pri
# 454  633                                  Díaz Bautista Edgar   mex08s   pri
# 455  134                              Zarzosa Sánchez Eduardo   mex09p   pri
# 456  634                       Mercado García María del Pilar   mex09s   pri
# 457  135                             Vázquez Pérez Noé Martín   mex10p   pri
# 458  635                           Mobarak López Reyna Alicia   mex10s   pri
# 459  136                            Hernández Hernández Jorge   mex11p   pri
# 460  636                          Solís García María Verónica   mex11s   pri
# 461  137                                Corona Rivera Armando   mex12p   pri
# 462  637                           Varela Espinoza Concepción   mex12s   pri
# 463  138                          Torres Huitrón José Alfredo   mex13p   pri
# 464  638                               López Escobar Marisela   mex13s   pri
# 465  139                      Saldaña del Moral Fausto Sergio   mex14p   pri
# 466  639                      Hernández Escobar Alma Berenice   mex14s   pri
# 467  140                                   Bello Otero Carlos   mex15p   pan
# 468  640                      Montes de Oca Rodríguez Xóchitl   mex15s   pan
# 469  141                              Soto Oseguera José Luis   mex16p   pri
# 470  641                              Camargo Pérez Esperanza   mex16s   pri
# 471  142                            Valdés Huezo Josué Cirino   mex17p   pri
# 472  642                    Quezada Samaniego Dolores Rosalía   mex17s   pri
# 473  143                        Navarrete Prida Jesús Alfonso   mex18p   pri
# 474  643                   Castilla García Guadalupe Gabriela   mex18s   pri
# 475  144                                Monroy Estrada Amador   mex19p   pri
# 476  644                           López Escobar Rosa de Lima   mex19s   pri
# 477  145                       Enríquez Fuentes Jesús Ricardo   mex20p   pri
# 478  645                          Bernal Díaz Martha Patricia   mex20s   pri
# 479  146                                Reina Liceaga Rodrigo   mex21p   pri
# 480  646                                 Oyoque Ortíz Claudia   mex21s   pri
# 481  147                        Sánchez Guevara David Ricardo   mex22p   pri
# 482  647                        Olivas Hernández María Elvira   mex22s   pri
# 483  148                        Pichardo Lechuga José Ignacio   mex23p   pri
# 484  648                                 Colín Vázquez Gisela   mex23s   pri
# 485  149                                Mancilla Zayas Sergio   mex24p   pri
# 486  649                                Durán Ortíz Estefanía   mex24s   pri
# 487  150                                Ibarra Piña Inocencio   mex25p   pri
# 488  650                 Villalobos Hernández Norma Margarita   mex25s   pri
# 489  151                               Hernández Silva Héctor   mex26p   pri
# 490  651                                   Ruíz Ugalde Romina   mex26s   pri
# 491  152                          Terrón Mendoza Miguel Ángel   mex27p   pri
# 492  652                      Bastida Guadarrama Norma Karina   mex27s   pri
# 493  153                       Germán Olivares Sergio Octavio   mex28p   pan
# 494  653                               Piña Pérez Pedro César   mex28s   pan
# 495  154                               Pedroza Jiménez Héctor   mex29p   pri
# 496  654                Franco Cruz María de la Cruz Patricia   mex29s   pri
# 497  155                              Rodríguez Cisneros Omar   mex30p   pri
# 498  655                         Báez Padilla María del Pilar   mex30s   pri
# 499  156                       Cortez Sandoval Germán Osvaldo   mex31p   pri
# 500  656                           Soria Morales Blanca Juana   mex31s   pri
# 501  157                            Luna Munguía Miguel Ángel   mex32p   pri
# 502  657                                 Sánchez Amaya Raquel   mex32s   pri
# 503  158                             Yáñez Montaño J. Eduardo   mex33p   pri
# 504  658                       Martínez Tenorio Lilia Leticia   mex33s   pri
# 505  159                               Velasco Lino José Luis   mex34p   pri
# 506  659                                 Suárez Bastida Hazel   mex34s   pri
# 507  160                           Ferreyra Olivares Fernando   mex35p   pri
# 508  660                                    Anaya Gómez Karla   mex35s   pri
# 509  161                           Casique Vences Guillermina   mex36p   pri
# 510  661                           Montañez Martínez Apolinar   mex36s   pri
# 511  162                                  Guzmán Rosas Emilia   mex37p   pri
# 512  662                          Ledesma Magaña Israel Reyes   mex37s   pri
# 513  163                                Cadena Morales Manuel   mex38p   pri
# 514  663                                  Barrera Mazon Paola   mex38s   pri
# 515  164                                Aguirre Romero Andrés   mex39p   pri
# 516  664                        García Rodríguez Benylu Joana   mex39s   pri
# 517  165                       Rubí Salazar José Adán Ignacio   mex40p   pri
# 518  665                             Garduño Estrada Verónica   mex40s   pri
# 519  395                        Vázquez Mota Josefina Eugenia mexrp01p   pan  COORD PAN
# 520  895                                   Novoa Gómez Miguel mexrp01s   pan
# 521  396                                 Guerrero Rubio Diego mexrp02p  pvem
# 522  896                               Herrera Martínez Jorge mexrp02s  pvem
# 523  397                      Benítez Treviño Víctor Humberto mexrp03p   pri
# 524  897                     López Cano Aveleyra Norma Silvia mexrp03s   pri
# 525  398                              Martel López José Ramón mexrp04p   pri
# 526  898                          Gómez Molano Judith Aracely mexrp04s   pri
# 527  399                             Massieu Fernández Andrés mexrp05p   pri
# 528  899                            Melgarejo Fukutake Imelda mexrp05s   pri
# 529  400                                 Neyra Chávez Armando mexrp06p   pri
# 530  900                                  Espino Suárez Mayra mexrp06s   pri
# 531  401                       Rojas Gutiérrez Francisco José mexrp07p   pri COORD PRI
# 532  901              Juan López María de las Mercedes Martha mexrp07s   pri
# 533  402                           Serrano Hernández Maricela mexrp08p   pri
# 534  902                                  Arvizu Lara Orlando mexrp08s   pri
# 535  403                                  Videgaray Caso Luis mexrp09p   pri
# 536  903                            Fernández Martínez Silvia mexrp09s   pri
# 537  404                 Hinojosa Céspedes Adriana de Lourdes mexrp10p   pan
# 538  904                             Muro Ortíz Jorge Alberto mexrp10s   pan
# 539  405              Landero Gutiérrez José Francisco Javier mexrp11p   pan
# 540  905                            Mondragón Cobos Guadalupe mexrp11s   pan
# 541  406                          Pérez Cuevas Carlos Alberto mexrp12p   pan
# 542  906                             Zafra Alvarado Ana María mexrp12s   pan
# 543  407                     Pérez de Tejada Romero Ma. Elena mexrp13p   pan
# 544  907                                Morales Najera Jacobo mexrp13s   pan
# 545  408                     Bernardino Rojas Martha Angélica mexrp14p   prd
# 546  908                         Álvarez Vázquez María Teresa mexrp14s   prd
# 547  409                     Castro y Castro Juventino Víctor mexrp15p   prd
# 548  909                      Sánchez Cortés Hilario Everardo mexrp15s   prd
# 549  410                              García Coronado Lizbeth mexrp16p   prd
# 550  910                           Méndez Aguilar María Diana mexrp16s   prd
# 551  411                               Jaime Correa José Luis mexrp17p   prd
# 552  911                                   Alonso Pérez Pedro mexrp17s   prd
# 553  412                         Marín Díaz Feliciano Rosendo mexrp18p   prd
# 554  912                      Enríquez Rosado José del Carmen mexrp18s   prd
# 555  413                                García Cañón Carolina mexrp19p  pvem
# 556  913                              Del Mazo Maza Alejandro mexrp19s  pvem
# 557  414                                Guerra Abud Juan José mexrp20p  pvem  COORD PVEM
# 558  914                               Sánchez Sánchez Misael mexrp20s  pvem
# 559  415                         Vargas Sáenz Álvaro Raymundo mexrp21p  pvem
# 560  915                                Sáenz Vargas Caritina mexrp21s  pvem
# 561  416                                 González Yáñez Óscar mexrp22p    pt
# 562  916                             Palacios Calderón Martín mexrp22s    pt
# 563  417                       Reyes Sahagún Teresa Guadalupe mexrp23p    pt
# 564  917                         González Camacho María Elena mexrp23s    pt
# 565  418                               Gertz Manero Alejandro mexrp24p conve
# 566  918                      Tapia Latisnere Paulino Gerardo mexrp24s conve
# 567  419                       Ochoa Mejía Ma. Teresa Rosaura mexrp25p conve
# 568  919                            Huidobro González Zuleyma mexrp25s conve
# 569  420                        Torre Canales María del Pilar mexrp26p panal
# 570  920                               Romero Vázquez Gabriel mexrp26s panal
# 571  421                         Vázquez Aguilar Jaime Arturo mexrp27p panal
# 572  921                        Molina Vélez Sarbelio Augusto mexrp27s panal
# 573  166                            Godoy Toscano Julio César   mic01p   prd
# 574  666                                 Madrigal Ceja Israel   mic01s   prd
# 575  167                               Torres Robledo José M.   mic02p   prd
# 576  667                               Barajas Ontiveros Raúl   mic02s   prd
# 577  168                                Herrera Soto Ma. Dina   mic03p   prd
# 578  668                            Toledo Hernández Angélica   mic03s   prd
# 579  169                               Sánchez Gálvez Ricardo   mic04p   pri
# 580  669                                Gutiérrez Cuadra Raúl   mic04s   pri
# 581  170                          Torres Santos Sergio Arturo   mic05p   pan
# 582  670                              Acevedo Martínez Judith   mic05s   pan
# 583  171                          Velázquez Esquivel Emiliano   mic06p   prd
# 584  671                              Pérez Nieves Eric David   mic06s   prd
# 585  172                                 García Avilés Martín   mic07p   prd
# 586  672                     Márquez Capiz Jeanette Guadalupe   mic07s   prd
# 587  173                       Martínez Alcázar Alfonso Jesús   mic08p   pan
# 588  673                                Salazar Blanco Iridia   mic08s   pan
# 589  174                                  López Paredes Uriel   mic09p   prd
# 590  674                                   García Ayala Celia   mic09s   prd
# 591  175                      Suárez González Laura Margarita   mic10p   pan
# 592  675                       Villalobos Guzmán José Eugenio   mic10s   pan
# 593  176                              Báez Ceja Víctor Manuel   mic11p   prd
# 594  676                           González Díaz José Alfredo   mic11s   prd
# 595  177                          Valencia Barajas José María   mic12p   prd
# 596  677                               Mendoza Alcaráz Andrés   mic12s   prd
# 597  422                            De los Reyes Aguilar Jeny micrp01p   pri
# 598  922                              Espino Arévalo Fernando micrp01s   pri
# 599  423                            Castellanos Ramírez Julio micrp02p   pan
# 600  923                               Gudiño Paredes Mariana micrp02s   pan
# 601  424                           Hinojosa Pérez José Manuel micrp03p   pan
# 602  924                            Medina Castro María Elena micrp03s   pan
# 603  425                              Quezada Naranjo Benigno micrp04p   pan
# 604  925                  Herrera Maldonado Teresita de Jesús micrp04s   pan
# 605  426                              Torres Ibarrola Agustín micrp05p   pan
# 606  926                         González Hernández Doramitzi micrp05s   pan
# 607  427              Nazares Jerónimo Dolores de los Ángeles micrp06p   prd
# 608  927                       Frausto Vázquez Karla Patricia micrp06s   prd
# 609  428                                   Torres Piña Carlos micrp07p   prd
# 610  928                                    Díaz Juárez Pável micrp07s   prd
# 611  178                    Moreno Merino Francisco Alejandro   mor01p   pri
# 612  678                                Magdaleno Gómez Elvia   mor01s   pri
# 613  179                             Agüero Tovar José Manuel   mor02p   pri
# 614  679                               Flores Rodríguez Edith   mor02s   pri
# 615  180                            Rodríguez Sosa Luis Félix   mor03p   pri
# 616  680                     Nava Sánchez Christian Alejandro   mor03s   pri
# 617  181                                Mazari Espín Rosalina   mor04p   pri
# 618  681                             Olivares Hernández Pedro   mor04s   pri
# 619  182                                  Sánchez Vélez Jaime   mor05p   pri
# 620  682                              Aispuro Funes José Luis   mor05s   pri
# 621  429                                  Giles Sánchez Jesús morrp01p   pan
# 622  929                       Rubí Huicochea Fidel Christian morrp01s   pan
# 623  430                               Álvarez Cisneros Jaime morrp02p conve
# 624  930                            Garduño Aguilar Guillermo morrp02s conve
# 625  183                         Cota Jiménez Manuel Humberto   nay01p   pri
# 626  683                         Gómez Montero Fátima del Sol   nay01s   pri
# 627  184                            García Gómez Martha Elena   nay02p   prd
# 628  684                      Ocegueda Silva María Florentina   nay02s   prd
# 629  185                            Reyes Hernández Ivideliza   nay03p   pan
# 630  685                               Pimienta Márquez Pablo   nay03s   pan
# 631  431                       Domínguez Arvizu María Hilaria nayrp01p   pri
# 632  931                           Chávez Pérez Ricardo Pedro nayrp01s   pri
# 633  432                        Parra Becerra María Felícitas nayrp02p   pan
# 634  932                               Ramos Quirarte Antonio nayrp02s   pan
# 635  433                                   Pinedo Alonso Cora nayrp03p panal
# 636  933                 Talamante Lemas Dora María Guadalupe nayrp03s panal
# 637  186                    Balderas Vaquera Víctor Alejandro    nl01p   pan
# 638  686                       Theagene Navarro Zaira Jessica    nl01s   pan
# 639  187                        Guajardo Villarreal Ildefonso    nl02p   pri
# 640  687                             Coronado Hernández Félix    nl02s   pri
# 641  188                       Bailey Elizondo Eduardo Alonso    nl03p   pri
# 642  688                              Valdez Olivares Maribel    nl03s   pri
# 643  189                                Ramírez Puente Camilo    nl04p   pan
# 644  689                       Cuadra Tinajero Alfredo Sergio    nl04s   pan
# 645  190                              Guerra Castillo Marcela    nl05p   pri
# 646  690                              Reyes Montemayor Rafael    nl05s   pri
# 647  191                               Hurtado Leija Gregorio    nl06p   pan
# 648  691                   Rodríguez Bautista Karla Alejandra    nl06s   pan
# 649  192                            Enríquez Hernández Felipe    nl07p   pri
# 650  692                          De la Garza Malacara Adolfo    nl07s   pri
# 651  193                       Aguirre Maldonado Ma. de Jesús    nl08p   pri
# 652  693                            Aguirre González Patricia    nl08s   pri
# 653  194                                Montes Cavazos Fermín    nl09p   pri
# 654  694                            Tovar Puente María Teresa    nl09s   pri
# 655  195                      Rodríguez Dávila Alfredo Javier    nl10p   pan
# 656  695                            Pérez Bernal José Alfredo    nl10s   pan
# 657  196                          Díaz Salazar María Cristina    nl11p   pri
# 658  696                           Cantú Cantú Gabriel Tlaloc    nl11s   pri
# 659  197                                  Cerda Pérez Rogelio    nl12p   pri
# 660  697                               Ibarra Hinojosa Álvaro    nl12s   pri
# 661  434                       Clariond Reyes Retana Benjamín  nlrp01p   pri
# 662  934                      García Velázquez Antonia Mónica  nlrp01s   pri
# 663  435                      Cantú Rodríguez Felipe de Jesús  nlrp02p   pan
# 664  935                      Carrasco Alarcón Aleida Giovana  nlrp02s   pan
# 665  436                         Martínez Montemayor Baltazar  nlrp03p   pan
# 666  936                       Castillo Almanza Itzel Soledad  nlrp03s   pan
# 667  437                                    Garza Romo Kattia  nlrp04p  pvem
# 668  937                                 Cueva Sada Guillermo  nlrp04s  pvem
# 669  438                        Pérez-Alonso González Rodrigo  nlrp05p  pvem
# 670  938                                García Requen Roberto  nlrp05s  pvem
# 671  439                               Vázquez González Pedro  nlrp06p    pt  COORD PT
# 672  939                                 Quiroz García Héctor  nlrp06s    pt
# 673  440                                Tamez Guerra Reyes S.  nlrp07p panal
# 674  940                            Meza Elizondo José Isabel  nlrp07s panal
# 675  198                                   Pérez Magaña Eviel   oax01p   pri
# 676  698                               Avilés Álvarez Violeta   oax01s   pri
# 677  199                    Concha Arellano Elpidio Desiderio   oax02p   pri
# 678  699                                   Leyva García David   oax02s   pri
# 679  200                    González Ilescas Jorge Venustiano   oax03p   pri
# 680  700                         Mendoza Aroche Javier Sergio   oax03s   pri
# 681  201                          Ambrosio Cipriano Heriberto   oax04p   pri
# 682  701                                       Diego Cruz Eva   oax04s   pri
# 683  202                                    Castro Ríos Sofía   oax05p   pri
# 684  702                           Zetuna Curioca Jorge Zarif   oax05s   pri
# 685  203                      Díaz Escárraga Heliodoro Carlos   oax06p   pri
# 686  703                            Hernández Reyes Elizabeth   oax06s   pri
# 687  204                         Mendoza Kaplan Emilio Andrés   oax07p   pri
# 688  704                        García López Francisco Javier   oax07s   pri
# 689  205                 De Esesarte Pesqueira Manuel Esteban   oax08p   pri
# 690  705                                   España López Paola   oax08s   pri
# 691  206                         García Corpus Teófilo Manuel   oax09p   pri
# 692  706                   Silva Fernández Claudia del Carmen   oax09s   pri
# 693  207                      Ramírez Puga Leyva Héctor Pablo   oax10p   pri
# 694  707                           Ramos Ramos Judith Marisol   oax10s   pri
# 695  208                        Yglesias Arreola José Antonio   oax11p   pri
# 696  708                          Ziga Martínez Zory Maristel   oax11s   pri
# 697  441                         Franco Vargas Jorge Fernando oaxrp01p   pri
# 698  941                  Aparicio Sánchez Florencia Carolina oaxrp01s   pri
# 699  442                           Liborio Arrazola Margarita oaxrp02p   pri
# 700  942                            Ortega Habib Miguel Ángel oaxrp02s   pri
# 701  443                            Ramírez Pineda Narcedalia oaxrp03p   pri
# 702  943                        Mendicuti Priego José Ignacio oaxrp03s   pri
# 703  444                        Zavaleta Rojas Guillermo José oaxrp04p   pan
# 704  944                       Mendoza Sánchez María de Jesús oaxrp04s   pan
# 705  445                        Carmona Cabrera Bélgica Nabil oaxrp05p   prd
# 706  945                        Serrano Rosado Aleida Tonelly oaxrp05s   prd
# 707  446                            Cruz Cruz Juanita Arcelia oaxrp06p   prd
# 708  946                                  Hernández Díaz Aída oaxrp06s   prd
# 709  447                       García Almanza María Guadalupe oaxrp07p conve
# 710  947                                   Soto Martínez José oaxrp07s conve
# 711  209                                Vargas Fosado Ardelio   pue01p   pri
# 712  709                                 Urzúa Rivera Ricardo   pue01s   pri
# 713  210                           Lastiri Quirós Juan Carlos   pue02p   pri
# 714  710                               Ramírez Martínez Malco   pue02s   pri
# 715  211                      Juraidini Rumilla Jorge Alberto   pue03p   pri
# 716  711                          Martagón López Ramón Daniel   pue03s   pri
# 717  212                          Aguilar González José Óscar   pue04p   pri
# 718  712                         Cabrera Huerta María Eugenia   pue04s   pri
# 719  213                      González Tostado Janet Graciela   pue05p   pri
# 720  713                                Sánchez Romero Carlos   pue05s   pri
# 721  214                              Ramos Montaño Francisco   pue06p   pri
# 722  714                              Romero Sierra Ana Laura   pue06s   pri
# 723  215                        González Morales José Alberto   pue07p   pri
# 724  715            Bretón Sánchez María de Lourdes Rosalinda   pue07s   pri
# 725  216                            Morales Martínez Fernando   pue08p   pri
# 726  716                              Camarillo Medina Néstor   pue08s   pri
# 727  217                      Jiménez Hernández Blanca Estela   pue09p   pri
# 728  717                        Carreto Pacheco Víctor Manuel   pue09s   pri
# 729  218                            Jiménez Concha Juan Pablo   pue10p   pri
# 730  718                              Roldán Castillo Matilde   pue10s   pri
# 731  219                             Natale López Juan Carlos   pue11p  pvem
# 732  719                        Alcántara Silva Miguel Sergio   pue11s  pvem
# 733  220                               Soto Martínez Leobardo   pue12p   pri
# 734  720                            Conde Montaño María Luisa   pue12s   pri
# 735  221                          Merlo Talavera María Isabel   pue13p   pri
# 736  721                               Delgado Juárez Modesta   pue13s   pri
# 737  222                     Jiménez Merino Francisco Alberto   pue14p   pri
# 738  722                    Guevara González Javier Filiberto   pue14s   pri
# 739  223                   Izaguirre Francos María del Carmen   pue15p   pri
# 740  723 Soberón Espinosa Felipe de Jesús del Sagrado Corazón   pue15s   pri
# 741  224                         Marín Torres Julieta Octavia   pue16p   pri
# 742  724                                 Abril Cogque Gonzalo   pue16s   pri
# 743  448           Díaz de Rivera Hernández Augusta Valentina puerp01p   pan
# 744  948                               García Olivares Andrés puerp01s   pan
# 745  449                            Rodríguez Regordosa Pablo puerp02p   pan
# 746  949                       Guzmán Lozano María del Carmen puerp02s   pan
# 747  225                             Martínez Peñaloza Miguel   que01p   pan
# 748  725                           Morado García Ma. Soledad    que01s   pan
# 749  226                               Fuentes Cortés Adriana   que02p   pan
# 750  726                                    Ochoa Parra Mario   que02s   pan
# 751  227                         Lugo Oñate Alfredo Francisco   que03p   pri
# 752  727              González Alvarado María Genoveva Anavel   que03s   pri
# 753  228                         Rivera de la Torre Reginaldo   que04p   pri
# 754  728                          Roque Almazo Dulce Patricia   que04s   pri
# 755  450                      Rodríguez Hernández Jesús María querp01p   pri
# 756  950                          Guidi Kawas María Guadalupe querp01s   pri
# 757  451                        Torres Peimbert María Marcela querp02p   pan
# 758  951                                 Cantú Latapi Roberto querp02s   pan
# 759  452                         Ugalde Basaldúa María Sandra querp03p   pan
# 760  952                               Pájaro Anaya Francisco querp03s   pan
# 761  453                         Ezeta Salcedo Mariana Ivette querp04p  pvem
# 762  953                         Ezeta Salcedo Carlos Alberto querp04s  pvem
# 763  454                              Pacchiano Alamán Rafael querp05p  pvem
# 764  954                          Lagunes Soto Ruíz Alejandra querp05s  pvem
# 765  229                                 Borge Angulo Roberto   qui01p   pri
# 766  729                               Hurtado Vallejo Susana   qui01s   pri
# 767  230                              Ortiz Yeladaqui Rosario   qui02p   pri
# 768  730                                    García Silva Luis   qui02s   pri
# 769  231                       Joaquín González Carlos Manuel   qui03p   pri
# 770  731                                      Hop Arzate Olga   qui03s   pri
# 771  455                Ortega Joaquín Gustavo Antonio Miguel quirp01p   pan
# 772  955                           Guerrero García Eva Sylvia quirp01s   pan
# 773  232                                   Mendoza Díaz Sonia   san01p   pan
# 774  732                            García Pacheco José Fidel   san01s   pan
# 775  233                    Rodríguez Galarza Wendy Guadalupe   san02p   pan
# 776  733                           Calzada Macías Luis Manuel   san02s   pan
# 777  234                                   Gama Dufour Sergio   san03p   pan
# 778  734                         Govea Derreza Bertha Yolanda   san03s   pan
# 779  235                              Guerrero Coronado Delia   san04p   pri
# 780  735                          Rodríguez Lucero María Elia   san04s   pri
# 781  236                         Pedroza Gaitán César Octavio   san05p   pan
# 782  736                            Alonso Sotelo Rosa Imelda   san05s   pan
# 783  237                          Escobar Martínez Juan Pablo   san06p   pan
# 784  737                       Cid González Alexandra Daniela   san06s   pan
# 785  238                           Bautista Concepción Sabino   san07p   pri
# 786  738                                 Lara Pérez Ermentina   san07s   pri
# 787  456                          Montiel Solís Sara Gabriela sanrp01p   pri
# 788  956                        Rosas Ramírez Enrique Salomón sanrp01s   pri
# 789  457                       Salazar Sáenz Francisco Javier sanrp02p   pan
# 790  957           Vázquez García Josefina de la Cruz Celeste sanrp02s   pan
# 791  458                         Trejo Azuara Enrique Octavio sanrp03p   pan
# 792  958                           Juárez Alejo Dora Patricia sanrp03s   pan
# 793  459                            Rodríguez Martell Domingo sanrp04p   prd
# 794  959                           Arguijo Baldenegro Eduardo sanrp04s   prd
# 795  239                                   Lara Salazar Óscar   sin01p   pri
# 796  739                         Ramos Carbajal Nubia Xiclali   sin01s   pri
# 797  240                                 Zubia Rivera Rolando   sin02p   pri
# 798  740                   Calderón Guillén María del Socorro   sin02s   pri
# 799  241                          Bojórquez Gutiérrez Rolando   sin03p   pri
# 800  741                    Casillas Alvarado Rosa del Carmen   sin03s   pri
# 801  242                          Gastélum Bajo Diva Hadamira   sin04p   pri
# 802  742                        Camacho Luque Rosendo Enrique   sin04s   pri
# 803  243                                   Irízar López Aarón   sin05p   pri
# 804  743                          Tirado Gálvez Reyna Araceli   sin05s   pri
# 805  244                              Contreras García Germán   sin06p   pri
# 806  744                               Millán Velarde Araceli   sin06s   pri
# 807  245                           Lara Aréchiga Óscar Javier   sin07p   pri
# 808  745                                  Moncayo Leyva Paola   sin07s   pri
# 809  246                         García Granados Miguel Ángel   sin08p   pri
# 810  746                                Chollet Morán Maribel   sin08s   pri
# 811  460                         Levin Coppel Óscar Guillermo sinrp05p   pri
# 812  960                      Torres Lizarraga Carmen Julieta sinrp05s   pri
# 813  461                             Villegas Arreola Alfredo sinrp06p   pri
# 814  961                        Moreno Ovalles Irma Guadalupe sinrp06s   pri
# 815  462                      Clouthier Carrillo Manuel Jesús sinrp07p   pan
# 816  962                           González Schcolnik Valerio sinrp07s   pan
# 817  463                      Robles Medina Guadalupe Eduardo sinrp08p   pan
# 818  963                                Soto Piña María Elena sinrp08s   pan
# 819  464                                  Rojo Montoya Adolfo sinrp09p   pan
# 820  964                                 Zamudio Guzmán María sinrp09s   pan
# 821  465                         Escobar García Herón Agustín sinrp10p    pt
# 822  965                              Hernández Reyes Mariano sinrp10s    pt
# 823  247                       Guillén Medina Leonardo Arturo   son01p   pan
# 824  747                              Valle Vea Carmen Lizeth   son01s   pan
# 825  248                         Pompa Corella Miguel Ernesto   son02p   pri
# 826  748                                Palacio García Mónica   son02s   pri
# 827  249                             De Lucas Hopkins Ernesto   son03p   pri
# 828  749                                Encinas Parra Cecilia   son03s   pri
# 829  250                          León Perea José Luis Marcos   son04p   pri
# 830  750                             Calles Villegas Patricia   son04s   pri
# 831  251                      Acosta Gutiérrez Manuel Ignacio   son05p   pri
# 832  751                          Franco Hernández Luz Mireya   son05s   pri
# 833  252                  Díaz Brown Ramsburgh Rogelio Manuel   son06p   pri
# 834  752                             Caraveo Galindo Teresita   son06s   pri
# 835  253                        Mariscales Delgadillo Onésimo   son07p   pri
# 836  753                       Amarillas Urias Patricia Elena   son07s   pri
# 837  466                             Cano Vélez Jesús Alberto sonrp01p   pri
# 838  966                           Sánchez Chiu Iris Fernanda sonrp01s   pri
# 839  467                        Del Río Sánchez María Dolores sonrp02p   pan
# 840  967                      Pavlovich Rodríguez José Gaston sonrp02s   pan
# 841  468                                  Pérez Esquer Marcos sonrp03p   pan
# 842  968                            Madera Ayon Irocema Naidu sonrp03s   pan
# 843  469                               Torres Delgado Enrique sonrp04p   pan
# 844  969                             Flores Torres Rosa Icela sonrp04s   pan
# 845  470                          Trigueras Durón Dora Evelyn sonrp05p   pan
# 846  970                                 Encinas Castro Jesús sonrp05s   pan
# 847  471                           Moreno Terán Carlos Samuel sonrp06p  pvem
# 848  971                              Quihuis Fragoso Maríano sonrp06s  pvem
# 849  254                             Aysa Bernat José Antonio   tab01p   pri
# 850  754                                 Lastiri Falcón Anahí   tab01s   pri
# 851  255                     De la Fuente Dagdug María Estela   tab02p   pri
# 852  755                          De la Cruz Maldonado Eladio   tab02s   pri
# 853  256                        Burelo Burelo César Francisco   tab03p   prd
# 854  756                                   Suárez Ruíz Amador   tab03s   prd
# 855  257                         López Hernández Adán Augusto   tab04p   prd
# 856  757                       De la Fuente Godínez Alejandro   tab04s   prd
# 857  258                        Bellizia Aboaf Nicolás Carlos   tab05p   pri
# 858  758                      Magaña Madrigal Elsy del Carmen   tab05s   pri
# 859  259                     Córdova Hernández José del Pilar   tab06p   pri
# 860  759                                Cachón Álvarez Miguel   tab06s   pri
# 861  472                           Trujillo Zentella Georgina tabrp01p   pri
# 862  972                             Amaro Betancourt Gerardo tabrp01s   pri
# 863  473                        Valenzuela Cabrales Guadalupe tabrp02p   pan
# 864  973                    Soberano Miranda Francisco Javier tabrp02s   pan
# 865  474                                 Lara Lagunas Rodolfo tabrp03p   prd
# 866  974                              Garduño Yáñez Francisco tabrp03s   prd
# 867  475                          Flores Ramírez Juan Gerardo tabrp04p  pvem
# 868  975                            Tomas Ruíz Verónica Rocío tabrp04s  pvem
# 869  476                                   Jiménez León Pedro tabrp05p conve  COORD Conve
# 870  976                            Coronado Sangines Ricardo tabrp05s conve
# 871  260                            Zamora Cabrera Cristabell   tam01p   pri
# 872  760                         García Jiménez Adolfo Víctor   tam01s   pri
# 873  261                    Villarreal Salinas Jesús Everardo   tam02p   pri
# 874  761                        García Dávila Laura Felícitas   tam02s   pri
# 875  262                               Melhem Salinas Edgardo   tam03p   pri
# 876  762                          Garza Peña Litha del Carmen   tam03s   pri
# 877  263                       Hinojosa Ochoa Baltazar Manuel   tam04p   pri
# 878  763                      Molina Pérez Martha Alejandrina   tam04s   pri
# 879  264                                  Torre Cantú Rodolfo   tam05p   pri
# 880  764                   Canseco Gómez Morelos Jaime Carlos   tam05s   pri
# 881  265                         Guevara Cobos Luis Alejandro   tam06p   pri
# 882  765                     Guajardo Maldonado Sergio Carlos   tam06s   pri
# 883  266                                     Gil Ortiz Javier   tam07p   pri
# 884  766                                   Romero Vega Esdras   tam07s   pri
# 885  267                       Rábago Castillo José Francisco   tam08p   pri
# 886  767                              Sosa Ruíz Olga Patricia   tam08s   pri
# 887  477                                   Flores Rico Carlos tamrp01p   pri
# 888  977                         Gómez Ramírez Nayeli Lizbeth tamrp01s   pri
# 889  478                  Guillén Vicente Mercedes del Carmen tamrp02p   pri
# 890  978                                   Cabral Soto Felipe tamrp02s   pri
# 891  479                                   López Aguilar Cruz tamrp03p   pri
# 892  979                           Silva Solórzano Ana Bertha tamrp03s   pri
# 893  480                                   Solís Acero Felipe tamrp04p   pri
# 894  980                      Del Real Jaime Mónica Deyhanira tamrp04s   pri
# 895  481                        Salazar Vázquez Norma Leticia tamrp05p   pan
# 896  981                        Saldivar Reyna Gerardo Javier tamrp05s   pan
# 897  268                               López Hernández Oralia   tla01p   pan
# 898  768                       Macías Romero Humberto Agustín   tla01s   pan
# 899  269                Velázquez y Llorente Julián Francisco   tla02p   pan
# 900  769                       Juárez Capilla Prudencia Félix   tla02s   pan
# 901  270                         López Loyo María Elena Perla   tla03p   pri
# 902  770                               Herrera Ortega Nazario   tla03s   pri
# 903  482                               Paredes Rangel Beatriz tlarp01p   pri
# 904  982                    Álvarez y Mazarrasa Jaime Aguilar tlarp01s   pri
# 905  483                            González Hernández Sergio tlarp02p   pan
# 906  983                                 Pérez Gutiérrez Fany tlarp02s   pan
# 907  271                          Chirinos del Ángel Patricio   ver01p   pri
# 908  771                           Escalante Zapata Nora Irma   ver01s   pri
# 909  272                            Mejía de la Merced Genaro   ver02p   pri
# 910  772                        Díaz Azuara Norberta Adalmira   ver02s   pri
# 911  273                                  Martín López Miguel   ver03p   pan
# 912  773                                 Vera Hernández Jorge   ver03s   pan
# 913  274                                 Manzur Díaz Salvador   ver04p   pri
# 914  774                                 Robles Morales Adela   ver04s   pri
# 915  275                           Quiroz Cruz Sergio Lorenzo   ver05p   pri
# 916  775                              Gómez González Leobardo   ver05s   pri
# 917  276                            Herrera Jiménez Francisco   ver06p   pri
# 918  776                                Tovar Lorenzo Mariela   ver06s   pri
# 919  277                          Méndez Herrera Alba Leonila   ver07p   pan
# 920  777                             Carballo Rendón Salvador   ver07s   pan
# 921  278                                 Lagos Galindo Silvio   ver08p   pri
# 922  778                                     Ayala Ríos Erika   ver08s   pri
# 923  279                        Yunes Zorrilla José Francisco   ver09p   pri
# 924  779                               Peralta Galicia Aníbal   ver09s   pri
# 925  280                              Ahued Bardahuil Ricardo   ver10p   pri
# 926  780                                 Aburto López Rosalba   ver10s   pri
# 927  281                        García Bringas Leandro Rafael   ver11p   pan
# 928  781                         Uribe Pozos José Encarnación   ver11s   pan
# 929  282                            Gudiño Corro Luz Carolina   ver12p   pri
# 930  782                           Miranda Herrera Nely Edith   ver12s   pri
# 931  283                        Flores Espinosa Felipe Amadeo   ver13p   pri
# 932  783                          Rosas Peralta Frida Celeste   ver13s   pri
# 933  284                       Martínez Armengol Luis Antonio   ver14p   pri
# 934  784                            Félix Porras Ciro Gonzalo   ver14s   pri
# 935  285                                  Kuri Grajales Fidel   ver15p   pri
# 936  785                      Castelán Macías Adriana Refugio   ver15s   pri
# 937  286                               Duarte de Ochoa Javier   ver16p   pri
# 938  786                               Nadal Riquelme Daniela   ver16s   pri
# 939  287                          Carrillo Sánchez José Tomás   ver17p   pri
# 940  787                           Álvarez Martínez José Luis   ver17s   pri
# 941  288                            Pérez Santos María Isabel   ver18p   pri
# 942  788                    Barrientos de la Rosa María Irene   ver18s   pri
# 943  289                           Santamaría Prieto Fernando   ver19p   pan
# 944  789                         Torres Hernández María Elena   ver19s   pan
# 945  290                          Vázquez Saut Judith Fabiola   ver20p   pri
# 946  790                            Rodríguez González Rafael   ver20s   pri
# 947  291                                Benítez Lucho Antonio   ver21p   pri
# 948  791                    Enríquez Merlín Emigdio Heliodoro   ver21s   pri
# 949  484                         Callejas Arroyo Juan Nicolás verrp01p   pri
# 950  984                             González Cerecedo Alicia verrp01s   pri
# 951  485                          Flores Morales Víctor Félix verrp02p   pri
# 952  985                                 García López Ignacia verrp02s   pri
# 953  486                         Terán Velázquez María Esther verrp03p   pri
# 954  986                               Villarino Pérez Arturo verrp03s   pri
# 955  487                          Castillo Andrade Óscar Saúl verrp04p   pan
# 956  987                         Pérez Reséndiz Ma. Felícitas verrp04s   pan
# 957  488                       Monge Villalobos Silvia Isabel verrp05p   pan
# 958  988                                 Martínez Chávez Raúl verrp05s   pan
# 959  489                                  Saldaña Morán Julio verrp06p   pan
# 960  989                         González Cruz Karla Verónica verrp06s   pan
# 961  490                     Téllez Juárez Bernardo Margarito verrp07p   pan
# 962  990                       Campos López María del Rosario verrp07s   pan
# 963  491                                  Sarur Torre Adriana verrp08p  pvem
# 964  991                             Orantes Coello Alejandro verrp08s  pvem
# 965  292                                Vidal Aguilar Liborio   yuc01p  pvem
# 966  792                         Vales Traconis Jorge Alberto   yuc01s  pvem
# 967  293                             Cervera Hernández Felipe   yuc02p   pri
# 968  793                           Alonzo Morales María Ester   yuc02s   pri
# 969  294                     Araujo Lara Angélica del Rosario   yuc03p   pri
# 970  794                       Aguilar Góngora Efraín Ernesto   yuc03s   pri
# 971  295                         Zapata Bello Rolando Rodrigo   yuc04p   pri
# 972  795                          Granja Peniche Daniel Jesús   yuc04s   pri
# 973  296                          Castillo Ruz Martín Enrique   yuc05p   pri
# 974  796                              Vela Reyes Marco Alonso   yuc05s   pri
# 975  492                           Ramírez Marín Jorge Carlos yucrp01p   pri
# 976  992                              Aguilar García Patricia yucrp01s   pri
# 977  493                             Rubio Barthell Eric Luis yucrp02p   pri
# 978  993                       Ruíz Contreras Martha Patricia yucrp02s   pri
# 979  494                            Ávila Ruiz Daniel Gabriel yucrp03p   pan
# 980  994                           Loeza Rodríguez María Inés yucrp03s   pan
# 981  495                             Díaz Lizama Rosa Adriana yucrp04p   pan
# 982  995                       Sánchez Camargo Tito Florencio yucrp04s   pan
# 983  496                         Valencia Vales María Yolanda yucrp05p   pan
# 984  996                                  Vila Dosal Mauricio yucrp05s   pan
# 985  297                              Leyva Hernández Gerardo   zac01p   prd
# 986  797                                Padilla Estrada Jesús   zac01s   prd
# 987  298                                Jiménez Fuentes Ramón   zac02p   prd
# 988  798                            Ávila Cortez Héctor Jaime   zac02s   prd
# 989  299              Verver y Vargas Ramírez Heladio Gerardo   zac03p   prd
# 990  799                              Molina Ramírez Cipriano   zac03s   prd
# 991  300                                Herrera Chávez Samuel   zac04p   prd
# 992  800                                  Chacón Ruíz Joaquín   zac04s   prd
# 993  497                         Mercado Sánchez Luis Enrique zacrp01p   pan
# 994  997                               Dick Neufeld Francisco zacrp01s   pan
# 995  498                                 Ramírez Bucio Arturo zacrp02p   pan
# 996  998                Rivera Velázquez Patricia Guillermina zacrp02s   pan
# 997  499                             Anaya Mota Claudia Edith zacrp03p   prd
# 998  999                                Puente Montes Adriana zacrp03s   prd
# 999  500                                  Narro Céspedes José zacrp04p   prd
#1000 1000                              Regis Adame Juan Carlos zacrp04s   prd
#1001 1001                         Alemán Olvera Emma Margarita mexrpXXp   pan
#1002 1002                            Sánchez Miranda Hugo Lino mexrpXXs   pan
#1003 1003                   González Hernández Yolanda Eugenia cpsrpXXp   pri

J <- 1003 # hubo tres extras para suplir vacantes
coord <- rep("", times = J)
coord[dipdat$id=="mexrp07p"] <- "Rojas"  ## pri
coord[dipdat$id=="mexrp01p"] <- "JVM"    ## pan1
coord[dipdat$id=="jal10p"] <- "R.Acuña"  ## pan2
coord[dipdat$id=="dfrp12p"] <- "Encinas" ## prd1
coord[dipdat$id=="gue03p"] <- "R.Píter"  ## prd2
coord[dipdat$id=="mexrp20p"] <- "Guerra" ## pvem
coord[dipdat$id=="tabrp05p"] <- "J.León" ## conve
coord[dipdat$id=="nlrp06p"] <- "Vzq.Glz" ## pt
coord[dipdat$id=="dfrp25p"] <- "Kahwagi" ## panal
#
dcoord <- rep(NA, times = J)
dcoord[dipdat$id=="mexrp07p"] <- 1  ## pri
dcoord[dipdat$id=="mexrp01p"] <- 1    ## pan1
dcoord[dipdat$id=="jal10p"] <- 1  ## pan2
dcoord[dipdat$id=="dfrp12p"] <- 1 ## prd1
dcoord[dipdat$id=="gue03p"] <- 1  ## prd2
dcoord[dipdat$id=="mexrp20p"] <- 1 ## pvem
dcoord[dipdat$id=="tabrp05p"] <- 1 ## conve
dcoord[dipdat$id=="nlrp06p"] <- 1 ## pt
dcoord[dipdat$id=="dfrp25p"] <- 1 ## panal
## 
color <- rep(".", times = J) 
color[dipdat$part=="pan"] <- "darkblue" 
color[dipdat$part=="pri"] <- "forestgreen" 
color[dipdat$part=="prd"] <- "gold" 
color[dipdat$part=="pt"] <- "red" 
color[dipdat$part=="pvem"] <- "darkolivegreen2" 
color[dipdat$part=="conve"] <- "orange" 
color[dipdat$part=="panal"] <- "cyan" 
## 
part.list <- c("PAN", "PRI", "PRD", "PT", "PVEM", 
                "Conv.", "PANAL")
color.list <- c("darkblue", "forestgreen", "gold", 
                "red", "darkolivegreen2", "orange", 
                "cyan")
##
dipdat$color <- as.character(color)
dipdat$dcoord <- dcoord
dipdat$coord <- coord
rm(color, dcoord, coord)
##
## RECODE RCs TO -1=nay 0=abstain/absent 1=aye
rc <- apply(rc, 2, recode, recodes="2=-1; 3=0; 4=0; 5=0") 
##

## VOTE AGGREGATES FUNCTION
count.votes <- function (X)
{
   ayes <- length(X[is.na(X)==FALSE & X==1])
   nays <- length(X[is.na(X)==FALSE & X==-1])
   valid <- ayes+nays
   abs <- length(X[is.na(X)==FALSE & X==0])
   tot <- valid+abs
   categ <- c (ayes, nays, valid, abs, tot)
   return (categ)
}
##
names(votdat)[1:5] <- c("ayes","nays","valid","abs","tot") ## CHANGE VOTE STATS
I <- dim(rc)[1]; J <- dim(rc)[2]
for (i in 1:I){
    votdat$ayes[i] <- count.votes(rc[i,])[1]
    votdat$nays[i] <- count.votes(rc[i,])[2]
    votdat$valid[i] <- count.votes(rc[i,])[3]
    votdat$abs[i] <- count.votes(rc[i,])[4]
    votdat$tot[i] <- count.votes(rc[i,])[5]
    votdat$ayesPan[i] <- count.votes(rc[i,dipdat$part=="pan"])[1]
    votdat$naysPan[i] <- count.votes(rc[i,dipdat$part=="pan"])[2]
    votdat$ayesPri[i] <- count.votes(rc[i,dipdat$part=="pri"])[1]
    votdat$naysPri[i] <- count.votes(rc[i,dipdat$part=="pri"])[2]
    votdat$ayesPrd[i] <- count.votes(rc[i,dipdat$part=="prd"])[1]
    votdat$naysPrd[i] <- count.votes(rc[i,dipdat$part=="prd"])[2]
    votdat$ayesPt[i] <- count.votes(rc[i,dipdat$part=="pt"])[1]
    votdat$naysPt[i] <- count.votes(rc[i,dipdat$part=="pt"])[2]
    votdat$ayesPvem[i] <- count.votes(rc[i,dipdat$part=="pvem"])[1]
    votdat$naysPvem[i] <- count.votes(rc[i,dipdat$part=="pvem"])[2]
    votdat$ayesConve[i] <- count.votes(rc[i,dipdat$part=="conve"])[1]
    votdat$naysConve[i] <- count.votes(rc[i,dipdat$part=="conve"])[2]
    votdat$ayesPanal[i] <- count.votes(rc[i,dipdat$part=="panal"])[1]
    votdat$naysPanal[i] <- count.votes(rc[i,dipdat$part=="panal"])[2]
              }
##

##
#for(i in 1:I){
#    print(colnames(rc)[i])
#    print(table(rc[,i]))
#    }

## PROPIETARIO/SUPLENTE/NEITHER WAS MEMBER
votdat$date <- as.Date(paste(votdat$yr[1:I],votdat$mo[1:I],votdat$dy[1:I],sep="-"))
## IF NEED TO SEE DATES; USE FOLLOWING COMMAND
#votdat$date <- format(votdat$date, format="%d %b %y")
#
dipdat$in1 <- dipdat$out1 <- dipdat$in2  <- dipdat$out2 <- rep(NA,J)
for (j in 1:J){
    dipdat$in1[j]  <- ifelse( dipdat$yrin1[j]=="." ,  NA, as.Date(paste(dipdat$yrin1[j],dipdat$moin1[j],dipdat$dyin1[j],sep="-")) )
    dipdat$out1[j] <- ifelse( dipdat$yrout1[j]==".",  NA, as.Date(paste(dipdat$yrout1[j],dipdat$moout1[j],dipdat$dyout1[j],sep="-")) )
    dipdat$in2[j]  <- ifelse( dipdat$yrin2[j]=="." ,  NA, as.Date(paste(dipdat$yrin2[j],dipdat$moin2[j],dipdat$dyin2[j],sep="-")) )
    dipdat$out2[j] <- ifelse( dipdat$yrout2[j]==".",  NA, as.Date(paste(dipdat$yrout2[j],dipdat$moout2[j],dipdat$dyout2[j],sep="-")) )
    }
## IF NEED TO SEE DATES; USE FOLLOWING COMMAND
#dipdat$in1  <- format(dipdat$in1 , format="%d %b %y")
#dipdat$out1 <- format(dipdat$out1, format="%d %b %y")
#dipdat$in2  <- format(dipdat$in2 , format="%d %b %y")
#dipdat$out2 <- format(dipdat$out2, format="%d %b %y")
#
dmember <- rc
dmember[,] <- NA
#
dmember[,is.na(dipdat$in1)==TRUE] <- 0 
for (i in 1:I){
    dmember[i,(votdat$date[i]< dipdat$in1  & is.na(dipdat$in1)==FALSE)] <- 0;
    dmember[i,(dipdat$in1<=votdat$date[i]  & is.na(dipdat$in1)==FALSE)] <- 1;
    dmember[i,(dipdat$out1<=votdat$date[i] & is.na(dipdat$out1)==FALSE)] <- 0;
    dmember[i,(dipdat$in2<=votdat$date[i]  & is.na(dipdat$in2)==FALSE)] <- 1;
    dmember[i,(dipdat$out2<=votdat$date[i] & is.na(dipdat$out2)==FALSE)] <- 0;
    }
rm(i,j)

## DEPUTY'S ABSTENTION RATE OVERALL (TO DROP THOSE NOT VOTING ENOUGH OVERALL)
noVoteRate <- as.numeric(rc[1,])
for (j in 1:J){
    noVoteRate[j] <- count.votes(rc[,j])[4] / count.votes(rc[,j])[5]
#    noVoteRate[j] <- length(rc[rc[,j]==0,d])/dim(rc)[1]
    }
dipdat$noVoteRate <- noVoteRate
rm(noVoteRate)
#
## DEPUTY'S ABSTENTION RATE WHEN CHAMBER MEMBER (FOR DESCRIPTIVE STATS)
noVoteRateMem <- as.numeric(rc[1,])
tmp <- rc*dmember  
## DMEMBER STILL HAS PROBLEMS, IN/OUT DATES DO NOT MATCH VOTES EXACTLY
j <- 75; cbind(dmember[,j], rc[,j]) ## AS THIS SHOWS FOR SOME js
for (j in 1:J){
    dmember[abs(rc[,j])==1,j] <- 1  ## IMPERFECT BUT PRACTICAL SOLUTION: IMPLY dmember IS ONE if rc IS -1 or 1
    }
ttmp <- as.numeric(rc[1,])
tmp <- rc*dmember  
for (j in 1:J){
    ttmp[j] <- sum(dmember[,j])
    noVoteRateMem[j] <- 1-count.votes(tmp[,j])[3] / sum(dmember[,j])
    }
dipdat$noVoteRateMem <- noVoteRateMem
rm(j, tmp, ttmp, noVoteRateMem)
##
## DROP DIPUTADOS WHO NEVER PLEDGED
dropUnpledged <- rep(0, times = J)
for (j in 1:J){
    dropUnpledged[j] <- ifelse(count.votes(rc[,j])[3]==0, -1, 0)
                 }
length(dropUnpledged[dropUnpledged<0])  ## HOW MANY NEVER PLEDGED (DESCRIPTIVES)
dropUnpledged <- dropUnpledged * (1:1003)
dropUnpledged <- dropUnpledged[dropUnpledged<0] ## NEGATIVE INDEX FOR THOSE WHO NEVER PLEDGED
rc <- rc[,dropUnpledged]; dipdat <- dipdat[dropUnpledged,]; dmember <- dmember[,dropUnpledged]
tmp <- rc; tmp <- t(tmp); table(tmp) ## CHECA QUE TODO SEA -1 0 1
J <- dim(rc)[2]

##
## DROP DIPUTADOS WHO VOTED IN LESS THAN 10% OF ALL VOTES
D <- dim(dipdat)[1]
dropAbstainers <- rep(0, times=D)
for (d in 1:D){
    dropAbstainers[d] <- ifelse(dipdat$noVoteRate[d]>.9, -1, 0)
                 }
##
length(dropAbstainers[dropAbstainers<0])  ## HOW MANY HAD TOO FEW VOTES (DESCRIPTIVES)
dipdat$id[dropAbstainers<0]               ## WHO THEY ARE (MOSTLY SUPLENTES)
##
dropAbstainers <- dropAbstainers * (1:D)
dropAbstainers <- dropAbstainers[dropAbstainers<0] ## NEGATIVE INDEX FOR THOSE WHO NEVER VOTED
rc <- rc[,dropAbstainers]; dipdat <- dipdat[dropAbstainers,]
rm(d, D, dropAbstainers, dropUnpledged)
##
## DROP UNCONTESTED VOTES
I <- dim(rc)[1]
dropUncontested <- ifelse(votdat$ayes==0 | votdat$nays==0, -1, 0)
##
length(dropUncontested[dropUncontested<0])  ## HOW MANY UNCONTESTED VOTES (DESCRIPTIVES)
##
dropUncontested <- dropUncontested * (1:I)
dropUncontested <- dropUncontested[dropUncontested<0] ## NEGATIVE INDEX 
rc <- rc[dropUncontested,]
votdat <- votdat[dropUncontested,]
rm(j, dropUncontested, tmp)
##
## DROP UNIFORMATIVE VOTES WITH MINORITY < 2.5%
I <- dim(rc)[1]
dropUninformative <- ifelse( votdat$ayes/votdat$valid<.025 | votdat$nays/votdat$valid<.025, -1, 0)
##
length(dropUninformative[dropUninformative<0])  ## HOW MANY UNINFORMATIVE VOTES (DESCRIPTIVES)
##
dropUninformative <- dropUninformative * (1:I)
dropUninformative <- dropUninformative[dropUninformative<0] ## NEGATIVE INDEX 
rc <- rc[dropUninformative,]
votdat <- votdat[dropUninformative,]
rm(dropUninformative)

#### WORK WITH SAMPLE OF VOTES IF SO WISHED
##rnd <- runif(dim(rc)[1])
##rc <- rc[rnd<.3,]
##votdat <- votdat[rnd<.3,]
####
#### WORK WITH SAMPLE OF DIPUTADOS IF SO WISHED
##rnd <- runif(dim(rc)[2])
##rc <- rc[,rnd<.25]
##dipdat <- dipdat[rnd<.25,]
####
##J <- ncol(rc); I <- nrow(rc)

## ## ONE-DIM ARRANGEMENT
## ## AGREEMENT MATRIX --- LA GUARDE PORQUE ESTO TARDA AÑOS
## load("agreeMatrix.Rdata")
## #agreeMatrix <- matrix(NA, ncol=J, nrow=J); tmp <- rep(NA, times=I)
## #datt <- t(dat)
## #for (j in 1:J){
## #    agreeMatrix[j,j] <- 1  ## DIAGONAL
## #              }
## #for (j1 in 2:J){
## #    for (j2 in (j1-1):1){
## #        for (i in 1:I){
## #            tmp[i] <- ifelse(datt[i,j1]==datt[i,j2], 1, 0)
## #                      }
## #        agreeMatrix[j2,j1] <- sum(tmp)/I; agreeMatrix[j1,j2] <- agreeMatrix[j2,j1]
## #        print( paste("j1 =",j1,"; j2 =",j2) )
## #                        }
## #               } 
## #rm(datt)
## #save(agreeMatrix, file="agreeMatrix.Rdata")
## ## SQUARED DISTANCES
## sd <- (1-agreeMatrix)^2
## ## DOUBLE-CENTRED MATRIX
## pmean <- rep(NA, times=J); mmat <- mean(sd); dc <- sd
## for (j in 1:J){
##     pmean[j] <- mean(sd[j,])
##               }
## for (r in 1:J){
##     for (c in 1:J){
##         dc[r,c] <- (sd[r,c] - pmean[r] - pmean[c] + mmat)/-2
##                   }
##               }
## ## SIMPLE ONE-DIM IDEAL POINTS
## tmp <- sqrt(dc[1,1])
## ip  <- c(tmp, dc[2:J,1]/tmp)
## summary(ip)
## ##
## ## EXTREMA DERECHA
## thr <- .14
## data.frame(ip=ip[c(1:J)[ip>thr]], id=dipdat$id[c(1:J)[ip>thr]], nom=dipdat$nom[c(1:J)[ip>thr]], part=dipdat$part[c(1:J)[ip>thr]], noVote=dipdat$noVoteRate[c(1:J)[ip>thr]])
## ##EXTREMA IZQUIERDA
## thr <- -.185
## data.frame(ip=ip[c(1:J)[ip<thr]], id=dipdat$id[c(1:J)[ip<thr]], nom=dipdat$nom[c(1:J)[ip<thr]], part=dipdat$part[c(1:J)[ip<thr]], noVote=dipdat$noVoteRate[c(1:J)[ip< thr]])
## ##
## tmp <- dat[,dimnames(dat)[[2]]=="dpanid" | dimnames(dat)[[2]]=="dpriid" | dimnames(dat)[[2]]=="dprdid"]
## tmp[,4] <- 1 - tmp[,1] - tmp[,2] - tmp[,3]
## dimnames(tmp)[[2]][4] <- "dothid"
## tmp[,2] <- tmp[,2]*2; tmp[,3] <- tmp[,3]*3; tmp[,4] <- tmp[,4]*4
## part <- as.vector(apply(tmp, 1, sum))
## #
## rnd <- (runif(dim(dat)[1])-.5)/10 ## random jitter
## plot(c(-.5,.3), c(1,4), type="n")
## for (j in 1:J){
##     points(ip[j], part[j]+rnd[j], pch=20, cex=.5, col=envud$color[j])
##     }
## # 
## ##################################################
## ### FACTOR ANALYSIS TO ESTIMATE DIMENSIONALITY ###
## ##################################################
## #
## ### ALLOWS TO DROP CASES FROM ANALYSIS
## #year1 <- ifelse( (cuad==1 | cuad==2 | cuad==3), 1, 0 ) ## FALTA QUITAR A UN DIPUTADO (M?S?) QUE NO ENTR? HASTA DESPU?S
## #drop <- ifelse(year1==0,1,0)
## #tmp<-RCs[drop==0,]
## #RCs<-tmp
## #sem<-sem[drop==0]; cuad<-cuad[drop==0]; trim<-trim[drop==0]
## #
## #votes <- rc
## #votes[votes==-1] <- 0  # los -1s se vuelven 0s # DEJA ABSTENCION == NAY  
## #votes <- t(votes)
## #votes <- votes[,1:50] ## subset to test
## #
## cor(dat)
## factanal(dat, factors=4) # varimax is the default
## #
## factanal(dat, factors=3, rotation="promax")
## #
## # A little demonstration, v2 is just v1 with noise,
## # and same for v4 vs. v3 and v6 vs. v5
## # Last four cases are there to add noise
## # and introduce a positive manifold (g factor)
## v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
## v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
## v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
## v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
## v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
## v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
## m1 <- cbind(v1,v2,v3,v4,v5,v6)
## cor(m1)
## factanal(m1, factors=3) # varimax is the default
## factanal(m1, factors=3, rotation="promax")
## # The following shows the g factor as PC1
## prcomp(m1)
## #
## ## formula interface
## factanal(~v1+v2+v3+v4+v5+v6, factors = 3,
##         scores = "Bartlett")$scores
## #
## ## a realistic example from Barthlomew (1987, pp. 61-65)
## example(ability.cov)

## ROLL RATES A LA JENKINS+MONROE
votdat$pass <- ifelse(votdat$ayes > votdat$nays, 1, 0)
rollPan <- rollPri <- rollPrd <- rollPvem <- rollPanal <- rollPt <- rollConve <- votdat
#
rollPan$favors <- ifelse( votdat$ayesPan > votdat$naysPan, 1, 0 )
rollPan$block <-   ifelse ( rollPan$favors==0 & votdat$pass==0, 1, 0 )
rollPan$success <- ifelse ( rollPan$favors==1 & votdat$pass==1, 1, 0 )
rollPan$roll <-    ifelse ( rollPan$favors==0 & votdat$pass==1, 1, 0 )
rollPan$disapp <-  ifelse ( rollPan$favors==1 & votdat$pass==0, 1 ,0 )
rollPan <- rollPan[,c("block","success","roll","disapp")]
#
rollPri$favors <- ifelse( votdat$ayesPri > votdat$naysPri, 1, 0 )
rollPri$block <-   ifelse ( rollPri$favors==0 & votdat$pass==0, 1, 0 )
rollPri$success <- ifelse ( rollPri$favors==1 & votdat$pass==1, 1, 0 )
rollPri$roll <-    ifelse ( rollPri$favors==0 & votdat$pass==1, 1, 0 )
rollPri$disapp <-  ifelse ( rollPri$favors==1 & votdat$pass==0, 1 ,0 )
rollPri <- rollPri[,c("block","success","roll","disapp")]
#
rollPrd$favors <- ifelse( votdat$ayesPrd > votdat$naysPrd, 1, 0 )
rollPrd$block <-   ifelse ( rollPrd$favors==0 & votdat$pass==0, 1, 0 )
rollPrd$success <- ifelse ( rollPrd$favors==1 & votdat$pass==1, 1, 0 )
rollPrd$roll <-    ifelse ( rollPrd$favors==0 & votdat$pass==1, 1, 0 )
rollPrd$disapp <-  ifelse ( rollPrd$favors==1 & votdat$pass==0, 1 ,0 )
rollPrd <- rollPrd[,c("block","success","roll","disapp")]
#
rollPvem$favors <- ifelse( votdat$ayesPvem > votdat$naysPvem, 1, 0 )
rollPvem$block <-   ifelse ( rollPvem$favors==0 & votdat$pass==0, 1, 0 )
rollPvem$success <- ifelse ( rollPvem$favors==1 & votdat$pass==1, 1, 0 )
rollPvem$roll <-    ifelse ( rollPvem$favors==0 & votdat$pass==1, 1, 0 )
rollPvem$disapp <-  ifelse ( rollPvem$favors==1 & votdat$pass==0, 1 ,0 )
rollPvem <- rollPvem[,c("block","success","roll","disapp")]
#
rollPanal$favors <- ifelse( votdat$ayesPanal > votdat$naysPanal, 1, 0 )
rollPanal$block <-   ifelse ( rollPanal$favors==0 & votdat$pass==0, 1, 0 )
rollPanal$success <- ifelse ( rollPanal$favors==1 & votdat$pass==1, 1, 0 )
rollPanal$roll <-    ifelse ( rollPanal$favors==0 & votdat$pass==1, 1, 0 )
rollPanal$disapp <-  ifelse ( rollPanal$favors==1 & votdat$pass==0, 1 ,0 )
rollPanal <- rollPanal[,c("block","success","roll","disapp")]
#
rollPt$favors <- ifelse( votdat$ayesPt > votdat$naysPt, 1, 0 )
rollPt$block <-   ifelse ( rollPt$favors==0 & votdat$pass==0, 1, 0 )
rollPt$success <- ifelse ( rollPt$favors==1 & votdat$pass==1, 1, 0 )
rollPt$roll <-    ifelse ( rollPt$favors==0 & votdat$pass==1, 1, 0 )
rollPt$disapp <-  ifelse ( rollPt$favors==1 & votdat$pass==0, 1 ,0 )
rollPt <- rollPt[,c("block","success","roll","disapp")]
#
rollConve$favors <- ifelse( votdat$ayesConve > votdat$naysConve, 1, 0 )
rollConve$block <-   ifelse ( rollConve$favors==0 & votdat$pass==0, 1, 0 )
rollConve$success <- ifelse ( rollConve$favors==1 & votdat$pass==1, 1, 0 )
rollConve$roll <-    ifelse ( rollConve$favors==0 & votdat$pass==1, 1, 0 )
rollConve$disapp <-  ifelse ( rollConve$favors==1 & votdat$pass==0, 1 ,0 )
rollConve <- rollConve[,c("block","success","roll","disapp")]

apply(rollPan, 2, sum)
apply(rollPri, 2, sum)
apply(rollPrd, 2, sum)
apply(rollPvem, 2, sum)
apply(rollPanal, 2, sum)
apply(rollPt, 2, sum)
apply(rollConve, 2, sum)


##########################################################################
###   Static 2Dimensions four deputy anchors, irt paremeterization     ###
##########################################################################

## SORT BY PARTY
tmp <- 1:dim(rc)[1]
tmp <- ifelse ( dipdat$part=="pan", tmp,
        ifelse ( dipdat$part=="pri", tmp+1000,
         ifelse ( dipdat$part=="prd", tmp+2000,
          ifelse ( dipdat$part=="pt", tmp+3000,
           ifelse ( dipdat$part=="pvem", tmp+4000,
            ifelse ( dipdat$part=="conve", tmp+5000,
             ifelse ( dipdat$part=="panal", tmp+6000, tmp+7000 )))))))
## rcold <- rc; dipold <- dipdat
rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## PARTY INDICES (FOR ANCHORS)
PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]);
PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])
PVEM <- PT+length(tmp[tmp>4000&tmp<5001]); CONVE <- PVEM+length(tmp[tmp>5000&tmp<6001])
PANAL <- CONVE+length(tmp[tmp>6000&tmp<7001])

###########
## MODEL ##
###########
# # runjags version (comparable con Memo)
model2Dj.irt = "model {
for (j in 1:J){                ## loop over respondents
    for (i in 1:I){              ## loop over items
#old#      #v.hat[j,i] ~ dbern(p[j,i]);                            ## voting rule
#old#      #p[j,i] <- phi(v.star[j,i]);                            ## sets 0<p<1
#old#      v.star[j,i] ~ dnorm(mu[j,i],1)T(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
      v[j,i] ~ dbern(p[j,i]);                                 ## voting rule
      probit(p[j,i]) <- mu[j,i];                              ## sets 0<p<1 as function of mu
      mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j];     ## utility differential
                  }
                }
## ESTO LO PUEDO SACAR POST ESTIMACION
##  for (i in 1:I){
##  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
##  b[i] <- alpha[i] / delta[i] ## constante de cutline
##  }
  ## priors ################
for (j in 1:PAN){
    x[j] ~  dnorm(0, 4)   # PAN
    y[j] ~  dnorm(1, 4)
    }
for (j in (PAN+1):PRI){
    x[j] ~  dnorm(1, 4)    # PRI
    y[j] ~  dnorm(-1, 4)
    }
for (j in (PRI+1):PRD){
    x[j] ~  dnorm(-1, 4)    # PRD
    y[j] ~  dnorm(0, 4)
    }
for (j in (PRD+1):J){
    x[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
    y[j] ~  dnorm(0, .1)
    }
    for(i in 1:I){
        alpha[i] ~ dnorm( 0, 1)
        beta[i]  ~ dnorm( 0, 1)
        delta[i] ~ dnorm( 0, 1)
                 }
}"
#end model##############

# WITH ROLL CALLS, rc NEEDS 2 B TRANSPOSED, ITEMS IN COLUMNS, LEGISLATORS IN ROWS
v <- t(rc)
J <- nrow(v); I <- ncol(v)
## RECODE v TO 0=nay NA=abstain 1=aye
v <- apply(v, 2, recode, recodes="-1=0; 0=NA")
# runjags version
ip.parameters <- c("delta","beta", "alpha", "x", "y") #, "deviance")
ip.inits.1 <- dump.format (list(x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I)))
ip.inits.2 <- dump.format (list(x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I)))
ip.inits.3 <- dump.format (list(x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I)))
ip.data <- dump.format (list (v=as.matrix(v), J=J, I=I, PAN=PAN, PRI=PRI, PRD=PRD))
## rjags version
# ip.parameters <- c("delta","beta", "alpha", "x", "y")
# ip.inits<-function() {
#   list(x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I))
#   }
# ip.data <- list ("dat"=as.matrix(v), "J"=J, "I"=I, "PAN"=PAN, "PRI"=PRI, "PRD"=PRD)

#test ride first to see program works
###myWrap <- function(){
## runjags version
    results <- run.jags (
          model=model2Dj.irt,
          monitor=ip.parameters,
          n.chains=1,
          data=ip.data,
#          inits=list( ip.inits.1, ip.inits.2, ip.inits.3 ), ## USE IF RUNNING 3 CHAINS AT ONCE
          inits=list( ip.inits.1, ip.inits.2, ip.inits.3 )[[ch]], ## IF RUNNING CHAINS SEPARATE, SET ch AT START
          thin=50,
          burnin=5000,
          sample=5000,
          check.conv=FALSE,
     #     jags = "c:/Archivos de programa/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
     #     jags = "c:/Program Files (x86)/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
          jags = "C:/Program Files/JAGS/JAGS-3.1.0/x64/bin/jags-terminal.exe",
          plots=TRUE
     )
## rjags version
#    modelInit <- jags.model (
#         "model2Dj.irt.jag",
#         inits=ip.inits,
#         data=ip.data,
#         n.chains=3,
#         n.adapt=9000)
#    results <- coda.samples (modelInit,
#         ip.parameters,
#         n.iter=1000,
#         thin=10)
####    chains <- results[[1]]
####    for(z in 2:3){
####    chains[[z]]<-as.mcmc(results[[z]])
####    }
###    return(results)
###}
###print(system.time(myWrap()))

## change .ch number appropriately for each chain
results.3 <- results
save(results.3, file="tmp3.RData")
#rm(results)

## COMBINE UNICHAINS RUN SEPARATELY: LOAD EACH CHAIN NAMED results.ch, THEN:
load("tmp2.RData"); load("tmp3.RData")
## results <- results.1 ## so that it inherits mcmc.list attribute
## results[[2]] <- results.2[[1]]
## results[[3]] <- results.3[[1]]
chains <- list ( as.mcmc(results$mcmc), as.mcmc(results.2$mcmc), as.mcmc(results.3$mcmc) )

## CONVERGENCE DIAGNOSTIC... WON'T WORK, TOO FEW ITERATIONS?
gelman.diag(chains, confidence = 0.95, transform=FALSE, autoburnin=FALSE)

## PLOT CHAINS TO CHECK CONVERGENCE
tmp <- dimnames(results$mcmc[[1]]); tmp <- tmp[[2]]
cplt <- function(X)
    {
    tmp2 <- min(as.numeric(chains[[1]][,X]), as.numeric(chains[[2]][,X]), as.numeric(chains[[3]][,X]));
#    tmp2 <- min(as.numeric(results$mcmc[[1]][,X]), as.numeric(results[[2]][,X]), as.numeric(results[[3]][,X]));
    tmp3 <- max(as.numeric(chains[[1]][,X]), as.numeric(chains[[2]][,X]), as.numeric(chains[[3]][,X]));
    plot(c(1,results$sample/results$thin), c(tmp2,tmp3), type="n", main=tmp[X]);
    lines(1:(results$sample/results$thin), as.numeric(chains[[1]][,X]),col="red");
    lines(1:(results$sample/results$thin), as.numeric(chains[[2]][,X]),col="blue");
    lines(1:(results$sample/results$thin), as.numeric(chains[[3]][,X]),col="green");
    }
##
setwd(paste(workdir, "/graphs/convergence", sep=""))
for (i in 1:length(chains[[1]][1,]))
    {
    pdf( paste("tmp", i, ".pdf", sep=""))
    cplt(i)
    dev.off()
    }
setwd(workdir)

#to continue running in Jags
tmp.1 <- dump.format (list(
    v = results$end.state$Chain.1$v,
    x = results$end.state$Chain.1$x, 
    y = results$end.state$Chain.1$y, 
    alpha = results$end.state$Chain.1$alpha, 
    delta = results$end.state$Chain.1$delta, 
    beta = results$end.state$Chain.1$beta
    ))
tmp.2 <- dump.format (list(
    v = results.2$end.state$Chain.1$v,
    x = results.2$end.state$Chain.1$x, 
    y = results.2$end.state$Chain.1$y, 
    alpha = results.2$end.state$Chain.1$alpha, 
    delta = results.2$end.state$Chain.1$delta, 
    beta = results.2$end.state$Chain.1$beta
    ))
tmp.3 <- dump.format (list(
    v = results.3$end.state$Chain.1$v,
    x = results.3$end.state$Chain.1$x, 
    y = results.3$end.state$Chain.1$y, 
    alpha = results.3$end.state$Chain.1$alpha, 
    delta = results.3$end.state$Chain.1$delta, 
    beta = results.3$end.state$Chain.1$beta
    ))
rm(results, results.2, results.3, ip.inits.1, ip.inits.2, ip.inits.3) ## FREES MEMORY
results <- run.jags (
      model=model2Dj.irt,
      monitor=ip.parameters,
      n.chains=1,
      data=ip.data,
 #     inits=list( tmp.1, tmp.2, tmp.3 ), ## USE IF RUNNING 3 CHAINS AT ONCE
      inits=list( tmp.1, tmp.2, tmp.3 )[[ch]], ## IF RUNNING CHAINS SEPARATE, SET ch AT START
      thin=50,
      burnin=5000,
      sample=5000,
      check.conv=FALSE,
 #     jags = "c:/Archivos de programa/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
 #     jags = "c:/Program Files (x86)/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
      jags = "C:/Program Files/JAGS/JAGS-3.1.0/x64/bin/jags-terminal.exe",
      plots=TRUE
 )


#post.draw <- rbind(envud$mcmc[[1]], envud$mcmc[[2]], envud$mcmc[[3]])  ## runjags version
post.draw <- rbind(chains[[1]], chains[[2]]) ##, chains[[3]])
#post.draw <- rbind(results[[1]], results[[2]], results[[3]])  ## rjags version
post.x     <- post.draw[,grep("x", colnames(post.draw))]
post.y     <- post.draw[,grep("y", colnames(post.draw))]
post.alpha <- post.draw[,grep("alpha", colnames(post.draw))]
post.beta  <- post.draw[,grep("beta", colnames(post.draw))]
post.delta <- post.draw[,grep("delta", colnames(post.draw))]

## SACA CONSTANTE Y PENDIENTE DE CUTLINES
a <- post.beta / post.delta  ## pendiente de cutline
b <- post.alpha / post.delta ## constante de cutline

## 45-DEGREE CLOCKWISE ROTATION OF COORDINATES (SO THAT PRIORS REMAIN n, s, e, w)
xR <- post.x*cos(pi/4) + post.y*sin(pi/4)
yR <- -post.x*sin(pi/4) + post.y*cos(pi/4)
post.x <- xR; post.y <- yR;
## 45-DEGREE CLOCKWISE ROTATION OF CUTLINES
xA <- -b/a; yA <- rep(0, length(a)); xO <- rep(0, length(a)); yO <- b  ## coords de Abscisa al origen y Ordenada al origan de c/cutline
xAR <- xA*cos(pi/4) + yA*sin(pi/4)
yAR <- -xA*sin(pi/4) + yA*cos(pi/4)
xOR <- xO*cos(pi/4) + yO*sin(pi/4)
yOR <- -xO*sin(pi/4) + yO*cos(pi/4)
X <- xAR; Y <- yAR; XX <- xOR; YY <- yOR ## simplifica notación
aR <- (YY-Y)/(XX-X)           ## pendiente del cutline rotado
bR <- YY -((YY-Y)/(XX-X))*XX  ## constante del cutline rotado
rm(X,Y,XX,YY)
a <- aR; b <- bR


jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (post.x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (post.x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (post.x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (post.y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (post.y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (post.y[,j], 0.975, names=F)
    }


## dipdat$color[dipdat$color=="."] <- "black"
plot(c(-4,2), c(-2.5,3.5), type="n", main="61st Leg. 2009-11")
points(jotas[,2],jotas[,5], pch=19, cex=.75, col=dipdat$color)

amed <- rep(NA,times=I)
bmed <- rep(NA,times=I)
for (i in 1:I){
    amed[i] <- quantile (a[,i], 0.50, names=F)
    bmed[i] <- quantile (b[,i], 0.50, names=F)  }

### Exporta coordenadas de todos los diputados
tmp <- matrix(NA, nrow=67, ncol=4)
tmp[,1] <- as.numeric(jotas[,2])
tmp[,2] <- jotas[,5]
tmp[,3] <- names.67
tmp[,4] <- part.67
tmp<-data.matrix(tmp)
tmp[,1:2] <- as.numeric(tmp[,1:2])
write.table(tmp, file="aldfStaticIdPts.xls", sep=",")






eric  ###################################################################
### static model in Two Dimensions, extremists -- IRT PARAMETERIZATION
###################################################################

## SORT BY PARTY
tmp <- 1:dim(rc)[1]
tmp <- ifelse ( dipdat$part=="pan", tmp, 
        ifelse ( dipdat$part=="pri", tmp+1000, 
         ifelse ( dipdat$part=="prd", tmp+2000, 
          ifelse ( dipdat$part=="pt", tmp+3000, 
           ifelse ( dipdat$part=="pvem", tmp+4000, 
            ifelse ( dipdat$part=="conve", tmp+5000, 
             ifelse ( dipdat$part=="panal", tmp+6000, tmp+7000 )))))))
## rcold <- rc; dipold <- dipdat
rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## PARTY INDICES (FOR ANCHORS)
PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]); 
PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])
PVEM <- PT+length(tmp[tmp>4000&tmp<5001]); CONVE <- PVEM+length(tmp[tmp>5000&tmp<6001])
PANAL <- CONVE+length(tmp[tmp>6000&tmp<7001])

###########
## MODEL ##
###########
model2Dj.irt = "model {
  for (j in 1:J){                ## loop over diputados
    for (i in 1:I){              ## loop over items
      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
      v.star[j,i] ~ dnorm(mu[j,i],1)T(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
      mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j] ## utility differential
                  }
                }
## ESTO LO PUEDO SACAR POST ESTIMACION
##  for (i in 1:I){
##  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
##  b[i] <- alpha[i] / delta[i] ## constante de cutline
##  }
  ## priors ################
for (j in 1:PAN){
    x[j] ~  dnorm(0, 4)   # PAN
    y[j] ~  dnorm(1, 4)
    }
for (j in (PAN+1):PRI){
    x[j] ~  dnorm(1, 4)    # PRI
    y[j] ~  dnorm(-1, 4)
    }
for (j in (PRI+1):PRD){
    x[j] ~  dnorm(-1, 4)    # PRD
    y[j] ~  dnorm(0, 4)
    }
for (j in (PT+1):J){
    x[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
    y[j] ~  dnorm(0, .1)
    }
    for(i in 1:I){
        alpha[i] ~ dnorm( 0, 1)
        beta[i]  ~ dnorm( 0, 1)
        delta[i] ~ dnorm( 0, 1)
                 }
}
"
#end model##############

J <- ncol(rc); I <- nrow(rc)
v <- t(rc)
lo.v <- ifelse(is.na(v)==TRUE | v==  1, 0, -100)
hi.v <- ifelse(is.na(v)==TRUE | v== -1,  0, 100)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
    for (i in 1:I){
        vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
dip.parameters <- c("delta","beta", "alpha", "x", "y") #, "deviance")
dip.inits.1 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I)))
dip.inits.2 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I)))
dip.inits.3 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I)))
dip.data <- dump.format (list (J=J, I=I, lo.v=lo.v, hi.v=hi.v, PAN=PAN, PRI=PRI, PRD=PRD, PT=PT))

#test ride to see program works
#myWrap <- function(){
    result <- run.jags (
         model=model2Dj.irt, 
         monitor=dip.parameters, 
         n.chains=1,
         data=dip.data, 
    #     inits=list( dip.inits.1, dip.inits.2, dip.inits.3 ),
         inits=dip.inits.1,
         thin=10, 
         burnin=100, 
         sample=100,
         check.conv=FALSE, 
    #     jags = "c:/Archivos de programa/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
    #     jags = "c:/Program Files (x86)/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
         jags = "C:/Program Files/JAGS/JAGS-3.1.0/x64/bin/jags-terminal.exe",
         plots=FALSE
    )
#    return(result)
#}
#print(system.time(myWrap()))

#to continue running in Bugs
tmp1<-list (
    v.star=vstar,
    delta=dip.60$last.values[[1]]$delta,
    angle=dip.60$last.values[[1]]$angle,
    b=dip.60$last.values[[1]]$b,
    x=dip.60$last.values[[1]]$x,
    y=dip.60$last.values[[1]]$y
    )
tmp2<-list (
    v.star=vstar,
    delta=dip.60$last.values[[2]]$delta,
    angle=dip.60$last.values[[2]]$angle,
    b=dip.60$last.values[[2]]$b,
    x=dip.60$last.values[[2]]$x,
    y=dip.60$last.values[[2]]$y
    )
tmp3<-list (
    v.star=vstar,
    delta=dip.60$last.values[[3]]$delta,
    angle=dip.60$last.values[[3]]$angle,
    b=dip.60$last.values[[3]]$b,
    x=dip.60$last.values[[3]]$x,
    y=dip.60$last.values[[3]]$y
    )
### for (chain in 1:3){dip.60$last.values[[chain]]$v.star <- vstar}
dip.60.2 <- bugs (dip.data,
                inits=list(tmp1,tmp2,tmp3),
                dip.parameters,
#                "modelSta2Dj.irt.txt", n.chains=3,
                "modelSta2Dj.txt", n.chains=3,
                n.iter=5000, n.thin=25, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

dip.60 <- dip.60.2
rm(dip.60.2)

post.draw <- rbind(dip.60$mcmc[[1]], dip.60$mcmc[[2]], dip.60$mcmc[[3]])
post.x <- post.draw[,grep("x", colnames(post.draw))]
post.y <- post.draw[,grep("y", colnames(post.draw))]

jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (post.x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (post.x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (post.x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (post.y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (post.y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (post.y[,j], 0.975, names=F)
    }


dipdat$color[dipdat$color=="."] <- "black"
plot(jotas[,2],jotas[,5], pch=19, col=dipdat$color)


eric  ###################################################################
### static model in Two Dimensions, extremists -- CUTLINE SPECIFICATION
###################################################################

## SORT BY PARTY
tmp <- 1:dim(rc)[1]
tmp <- ifelse ( dipdat$part=="pan", tmp, 
        ifelse (dipdat$part=="pri", tmp+1000, 
         ifelse ( dipdat$part=="prd", tmp+2000, 
          ifelse ( dipdat$part=="pt", tmp+3000, tmp+4000))))
## rcold <- rc; dipold <- dipdat
rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## PARTY INDICES (FOR ANCHORS)
PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]); 
PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])

###########
## MODEL ##
###########
modelSta2Dj = "model {
  for (j in 1:J){                ## loop over diputados
    for (i in 1:I){              ## loop over items
      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
      v.star[j,i] ~ dnorm(mu[j,i],1)T(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
      mu[j,i] <- delta[i]*a[i]*x[j] + delta[i]*b[i] - delta[i]*y[j] ## utility differential
                  }
                }
  for (i in 1:I){
  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
  }
  ## priors ################
for (j in 1:PAN){
    x[j] ~  dnorm(1, 4)   # PAN
    y[j] ~  dnorm(1, 4)
    }
for (j in (PAN+1):PRI){
    x[j] ~  dnorm(0, 4)    # PRI
    y[j] ~  dnorm(-1, 4)
    }
for (j in (PRI+1):PRD){
    x[j] ~  dnorm(-1, 4)    # PRD
    y[j] ~  dnorm(.5, 4)
    }
for (j in (PRD+1):PT){
    x[j] ~  dnorm(-1, 4)    # PT
    y[j] ~  dnorm(-1, 4)
    }
for (j in (PT+1):J){
    x[j] ~  dnorm(0, .1)
    y[j] ~  dnorm(0, .1)
    }
    for(i in 1:I){
        delta[i] ~ dnorm( 0, 0.1)
        angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
        b[i] ~ dnorm( 0, .1)
                 }
}"
#end model##############

J <- ncol(rc); I <- nrow(rc)
v <- t(rc)
lo.v <- ifelse(is.na(v)==TRUE | v==  1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v== -1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
    for (i in 1:I){
        vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
dip.parameters <- c("delta","angle", "x", "y") #, "deviance")
dip.inits.1 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), angle = rnorm(I), delta = rnorm(I)))
dip.inits.2 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), angle = rnorm(I), delta = rnorm(I)))
dip.inits.3 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), angle = rnorm(I), delta = rnorm(I)))
dip.data <- dump.format (list (J=J, I=I, lo.v=lo.v, hi.v=hi.v, PAN=PAN, PRI=PRI, PRD=PRD, PT=PT))

#test ride to see program works
myWrap <- function(){
    dip.60 <- run.jags (
         model=modelSta2Dj, 
         monitor=dip.parameters, 
         n.chains=3,
         data=dip.data, 
         inits=list( dip.inits.1, dip.inits.2, dip.inits.3 ),
         thin=1, 
         burnin=5, 
         sample=5,
         check.conv=FALSE, 
    #     jags = "c:/Archivos de programa/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
    #     jags = "c:/Program Files (x86)/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
    #     jags = "C:/Program Files/JAGS/JAGS-3.1.0/x64/bin/jags-terminal.exe",
         plots=FALSE
    )
    return(dip.60)
}
print(system.time(myWrap()))

#to continue running in Bugs
tmp1<-list (
    v.star=vstar,
    delta=dip.60$last.values[[1]]$delta,
    angle=dip.60$last.values[[1]]$angle,
    b=dip.60$last.values[[1]]$b,
    x=dip.60$last.values[[1]]$x,
    y=dip.60$last.values[[1]]$y
    )
tmp2<-list (
    v.star=vstar,
    delta=dip.60$last.values[[2]]$delta,
    angle=dip.60$last.values[[2]]$angle,
    b=dip.60$last.values[[2]]$b,
    x=dip.60$last.values[[2]]$x,
    y=dip.60$last.values[[2]]$y
    )
tmp3<-list (
    v.star=vstar,
    delta=dip.60$last.values[[3]]$delta,
    angle=dip.60$last.values[[3]]$angle,
    b=dip.60$last.values[[3]]$b,
    x=dip.60$last.values[[3]]$x,
    y=dip.60$last.values[[3]]$y
    )
### for (chain in 1:3){dip.60$last.values[[chain]]$v.star <- vstar}
dip.60.2 <- bugs (dip.data,
                inits=list(tmp1,tmp2,tmp3),
                dip.parameters,
#                "modelSta2Dj.irt.txt", n.chains=3,
                "modelSta2Dj.txt", n.chains=3,
                n.iter=5000, n.thin=25, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

dip.60 <- dip.60.2
rm(dip.60.2)

post.draw <- rbind(dip.60$mcmc[[1]], dip.60$mcmc[[2]], dip.60$mcmc[[3]])
post.x <- post.draw[,grep("x", colnames(post.draw))]
post.y <- post.draw[,grep("y", colnames(post.draw))]

jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (post.x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (post.x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (post.x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (post.y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (post.y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (post.y[,j], 0.975, names=F)
    }


dipdat$color[dipdat$color=="."] <- "black"
plot(jotas[,2],jotas[,5], pch=19, col=dipdat$color)



##################################################
#### static model in 1 dimension, extremist anchor 
##################################################
#cat("
#model {
#  for (j in 1:J){                ## loop over councilors  
#    for (i in 1:I){              ## loop over items
#     #v.hat[j,i] ~ dbern(p[j,i])                                   ## voting rule
#     #p[j,i] <- phi(y.star[j,i])                                   ## sets 0<p<1
#     v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i])   ## truncated normal sampling
#     mu[j,i] <- delta[i]*x[j] - n[i]                              ## utility differential
#     }
#  }
##  for (i in 1:I){
##     m[i] <- n[i] / delta[i]                                      ## midpoint 
##  } 
#  ## priors
#for (j in 1:(DER-1)){
#    x[j] ~  dnorm(0, .1)
#                    }
#    x[DER] ~  dnorm(-10, 4)    # Mr. RIGHT ags01p PAN
##    x[DER] <- -1
#for (j in (DER+1):(IZQ-1)){
#    x[j] ~  dnorm(0, .1)
#                          }
#    x[IZQ] ~  dnorm(10, 4)   # Mr. LEFT Noro?a PT
##    x[IZQ] <- 1
#for (j in (IZQ+1):J){
#    x[j] ~  dnorm(0, .1)
#                    }
#for (i in 1:I){
#    delta[i] ~ dnorm(0, 0.25)
#    n[i] ~ dnorm( 0, 0.25)
#              }
#}
#", file="modelSta1Dj.txt")
##
##################################################
#### static model in 1 dimension, item anchors
##################################################
#cat("
#model {
#  for (j in 1:J){                ## loop over councilors  
#    for (i in 1:I){              ## loop over items
#     #y.hat[j,i] ~ dbern(p[j,i])                                   ## voting rule
#     #p[j,i] <- phi(y.star[j,i])                                   ## sets 0<p<1
#     y.star[j,i] ~ dnorm(mu[j,i],1)I(lower.y[j,i],upper.y[j,i])   ## truncated normal sampling
#     mu[j,i] <- delta[i]*x[j] - n[i]                              ## utility differential
#     }
#  }
#  for (i in 1:I){
#     m[i] <- n[i] / delta[i]                                      ## midpoint 
#  } 
#  ## priors
#     for (j in 1:J){
#         x[j] ~ dnorm(0, .1)
#                   }
#    for(i in 1:31){
#        delta[i] ~ dnorm( 0, 0.25)
#                  }
#    delta[32] ~ dnorm( 4, 4)      ## folio 390, right=nay
#    for(i in 33:227){
#        delta[i] ~ dnorm( 0, 0.25)
#                   }
#    delta[228] ~ dnorm(-4, 4)      ## folio 1045, right=aye 
#    for(i in 229:I){
#        delta[i] ~ dnorm( 0, 0.25)
#                  }
#    for(i in 1:I){
#        n[i] ~ dnorm( 0, 0.25)
#                 }
#}
#", file="modelSta1Dj.txt")
# #
#### dynamic model for 66 members in Two Dimensions WITH CUTLINE ESTIMATES
#cat("
#model {
#  for (j in 1:J){                ## loop over diputados
#    for (i in 1:I){              ## loop over items
#      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
#      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
#      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling, cf. Jackman
#      mu[j,i] <- delta[i]*a[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i] + xFour[j]*d4[i] + xFive[j]*d5[i] + xSix[j]*d6[i] + xSeven[j]*d7[i] + xEight[j]*d8[i]) 
#                + delta[i]*b[i] - delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i] + yFour[j]*d4[i] + yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i] + yEight[j]*d8[i])  ## utility differential
#                  }
#      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
#      xTwo[j] ~   dnorm (xOne[j],15);
#      xThree[j] ~ dnorm (xTwo[j],15);
#      xFour[j] ~  dnorm (xThree[j],15);
#      xFive[j] ~  dnorm (xFour[j],15);
#      xSix[j] ~   dnorm (xFive[j],15);
#      xSeven[j] ~ dnorm (xSix[j],15);
#      xEight[j] ~ dnorm (xSeven[j],15);
#      yOne[j] ~   dnorm (yZero[j],15);
#      yTwo[j] ~   dnorm (yOne[j],15);
#      yThree[j] ~ dnorm (yTwo[j],15);
#      yFour[j] ~  dnorm (yThree[j],15);
#      yFive[j] ~  dnorm (yFour[j],15);
#      ySix[j] ~   dnorm (yFive[j],15);
#      ySeven[j] ~ dnorm (ySix[j],15);
#      yEight[j] ~ dnorm (ySeven[j],15);
#                }
#  for (i in 1:I){
#  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
#  }
#  ################
#  ## priors
#  ################
#for (j in 1:(N-1)){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#    xZero[N] ~  dnorm(0, 4)    # Mrs. NORTH Pi?a Olmedo Laura (PRD)
#    yZero[N] ~  dnorm(2, 4)    
##    xZero[N] <- 0
##    yZero[N] <- 2
#for (j in (N+1):(W-1)){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#    xZero[W] ~  dnorm(-2, 4)   # Mr. WEST M?ndez Rangel Avelino (PRD)
#    yZero[W] ~  dnorm(0, 4)    
##    xZero[W] <- -2
##    yZero[W] <- 0
#for (j in (W+1):(E-1)){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#    xZero[E] ~  dnorm(2, 4)    # Mrs. EAST Paula Adriana Soto Maldonado (PAN)
#    yZero[E] ~  dnorm(0, 4)    
##    xZero[E] <- 2
##    yZero[E] <- 0
#for (j in (E+1):(S-1)){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#    xZero[S] ~  dnorm(0, 4)    # Mr. SOUTH Tenorio Antiga Xiuh (PANAL)
#    yZero[S] ~  dnorm(-2, 4)    
##    xZero[S] <- 0
##    yZero[S] <- -2
#for (j in (S+1):J){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#    for(i in 1:I){
#        delta[i] ~ dnorm( 0, 0.01)
#        angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#        b[i] ~ dnorm( 0, .01)
#                 }
#}
#", file="model66Dyn2Dj.txt")
##
##
####################################################################
#### static model for 66 members in Two Dimensions, four ITEM anchors WITH CUTLINE ESTIMATES
####################################################################
#cat("
#model {
#  for (j in 1:J){                ## loop over diputados
#    for (i in 1:I){              ## loop over items
#      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
#      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
#      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
#      mu[j,i] <- delta[i]*a[i]*x[j] + delta[i]*b[i] - delta[i]*y[j] ## utility differential
#                  }
#                }
#  for (i in 1:I){
#  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
#  }
#  ## priors ################
#for (j in 1:J){
#    x[j] ~  dnorm(0, 1)
#    y[j] ~  dnorm(0, 1)
#    }
#for(i in 1:(V1-1)){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#delta[V1] ~ dnorm( -4, 4)
#angle[V1] ~ dunif(1.37,1.77) # (7pi/16,9pi/16) ---- VERTICAL
#b[V1] ~ dnorm( 0, 4)
#for(i in (V1+1):(H1-1)){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#delta[H1] ~ dnorm( -4, 4)
#angle[H1] ~ dunif(-0.2,0.2) # (-pi/16,pi/16) ------ HORIZONTAL
#b[H1] ~ dnorm( 0, 4)
#for(i in (H1+1):(V2-1)){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#delta[V2] ~ dnorm( 4, 4)
#angle[V2] ~ dunif(1.37,1.77) # (7pi/16,9pi/16) ---- VERTICAL
#b[V2] ~ dnorm( 0, 4)
#for(i in (V2+1):(H2-1)){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#delta[H2] ~ dnorm( -4, 4)
#angle[H2] ~ dunif(-0.2,0.2) # (-pi/16,pi/16) ------ HORIZONTAL
#b[H2] ~ dnorm( 0, 4)
#for(i in (H2+1):I){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#}
#", file="model66Sta2Di4.txt")
##
####################################################################
#### static model for 66 members in Two Dimensions, two ITEM anchors WITH CUTLINE ESTIMATES
####################################################################
#cat("
#model {
#  for (j in 1:J){                ## loop over diputados
#    for (i in 1:I){              ## loop over items
#      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
#      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
#      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
#      mu[j,i] <- delta[i]*a[i]*x[j] + delta[i]*b[i] - delta[i]*y[j] ## utility differential
#                  }
#                }
#  for (i in 1:I){
#  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
#  }
#  ## priors ################
#for (j in 1:J){
#    x[j] ~  dnorm(0, 1)
#    y[j] ~  dnorm(0, 1)
#    }
#for(i in 1:(V1-1)){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#delta[V1] ~ dnorm( -4, 4)
#angle[V1] ~ dunif(1.37,1.77) # (7pi/16,9pi/16) ---- VERTICAL
#b[V1] ~ dnorm( 0, 4)
#for(i in (V1+1):(H1-1)){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#delta[H1] ~ dnorm( -4, 4)
#angle[H1] ~ dunif(-0.2,0.2) # (-pi/16,pi/16) ------ HORIZONTAL
#b[H1] ~ dnorm( 0, 4)
#for(i in (H1+1):I){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#}
#", file="model66Sta2Di2.txt")
##
#### dynamic model for 66 members in Two Dimensions -- IRT PARAMETERIZATION
#cat("
#model {
#  for (j in 1:J){                ## loop over diputados
#    for (i in 1:I){              ## loop over items
#      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
#      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
#      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
#      mu[j,i] <- beta[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i] + xFour[j]*d4[i] + xFive[j]*d5[i] + xSix[j]*d6[i] + xSeven[j]*d7[i] + xEight[j]*d8[i])
#                - alpha[i] + delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i] + yFour[j]*d4[i] + yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i] + yEight[j]*d8[i])  ## utility differential
#                  }
#      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
#      xTwo[j] ~   dnorm (xOne[j],15);
#      xThree[j] ~ dnorm (xTwo[j],15);
#      xFour[j] ~  dnorm (xThree[j],15);
#      xFive[j] ~  dnorm (xFour[j],15);
#      xSix[j] ~   dnorm (xFive[j],15);
#      xSeven[j] ~ dnorm (xSix[j],15);
#      xEight[j] ~ dnorm (xSeven[j],15);
#      yOne[j] ~   dnorm (yZero[j],15);
#      yTwo[j] ~   dnorm (yOne[j],15);
#      yThree[j] ~ dnorm (yTwo[j],15);
#      yFour[j] ~  dnorm (yThree[j],15);
#      yFive[j] ~  dnorm (yFour[j],15);
#      ySix[j] ~   dnorm (yFive[j],15);
#      ySeven[j] ~ dnorm (ySix[j],15);
#      yEight[j] ~ dnorm (ySeven[j],15);
#                }
### ESTO LO PUEDO SACAR POST ESTIMACION
###  for (i in 1:I){
###  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
###  b[i] <- alpha[i] / delta[i] ## constante de cutline
###  }
#  ################
#  ## priors
#  ################
### 1a dim ############
#for (j in 1:(N-1)){
#    xZero[j] ~  dnorm(0, 1)
#    }
#    xZero[N] ~  dnorm(0, 4)    # Mrs. NORTH Pi?a Olmedo Laura (PRD)
##    xZero[N] <- 0
#for (j in (N+1):(W-1)){
#    xZero[j] ~  dnorm(0, 1)
#    }
#    xZero[W] ~  dnorm(-2, 4)    # Mr. WEST M?ndez Rangel Avelino (PRD)
##    xZero[W] <- -2
#for (j in (W+1):(E-1)){
#    xZero[j] ~  dnorm(0, 1)
#    }
#    xZero[E] ~  dnorm(2, 4)    # Mrs. EAST Paula Adriana Soto Maldonado (PAN)
##    xZero[E] <- 2
#for (j in (E+1):(S-1)){
#    xZero[j] ~  dnorm(0, 1)
#    }
#    xZero[S] ~  dnorm(0, 4)    # Mr. SOUTH Tenorio Antiga Xiuh (PANAL)
##    xZero[S] <- 0
#for (j in (S+1):J){
#    xZero[j] ~  dnorm(0, 1)
#    }
### 2a dim  ############
#for (j in 1:(N-1)){
#    yZero[j] ~  dnorm(0, 1)
#    }
#    yZero[N] ~  dnorm(2, 4)    # Mrs. NORTH
##    yZero[N] <- 2
#for (j in (N+1):(W-1)){
#    yZero[j] ~  dnorm(0, 1)
#    }
#    yZero[W] ~  dnorm(0, 4)    # Mr. WEST
##    yZero[W] <- 0
#for (j in (W+1):(E-1)){
#    yZero[j] ~  dnorm(0, 1)
#    }
#    yZero[E] ~  dnorm(0, 4)    # Mrs. EAST
##    yZero[E] <- 0
#for (j in (E+1):(S-1)){
#    yZero[j] ~  dnorm(0, 1)
#    }
#    yZero[S] ~  dnorm(-2, 4)    # Mr. SOUTH
##    yZero[S] <- -2
#for (j in (S+1):J){
#    yZero[j] ~  dnorm(0, 1)
#    }
#    for(i in 1:I){
#        alpha[i] ~ dnorm( 0, 1)
#        beta[i]  ~ dnorm( 0, 1)
#        delta[i] ~ dnorm( 0, 1)
#                 }
#}
#", file="model66Dyn2Dj.irt.txt")
##
##
########################################################################################
#### dynamic model for 66 members in Two Dimensions Four Item anchors-- IRT PARAMETERIZATION
########################################################################################
#cat("
#model {
#  for (j in 1:J){                ## loop over diputados
#    for (i in 1:I){              ## loop over items
#      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
#      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
#      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
#      mu[j,i] <- beta[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i] 
#                 + xFour[j]*d4[i] + xFive[j]*d5[i] + xSix[j]*d6[i] + xSeven[j]*d7[i] 
#                 + xEight[j]*d8[i])
#                 - alpha[i] + delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i] 
#                 + yFour[j]*d4[i] + yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i] 
#                 + yEight[j]*d8[i])  ## utility differential
#                  }
#      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
#      xTwo[j] ~   dnorm (xOne[j],15);
#      xThree[j] ~ dnorm (xTwo[j],15);
#      xFour[j] ~  dnorm (xThree[j],15);
#      xFive[j] ~  dnorm (xFour[j],15);
#      xSix[j] ~   dnorm (xFive[j],15);
#      xSeven[j] ~ dnorm (xSix[j],15);
#      xEight[j] ~ dnorm (xSeven[j],15);
#      yOne[j] ~   dnorm (yZero[j],15);
#      yTwo[j] ~   dnorm (yOne[j],15);
#      yThree[j] ~ dnorm (yTwo[j],15);
#      yFour[j] ~  dnorm (yThree[j],15);
#      yFive[j] ~  dnorm (yFour[j],15);
#      ySix[j] ~   dnorm (yFive[j],15);
#      ySeven[j] ~ dnorm (ySix[j],15);
#      yEight[j] ~ dnorm (ySeven[j],15);
#                }
### ESTO LO PUEDO SACAR POST ESTIMACION
###  for (i in 1:I){
###  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
###  b[i] <- alpha[i] / delta[i] ## constante de cutline
###  }
#  ################
#  ## priors
#  ################
### 1a dim ############
#for (j in 1:(N-1)){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#    xZero[N] ~  dnorm(-2, 4)    # Mrs. NORTH Pi?a Olmedo Laura (PRD)
#    yZero[N] ~  dnorm(2, 4)    
##    xZero[N] <- 0
##    yZero[N] <- 2
#for (j in (N+1):J){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#for(i in 1:(V1-1)){
#    alpha[i] ~ dnorm( 0, 1)
#    beta[i]  ~ dnorm( 0, 1)
#    delta[i] ~ dnorm( 0, 1)
#    }
#alpha[V1] ~ dnorm( 0, 1)
#beta[V1]  ~ dnorm(-4, 20)
#delta[V1] ~ dnorm(-4, 20)
#for(i in (V1+1):(H1-1)){
#    alpha[i] ~ dnorm( 0, 1)
#    beta[i]  ~ dnorm( 0, 1)
#    delta[i] ~ dnorm( 0, 1)
#    }
#alpha[H1] ~ dnorm( 0, 1)
#beta[H1]  ~ dnorm( 4, 20)
#delta[H1] ~ dnorm(-4, 20)
#for(i in (H1+1):(V2-1)){
#    alpha[i] ~ dnorm( 0, 1)
#    beta[i]  ~ dnorm( 0, 1)
#    delta[i] ~ dnorm( 0, 1)
#    }
#alpha[V2] ~ dnorm( 0, 1)
#beta[V2]  ~ dnorm( 4, 20)
#delta[V2] ~ dnorm( 4, 20)
#for(i in (V2+1):(H2-1)){
#    alpha[i] ~ dnorm( 0, 1)
#    beta[i]  ~ dnorm( 0, 1)
#    delta[i] ~ dnorm( 0, 1)
#    }
#alpha[H2] ~ dnorm( 0, 1)
#beta[H2]  ~ dnorm( 4, 20)
#delta[H2] ~ dnorm(-4, 20)
#for(i in (H2+1):I){
#    alpha[i] ~ dnorm( 0, 1)
#    beta[i]  ~ dnorm( 0, 1)
#    delta[i] ~ dnorm( 0, 1)
#    }
#}
#", file="model66Dyn2Di4.irt.txt")
##
##########################################################################################



### WAS NEEDED IN ALDF 
#for (n in 
#1:ncol(rc)){ 
#    rc[,n]<-as.numeric(rc[,n])
#}
#
### SORTS BY DATE
#tmp<-RCs[order(RCs$yr, RCs$mo, RCs$dy, RCs$folio),]  
#RCs<-tmp
#
## ## WILL BE NEEDED IN DYNAMIC VERSION, IF AT ALL
## trim <- votdat$mo
## trim[trim==1 | trim==2 | trim==3]<- 1
## trim[trim==4 | trim==5 | trim==6]<- 2
## trim[trim==7 | trim==8 | trim==9]<- 3
## trim[trim==10 | trim==11 | trim==12]<- 4
## #
## cuad <- votdat$mo
## cuad[cuad==1 | cuad==2 | cuad==3 | cuad==4]<- 1
## cuad[cuad==5 | cuad==6 | cuad==7 | cuad==8]<- 2
## cuad[cuad==9 | cuad==10 | cuad==11 | cuad==12]<- 3
## #
## titCuad <- cuad
## titCuad[cuad==1]<-"2006-3"
## titCuad[cuad==2]<-"2007-1"
## titCuad[cuad==3]<-"2007-2"
## titCuad[cuad==4]<-"2007-3"
## titCuad[cuad==5]<-"2008-1"
## titCuad[cuad==6]<-"2008-2"
## titCuad[cuad==7]<-"2008-3"
## titCuad[cuad==8]<-"2009-1"
## #
## sem <- votdat$mo
## sem[sem>0 & sem<7]<- 1
## sem[sem>6 & sem<13]<- 2
## #
## # unstar appropriate
## #tmp<-(votdat$yr-2006)*4 ## to work with trimestres 
## tmp<-(votdat$yr-2006)*3 ## to work with cuadrimestres 
## #tmp<-(votdat$yr-2006)*2 ## to work with semestres 
## #trim<-trim+tmp
## #trim<-trim-min(trim)+1
## cuad<-cuad+tmp
## cuad<-cuad-min(cuad)+1
## #sem<-sem+tmp
## #sem<-sem-min(sem)+1
## #
## #T<-max(trim)
## T<-max(cuad)
## #T<-max(sem)

## ONE-DIM ARRANGEMENT
## AGREEMENT MATRIX --- LA GUARDE PORQUE ESTO TARDA A?OS
load("agreeMatrix.Rdata")
#votes <- rc
#votes[votes==0] <- -1  # los 0s se vuelven -1s # DEJA ABSTENCION == NAY  
#I <- dim(votes)[1]; J <- dim(votes)[2]
#agreeMatrix <- matrix(NA, ncol=J, nrow=J); tmp <- rep(NA, times=I)
#for (j in 1:J){
#    agreeMatrix[j,j] <- 1  ## DIAGONAL
#              }
#for (j1 in 2:J){
#    for (j2 in (j1-1):1){
#        for (i in 1:I){
#            tmp[i] <- ifelse(votes[i,j1]==votes[i,j2], 1, 0)
#                      }
#        agreeMatrix[j2,j1] <- sum(tmp)/I; agreeMatrix[j1,j2] <- agreeMatrix[j2,j1]
#        print( paste("j1 =",j1,"; j2 =",j2) )
#                        }
#               } 
## SQUARED DISTANCES
sd <- (1-agreeMatrix)^2
## DOUBLE-CENTRED MATRIX
pmean <- rep(NA, times=J); mmat <- mean(sd); dc <- sd
for (j in 1:J){
    pmean[j] <- mean(sd[j,])
              }
for (r in 1:J){
    for (c in 1:J){
        dc[r,c] <- (sd[r,c] - pmean[r] - pmean[c] + mmat)/-2
                  }
              }
## SIMPLE ONE-DIM IDEAL POINTS
tmp <- sqrt(dc[1,1])
ip  <- c(tmp, dc[2:J,1]/tmp)
##
## EXTREMA DERECHA
thr <- .14
data.frame(ip=ip[c(1:J)[ip>thr]], id=dipdat$id[c(1:J)[ip>thr]], nom=dipdat$nom[c(1:J)[ip>thr]], part=dipdat$part[c(1:J)[ip>thr]], noVote=dipdat$noVoteRate[c(1:J)[ip>thr]])
##EXTREMA IZQUIERDA
thr <- -.185
data.frame(ip=ip[c(1:J)[ip<thr]], id=dipdat$id[c(1:J)[ip<thr]], nom=dipdat$nom[c(1:J)[ip<thr]], part=dipdat$part[c(1:J)[ip<thr]], noVote=dipdat$noVoteRate[c(1:J)[ip< thr]])
##
plot(c(-.3,.3), c(1,7), type="n")
for (j in 1:J){
    points(ip[j], dipdat$part[j], pch=20,col=dipdat$color[j])
    }




eric   ###################################################
###       Static 2Dimensions party anchors      ###
###################################################

## ANCHORS
#           Encinas                      Chepina                     Rojas 
#NW<-grep("dfrp12p", dipdat$id); NE<-grep("mexrp01p", dipdat$id); S<-grep("mexrp07p", dipdat$id)
#          AgsPan                      Noro?a                     Rojas 
#NE<-grep("ags01p", dipdat$id); SW<-grep("df19p", dipdat$id); S<-grep("mexrp07p", dipdat$id)
#tmp<-ifelse( (RCs$folio==2  & RCs$yr==2006 & RCs$mo==11 & RCs$dy==9 ),1,0 ); V1<-grep(1,tmp) # Vertical 8
#tmp<-ifelse( (RCs$folio==6  & RCs$yr==2006 & RCs$mo==12 & RCs$dy==28),1,0 ); V2<-grep(1,tmp) # Vertical 66
#tmp<-ifelse( (RCs$folio==10 & RCs$yr==2006 & RCs$mo==12 & RCs$dy==26),1,0 ); H1<-grep(1,tmp) # Horizontal 57

### ALLOWS TO DROP CASES FROM ANALYSIS
#year1 <- ifelse( (cuad==1 | cuad==2 | cuad==3), 1, 0 ) ## FALTA QUITAR A UN DIPUTADO (M?S?) QUE NO ENTR? HASTA DESPU?S
#drop <- ifelse(year1==0,1,0)
#tmp<-rc[drop==0,]
#rc<-tmp
#sem<-sem[drop==0]; cuad<-cuad[drop==0]; trim<-trim[drop==0]

## SORT BY PARTY
tmp <- 1:dim(rc)[1]
tmp <- ifelse ( dipdat$part=="pan", tmp, 
        ifelse (dipdat$part=="pri", tmp+1000, 
         ifelse ( dipdat$part=="prd", tmp+2000, 
          ifelse ( dipdat$part=="pt", tmp+3000, tmp+4000))))
## rcold <- rc; dipold <- dipdat
rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## PARTY INDICES
PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]); 
PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])

v <- rc
v <- t(v)
J <- nrow(v); I <- ncol(v)
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
dip.data <- list ("J", "I", "lo.v", "hi.v", "PAN", "PRI", "PRD", "PT")
dip.inits <- function (){
    list (
    v.star=vstar,
    delta=rnorm(I),
    angle=runif(I),
    b=rnorm(I),
    x=rnorm(J),
    y=rnorm(J)
    )
    }
dip.parameters <- c("delta","angle", "b", "x", "y")

#test ride to see program works
dip.60 <- bugs (dip.data, dip.inits, dip.parameters, 
#                "modelSta2Dj.irt.txt", n.chains=3, 
                "modelSta2Dj.txt", n.chains=3, 
                n.iter=10, n.thin=1, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

#longer run
dip.60 <- bugs (dip.data, dip.inits, dip.parameters, 
#                "modelSta2Dj.irt.txt", n.chains=3, 
                "modelSta2Dj.txt", n.chains=1, 
                n.iter=5000, n.thin=25, debug=F,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

plot(dip.60)
print(dip.60)

#to continue running
tmp1<-list (
    v.star=vstar,
    delta=dip.60$last.values[[1]]$delta,
    angle=dip.60$last.values[[1]]$angle,
    b=dip.60$last.values[[1]]$b,
    x=dip.60$last.values[[1]]$x,
    y=dip.60$last.values[[1]]$y
    )
tmp2<-list (
    v.star=vstar,
    delta=dip.60$last.values[[2]]$delta,
    angle=dip.60$last.values[[2]]$angle,
    b=dip.60$last.values[[2]]$b,
    x=dip.60$last.values[[2]]$x,
    y=dip.60$last.values[[2]]$y
    )
tmp3<-list (
    v.star=vstar,
    delta=dip.60$last.values[[3]]$delta,
    angle=dip.60$last.values[[3]]$angle,
    b=dip.60$last.values[[3]]$b,
    x=dip.60$last.values[[3]]$x,
    y=dip.60$last.values[[3]]$y
    )
### for (chain in 1:3){dip.60$last.values[[chain]]$v.star <- vstar}
dip.60.2 <- bugs (dip.data, 
                inits=list(tmp1,tmp2,tmp3), 
                dip.parameters, 
#                "modelSta2Dj.irt.txt", n.chains=3, 
                "modelSta2Dj.txt", n.chains=3, 
                n.iter=5000, n.thin=25, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

dip.60 <- dip.60.2
rm(dip.60.2)

plot(dip.60)
print(dip.60)

attach.bugs(dip.60)

jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (y[,j], 0.975, names=F)
    }

dipdat$color[dipdat$color=="."] <- "black"
plot(jotas[,2],jotas[,5], pch=19, col=dipdat$color)

bes <- matrix(NA, nrow=I, ncol=3)
for (i in 1:I){
    bes[i,1] <- quantile (b[,i], 0.025, names=F)
    bes[i,2] <- quantile (b[,i], 0.50, names=F)
    bes[i,3] <- quantile (b[,i], 0.975, names=F)
    }

a <- sin(angle) / sqrt(1-sin(angle)*sin(angle))
as <- matrix(NA, nrow=I, ncol=3)
for (i in 1:I){
    as[i,1] <- quantile (a[,i], 0.025, names=F)
    as[i,2] <- quantile (a[,i], 0.50, names=F)
    as[i,3] <- quantile (a[,i], 0.975, names=F)
    }
angles <- matrix(NA, nrow=I, ncol=3)
for (i in 1:I){
    angles[i,1] <- quantile (angle[,i], 0.025, names=F)
    angles[i,2] <- quantile (angle[,i], 0.50, names=F)
    angles[i,3] <- quantile (angle[,i], 0.975, names=F)
    }

subset <- c(7,41,121,160)
plot(jotas[,2],jotas[,5],pch=19,col=dipdat$color)
for (i in subset){
    abline(a=bes[i,2], b=as[i,2])}

a <- -beta / delta ## pendiente cutline
b <- alpha / delta ## constante cutline
## 45-DEGREE CLOCKWISE ROTATION OF COORDINATES (SO THAT PRIORS REMAIN n, s, e, w)
xR <- x*cos(pi/4) + y*sin(pi/4)
yR <- -x*sin(pi/4) + y*cos(pi/4)
x <- xR; y <- yR;
## 45-DEGREE CLOCKWISE ROTATION OF CUTLINES
xA <- -b/a; yA <- rep(0, length(a)); xO <- rep(0, length(a)); yO <- b  ## coords de Abscisa al origen y Ordenada al origan de c/cutline
xAR <- xA*cos(pi/4) + yA*sin(pi/4)
yAR <- -xA*sin(pi/4) + yA*cos(pi/4)
xOR <- xO*cos(pi/4) + yO*sin(pi/4)
yOR <- -xO*sin(pi/4) + yO*cos(pi/4)
X <- xAR; Y <- yAR; XX <- xOR; YY <- yOR ## simplifica notaci?n
aR <- (YY-Y)/(XX-X)           ## pendiente del cutline rotado
bR <- YY -((YY-Y)/(XX-X))*XX  ## constante del cutline rotado
rm(X,Y,XX,YY)
a <- aR; b <- bR


jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (y[,j], 0.975, names=F)
    }

# For use in rollrates analysis
postNorthprd <- rep(0,67)
postSouthprd <- rep(0,67)
for (j in 1:67){
    postNorthprd[j] <- ifelse (jotas[j,5]>=0 & part.67[j]=="prd", 1, 0) 
    postSouthprd[j] <- ifelse (jotas[j,5]<0 & part.67[j]=="prd", 1, 0) 
    }

amed <- rep(NA,times=I)
bmed <- rep(NA,times=I)
for (i in 1:I){
    amed[i] <- quantile (a[,i], 0.50, names=F)
    bmed[i] <- quantile (b[,i], 0.50, names=F)  }

### Exporta coordenadas de todos los diputados
tmp <- matrix(NA, nrow=67, ncol=4)
tmp[,1] <- as.numeric(jotas[,2])
tmp[,2] <- jotas[,5]
tmp[,3] <- names.67
tmp[,4] <- part.67
tmp<-data.matrix(tmp)
tmp[,1:2] <- as.numeric(tmp[,1:2])
write.table(tmp, file="aldfStaticIdPts.xls", sep=",")


### TO RESET GRAPH PARAMETERS SAY par(oldpar) ###
oldpar <- par(no.readonly=TRUE)

#par(mfrow=c(3,3))
#par("pin" = c(.63,.58)) #width and height of plot region in inches

## FUNCTION TO DRAW ELLIPSES OVOIDS
ellipsePoints <- function(a,b, alpha = 0, loc = c(0,0), n = 201)
{
    ## Purpose: ellipse points,radially equispaced, given geometric par.s
    ## -------------------------------------------------------------------------
    ## Arguments: a, b : length of half axes in (x,y) direction
    ##            alpha: angle (in degrees) for rotation
    ##            loc  : center of ellipse
    ##            n    : number of points
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 19 Mar 2002, 16:26
    B <- min(a,b)
    A <- max(a,b)
    ## B <= A
    d2 <- (A-B)*(A+B)                   #= A^2 - B^2
    phi <- 2*pi*seq(0,1, len = n)
    sp <- sin(phi)
    cp <- cos(phi)
    r <- a*b / sqrt(B^2 + d2 * sp^2)
    xy <- r * cbind(cp, sp)
    ## xy are the ellipse points for alpha = 0 and loc = (0,0)
    al <- alpha * pi/180
    ca <- cos(al)
    sa <- sin(al)
    xy %*% rbind(c(ca, sa), c(-sa, ca)) + cbind(rep(loc[1],n),
                                                rep(loc[2],n))
}

tmp <- c(jotas[,1],jotas[,4])
for (i in 1:length(tmp)) { tmp[i] <- ifelse(is.na(tmp[i])==TRUE,0,tmp[i]) }
min <- min( tmp )
tmp <- c(jotas[,3],jotas[,6])
for (i in 1:length(tmp)) { tmp[i] <- ifelse(is.na(tmp[i])==TRUE,0,tmp[i]) }
max <- max( tmp )
lims <- c(NA,NA)
lims[1] <- ifelse(abs(min)>max, min, -max)
lims[2] <- ifelse(abs(min)>max, abs(min), max)

#### FOR USE IF ELLIPSES WILL BE GRAPHED
##eps <- array(NA, dim=c(201,2,67))
##eps[,,1] <- ellipsePoints(a=jotas[t,2,1]-jotas[t,1,1],b=jotas[t,5,1]-jotas[t,4,1],alpha=0,loc=c(jotas[t,2,1],jotas[t,5,1]),n=201)
##eps[,,2] <- ellipsePoints(a=jotas[t,2,2]-jotas[t,1,2],b=jotas[t,5,2]-jotas[t,4,2],alpha=0,loc=c(jotas[t,2,2],jotas[t,5,2]),n=201)
##etcetera

par(mar = c(3.1, 3.1, 2.1, 2.1) )
plot(c(-1.5,1.5),c(-1.5,1.5),type="n",
       xlab=c(""), ##xlab=c("pro-SQ                                         pro-change"),
       ylab=c(""), ##ylab=c("interpretivist                                            literalist"),
       main=c("")) ##main=paste("Acc+Con (ancla j) time=",t,I,"obs"))
abline(-1.5,0,col="grey",lty=3); abline(-1,0,col="grey",lty=3); abline(-.5,0,col="grey",lty=3);
       abline(0,0,col="grey",lty=3); abline(.5,0,col="grey",lty=3); abline(1,0,col="grey",lty=3);
       abline(1.5,0,col="grey",lty=3);
abline(v=-1.5,col="grey",lty=3); abline(v=-1,col="grey",lty=3); abline(v=-.5,col="grey",lty=3);
       abline(v=0,col="grey",lty=3); abline(v=.5,col="grey",lty=3); abline(v=1,col="grey",lty=3);
       abline(v=1.5,col="grey",lty=3);
legend(1.1,-.65, legend=part.list, cex=.75, pch=20, pt.cex=1.25, col=color.list, bg="white")
##for (j in 1:J){
##    segments(jotas[t,1,j],jotas[t,5,j],jotas[t,3,j],jotas[t,5,j],col="gray")
##    segments(jotas[t,2,j],jotas[t,4,j],jotas[t,2,j],jotas[t,6,j],col="gray")
##    }
##for (j in 1:J){
##    lines(eps[,,j],col=color.67[j])
##    }
for (j in 1:J){
    points(jotas[j,2],jotas[j,5],pch=20,col=color.67[j])
    }
for (j in 1:J){
    points(jotas[j,2],jotas[j,5], col=dCoord[j]); ## pone coordinadores
    }
##for (j in 1:J){
##    text(jotas[j,2],jotas[j,5],labels=coords[j])
##    }


## cutlines
#cuad <- 1*d1+2*d2+3*d3+4*d4+5*d5+6*d6+7*d7+8*d8 ## could be handy to draw cutlines for some t in static map
    plot(c(-1.5,1.5),c(-1.5,1.5),type="n",
           xlab=c(""), 
           ylab=c(""), 
           main=c("")) 
#    atmp <- amed[cuad==t]; btmp <- bmed[cuad==t];
    atmp <- amed; btmp <- bmed;
    N <- length(atmp)
    for (n in 1:N){
        abline(a=btmp[n], b=atmp[n], col="grey") } ## OJO: a en mi modelo es slope, en R es constant
    for (j in 1:J){
        points(jotas[j,2],jotas[j,5],pch=20,col=color.67[j])
        }

## cutlines one-by-one
setwd("d:/01/data/rollcall/aldf/graphs/cutlinesOnebyOne")
    atmp <- amed; btmp <- bmed;
    N <- length(atmp)
#    n <- 12
    for (n in 1:N){
        plot(c(-2,2),c(-2,2),type="n",
           xlab=c(""), 
           ylab=c(""), 
           main=paste(RCs$yr[n],"-",RCs$mo[n],"-",RCs$dy[n],"#",RCs$folio[n],"  (",RCs$favor[n],"/",RCs$contra[n],"/",RCs$absten[n],")",sep="")) 
        abline(a=btmp[n], b=atmp[n], col="black") 
            for (j in 1:J){
                points(jotas[j,2],jotas[j,5],pch=20,col=color.67[j])
                }
        savePlot(filename = paste("cutline",n, sep=""), type = "pdf")
    }
setwd("d:/01/data/rollcall/aldf")

#######################################################
#######################################################
###      Static 1Dimension two extremist anchors    ###
#######################################################
#######################################################

## ANCHORS
##          AgsPan                      Noro?a
#DER<-grep("ags01p", dipdat$id); IZQ<-grep("df19p", dipdat$id)
##          Luken                      C?rdenas
DER<-grep("bc05p", dipdat$id); IZQ<-grep("df04p", dipdat$id)

### ALLOWS TO DROP CASES FROM ANALYSIS
#year1 <- ifelse( (cuad==1 | cuad==2 | cuad==3), 1, 0 ) ## FALTA QUITAR A UN DIPUTADO (M?S?) QUE NO ENTR? HASTA DESPU?S
#drop <- ifelse(year1==0,1,0)
#tmp<-rc[drop==0,]
#rc<-tmp
#sem<-sem[drop==0]; cuad<-cuad[drop==0]; trim<-trim[drop==0]

votes <- rc
## TAKE SAMPLE HERE IF SO WISHED
rnd <- runif(dim(rc)[1]); votes <- votes[rnd<.3,]
votes <- t(votes)
J <- nrow(votes); I <- ncol(votes)
v <- votes
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -50)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 50)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
dip.data <- list ("J", "I", "lo.v", "hi.v", "DER", "IZQ")
dip.inits <- function (){
    list (
    v.star=vstar,
    x=rnorm(J),
    delta=rnorm(I),
    n=rnorm(I)
    )
    }
dip.parameters <- c("delta","n", "x")

#test ride to see program works
dip.60 <- bugs (dip.data, dip.inits, dip.parameters, 
                "modelSta1Dj.txt", n.chains=3, 
                n.iter=10, n.thin=1, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

#longer run
dip.60 <- bugs (dip.data, dip.inits, dip.parameters, 
                "modelSta1Dj.txt", n.chains=3, 
                n.iter=1000, n.thin=10, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

plot(dip.60)
print(dip.60)

#to continue running
tmp1<-list (
    v.star=vstar,
    x=dip.60$last.values[[1]]$x,
    delta=dip.60$last.values[[1]]$delta,
    n=dip.60$last.values[[1]]$n
    )
tmp2<-list (
    v.star=vstar,
    x=dip.60$last.values[[2]]$x,
    delta=dip.60$last.values[[2]]$delta,
    n=dip.60$last.values[[2]]$n
    )
tmp3<-list (
    v.star=vstar,
    x=dip.60$last.values[[3]]$x,
    delta=dip.60$last.values[[3]]$delta,
    n=dip.60$last.values[[3]]$n
    )
### for (chain in 1:3){dip.60$last.values[[chain]]$v.star <- vstar}
dip.60.2 <- bugs (dip.data, 
                inits=list(tmp1,tmp2,tmp3), 
                dip.parameters, 
                "modelSta1Dj.txt", n.chains=3, 
                n.iter=1000, n.thin=10, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

dip.60 <- dip.60.2; rm(dip.60.2)

attach.bugs(dip.60)
ip <- abs(x)

tmp <- rep(NA, times=J)
ips <- data.frame(p025=tmp, p50=tmp, p975=tmp, nom=dipdat$nom, part=dipdat$part, id=dipdat$id)
for (j in 1:J){
    ips[j,1] <- quantile (ip[,j], 0.025, names=F)
    ips[j,2] <- quantile (ip[,j], 0.50, names=F)
    ips[j,3] <- quantile (ip[,j], 0.975, names=F)
              }
ips <- ips[,c(-1,-3)]
ips[order(ips$p50),]


###############################################################
### all periods 66 members 2Dimensions DYNAMIC item anchors ###
###############################################################

votes <- RCs[,9:ncol(RCs)]
#votes[votes==-1] <- 0  # los -1s se vuelven 0s # DEJA ABSTENCION COMO VOTO NAY  
votes <- t(votes)
J <- nrow(votes); I <- ncol(votes)
d1 <- ifelse(cuad==1,1,0) ## 
d2 <- ifelse(cuad==2,1,0) ## 
d3 <- ifelse(cuad==3,1,0) ## 
d4 <- ifelse(cuad==4,1,0) ## 
d5 <- ifelse(cuad==5,1,0) ## 
d6 <- ifelse(cuad==6,1,0) ## 
d7 <- ifelse(cuad==7,1,0) ## 
d8 <- ifelse(cuad==8,1,0) ## 
v <- votes
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
aldf.data <- list ("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "J", "I", "lo.v", "hi.v", "V1", "H1", "V2", "H2", "N")
##zero1<-rnorm(J); zero1[9]<-rnorm(1,-2); zero1[52]<-rnorm(1,2)  ## toma en cuenta priors j
##zero2<-rnorm(J); zero2[35]<-rnorm(1,2); zero2[64]<-rnorm(1,-2) 
aldf.inits <- function (){
    list (
    v.star=vstar,
    alpha=rnorm(I),
    beta=rnorm(I),
    delta=rnorm(I),
#    xZero=zero1,
#    yZero=zero2,
    xOne=rnorm(J),
    yOne=rnorm(J),
    xTwo=rnorm(J),
    yTwo=rnorm(J),
    xThree=rnorm(J),
    yThree=rnorm(J),
    xFour=rnorm(J),
    yFour=rnorm(J),
    xFive=rnorm(J),
    yFive=rnorm(J),
    xSix=rnorm(J),
    ySix=rnorm(J),
    xSeven=rnorm(J),
    ySeven=rnorm(J),
    xEight=rnorm(J),
    yEight=rnorm(J)
    )
    }
aldf.parameters <- c("delta", "xOne", "xTwo", "xThree", "xFour", "xFive", "xSix", "xSeven", "xEight", 
                              "yOne", "yTwo", "yThree", "yFour", "yFive", "ySix", "ySeven", "yEight", 
                              "alpha", "beta", "delta")

#test ride to see program works
aldf.66 <- bugs (aldf.data, aldf.inits, aldf.parameters, 
                "model66Dyn2Di4.irt.txt", n.chains=3, 
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
#                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

plot(aldf.66)
print(aldf.66)

#longer run
aldf.66 <- bugs (aldf.data, aldf.inits, aldf.parameters, 
                "model66Dyn2Di4.irt.txt", n.chains=3, 
                n.iter=10000, n.thin=20, debug=T,
                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
#                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

#to continue running
tmp1<-list (
    v.star=vstar,
    alpha=aldf.66$last.values[[1]]$alpha,
    beta=aldf.66$last.values[[1]]$beta,
    delta=aldf.66$last.values[[1]]$delta,
#    xZero=aldf.66$last.values[[1]]$xZero,
#    yZero=aldf.66$last.values[[1]]$yZero,
    xOne=aldf.66$last.values[[1]]$xOne,
    yOne=aldf.66$last.values[[1]]$yOne,
    xTwo=aldf.66$last.values[[1]]$xTwo,
    yTwo=aldf.66$last.values[[1]]$yTwo,
    xThree=aldf.66$last.values[[1]]$xThree,
    yThree=aldf.66$last.values[[1]]$yThree,
    xFour=aldf.66$last.values[[1]]$xFour,
    yFour=aldf.66$last.values[[1]]$yFour,
    xFive=aldf.66$last.values[[1]]$xFive,
    yFive=aldf.66$last.values[[1]]$yFive,
    xSix=aldf.66$last.values[[1]]$xSix,
    ySix=aldf.66$last.values[[1]]$ySix,
    xSeven=aldf.66$last.values[[1]]$xSeven,
    ySeven=aldf.66$last.values[[1]]$ySeven,
    xEight=aldf.66$last.values[[1]]$xEight,
    yEight=aldf.66$last.values[[1]]$yEight
    )
tmp2<-list (
    v.star=vstar,
    alpha=aldf.66$last.values[[2]]$alpha,
    beta=aldf.66$last.values[[2]]$beta,
    delta=aldf.66$last.values[[2]]$delta,
#    xZero=aldf.66$last.values[[2]]$xZero,
#    yZero=aldf.66$last.values[[2]]$yZero,
    xOne=aldf.66$last.values[[2]]$xOne,
    yOne=aldf.66$last.values[[2]]$yOne,
    xTwo=aldf.66$last.values[[2]]$xTwo,
    yTwo=aldf.66$last.values[[2]]$yTwo,
    xThree=aldf.66$last.values[[2]]$xThree,
    yThree=aldf.66$last.values[[2]]$yThree,
    xFour=aldf.66$last.values[[2]]$xFour,
    yFour=aldf.66$last.values[[2]]$yFour,
    xFive=aldf.66$last.values[[2]]$xFive,
    yFive=aldf.66$last.values[[2]]$yFive,
    xSix=aldf.66$last.values[[2]]$xSix,
    ySix=aldf.66$last.values[[2]]$ySix,
    xSeven=aldf.66$last.values[[2]]$xSeven,
    ySeven=aldf.66$last.values[[2]]$ySeven,
    xEight=aldf.66$last.values[[2]]$xEight,
    yEight=aldf.66$last.values[[2]]$yEight
    )
tmp3<-list (
    v.star=vstar,
    alpha=aldf.66$last.values[[3]]$alpha,
    beta=aldf.66$last.values[[3]]$beta,
    delta=aldf.66$last.values[[3]]$delta,
#    xZero=aldf.66$last.values[[3]]$xZero,
#    yZero=aldf.66$last.values[[3]]$yZero,
    xOne=aldf.66$last.values[[3]]$xOne,
    yOne=aldf.66$last.values[[3]]$yOne,
    xTwo=aldf.66$last.values[[3]]$xTwo,
    yTwo=aldf.66$last.values[[3]]$yTwo,
    xThree=aldf.66$last.values[[3]]$xThree,
    yThree=aldf.66$last.values[[3]]$yThree,
    xFour=aldf.66$last.values[[3]]$xFour,
    yFour=aldf.66$last.values[[3]]$yFour,
    xFive=aldf.66$last.values[[3]]$xFive,
    yFive=aldf.66$last.values[[3]]$yFive,
    xSix=aldf.66$last.values[[3]]$xSix,
    ySix=aldf.66$last.values[[3]]$ySix,
    xSeven=aldf.66$last.values[[3]]$xSeven,
    ySeven=aldf.66$last.values[[3]]$ySeven,
    xEight=aldf.66$last.values[[3]]$xEight,
    yEight=aldf.66$last.values[[3]]$yEight
    )
### for (chain in 1:3){aldf.66$last.values[[chain]]$v.star <- vstar}
aldf.66.2 <- bugs (aldf.data, 
                inits=list(tmp1,tmp2,tmp3), 
                aldf.parameters, 
                "model66Dyn2Di4.irt.txt", n.chains=3, 
                n.iter=10000, n.thin=20, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

aldf.66 <- aldf.66.2
rm(aldf.66.2)

plot(aldf.66)
print(aldf.66)

attach.bugs(aldf.66)
a <- -beta / delta ## pendiente cutline
b <- alpha / delta ## constante cutline
## 45-DEGREE CLOCKWISE ROTATION OF COORDINATES (SO THAT PRIORS REMAIN n, s, e, w)
xOneR <- xOne*cos(pi/4) + yOne*sin(pi/4)
yOneR <- -xOne*sin(pi/4) + yOne*cos(pi/4)
xTwoR <- xTwo*cos(pi/4) + yTwo*sin(pi/4)
yTwoR <- -xTwo*sin(pi/4) + yTwo*cos(pi/4)
xThreeR <- xThree*cos(pi/4) + yThree*sin(pi/4)
yThreeR <- -xThree*sin(pi/4) + yThree*cos(pi/4)
xFourR <- xFour*cos(pi/4) + yFour*sin(pi/4)
yFourR <- -xFour*sin(pi/4) + yFour*cos(pi/4)
xFiveR <- xFive*cos(pi/4) + yFive*sin(pi/4)
yFiveR <- -xFive*sin(pi/4) + yFive*cos(pi/4)
xSixR <- xSix*cos(pi/4) + ySix*sin(pi/4)
ySixR <- -xSix*sin(pi/4) + ySix*cos(pi/4)
xSevenR <- xSeven*cos(pi/4) + ySeven*sin(pi/4)
ySevenR <- -xSeven*sin(pi/4) + ySeven*cos(pi/4)
xEightR <- xEight*cos(pi/4) + yEight*sin(pi/4)
yEightR <- -xEight*sin(pi/4) + yEight*cos(pi/4)
xOne <- xOneR; yOne <- yOneR; xTwo <- xTwoR; yTwo <- yTwoR; xThree <- xThreeR; yThree <- yThreeR; 
xFour <- xFourR; yFour <- yFourR; xFive <- xFiveR; yFive <- yFiveR; xSix <- xSixR; ySix <- ySixR; 
xSeven <- xSevenR; ySeven <- ySevenR; xEight <- xEightR; yEight <- yEightR; 
## 45-DEGREE CLOCKWISE ROTATION OF CUTLINES
xA <- -b/a; yA <- rep(0, length(a)); xO <- rep(0, length(a)); yO <- b  ## coords de Abscisa al origen y Ordenada al origan de c/cutline
xAR <- xA*cos(pi/4) + yA*sin(pi/4)
yAR <- -xA*sin(pi/4) + yA*cos(pi/4)
xOR <- xO*cos(pi/4) + yO*sin(pi/4)
yOR <- -xO*sin(pi/4) + yO*cos(pi/4)
X <- xAR; Y <- yAR; XX <- xOR; YY <- yOR ## simplifica notaci?n
aR <- (YY-Y)/(XX-X)           ## pendiente del cutline rotado
bR <- YY -((YY-Y)/(XX-X))*XX  ## constante del cutline rotado
rm(X,Y,XX,YY)
a <- aR; b <- bR

jotas <- array(NA, dim=c(T,6,J))
for (j in 1:J){
    jotas[1,1,j] <- quantile (xOne[,j], 0.025, names=F)
    jotas[1,2,j] <- quantile (xOne[,j], 0.50, names=F)
    jotas[1,3,j] <- quantile (xOne[,j], 0.975, names=F)
    jotas[2,1,j] <- quantile (xTwo[,j], 0.025, names=F)
    jotas[2,2,j] <- quantile (xTwo[,j], 0.50, names=F)
    jotas[2,3,j] <- quantile (xTwo[,j], 0.975, names=F)
    jotas[3,1,j] <- quantile (xThree[,j], 0.025, names=F)
    jotas[3,2,j] <- quantile (xThree[,j], 0.50, names=F)
    jotas[3,3,j] <- quantile (xThree[,j], 0.975, names=F)
    jotas[4,1,j] <- quantile (xFour[,j], 0.025, names=F)
    jotas[4,2,j] <- quantile (xFour[,j], 0.50, names=F)
    jotas[4,3,j] <- quantile (xFour[,j], 0.975, names=F)
    jotas[5,1,j] <- quantile (xFive[,j], 0.025, names=F)
    jotas[5,2,j] <- quantile (xFive[,j], 0.50, names=F)
    jotas[5,3,j] <- quantile (xFive[,j], 0.975, names=F)
    jotas[6,1,j] <- quantile (xSix[,j], 0.025, names=F)
    jotas[6,2,j] <- quantile (xSix[,j], 0.50, names=F)
    jotas[6,3,j] <- quantile (xSix[,j], 0.975, names=F)
    jotas[7,1,j] <- quantile (xSeven[,j], 0.025, names=F)
    jotas[7,2,j] <- quantile (xSeven[,j], 0.50, names=F)
    jotas[7,3,j] <- quantile (xSeven[,j], 0.975, names=F)
    jotas[8,1,j] <- quantile (xEight[,j], 0.025, names=F)
    jotas[8,2,j] <- quantile (xEight[,j], 0.50, names=F)
    jotas[8,3,j] <- quantile (xEight[,j], 0.975, names=F)
    jotas[1,4,j] <- quantile (yOne[,j], 0.025, names=F)
    jotas[1,5,j] <- quantile (yOne[,j], 0.50, names=F)
    jotas[1,6,j] <- quantile (yOne[,j], 0.975, names=F)
    jotas[2,4,j] <- quantile (yTwo[,j], 0.025, names=F)
    jotas[2,5,j] <- quantile (yTwo[,j], 0.50, names=F)
    jotas[2,6,j] <- quantile (yTwo[,j], 0.975, names=F)
    jotas[3,4,j] <- quantile (yThree[,j], 0.025, names=F)
    jotas[3,5,j] <- quantile (yThree[,j], 0.50, names=F)
    jotas[3,6,j] <- quantile (yThree[,j], 0.975, names=F)
    jotas[4,4,j] <- quantile (yFour[,j], 0.025, names=F)
    jotas[4,5,j] <- quantile (yFour[,j], 0.50, names=F)
    jotas[4,6,j] <- quantile (yFour[,j], 0.975, names=F)
    jotas[5,4,j] <- quantile (yFive[,j], 0.025, names=F)
    jotas[5,5,j] <- quantile (yFive[,j], 0.50, names=F)
    jotas[5,6,j] <- quantile (yFive[,j], 0.975, names=F)
    jotas[6,4,j] <- quantile (ySix[,j], 0.025, names=F)
    jotas[6,5,j] <- quantile (ySix[,j], 0.50, names=F)
    jotas[6,6,j] <- quantile (ySix[,j], 0.975, names=F)
    jotas[7,4,j] <- quantile (ySeven[,j], 0.025, names=F)
    jotas[7,5,j] <- quantile (ySeven[,j], 0.50, names=F)
    jotas[7,6,j] <- quantile (ySeven[,j], 0.975, names=F)
    jotas[8,4,j] <- quantile (yEight[,j], 0.025, names=F)
    jotas[8,5,j] <- quantile (yEight[,j], 0.50, names=F)
    jotas[8,6,j] <- quantile (yEight[,j], 0.975, names=F)
    }
###Quita retiros y sustituciones
for (t in 8) jotas[t,,65] <- rep(NA,6) ## Jorge D?az Cuervo pide licencia 23/9/2008=inicio cuad 7
for (t in 1:6) jotas[t,,66] <- rep(NA,6) ## Carla S?nchez Armas, la suplente

amed <- rep(NA,times=I)
bmed <- rep(NA,times=I)
for (i in 1:I){
    amed[i] <- quantile (a[,i], 0.50, names=F)
    bmed[i] <- quantile (b[,i], 0.50, names=F)  }

### Exporta coordenadas de todos los diputados
tmp <- matrix(NA, nrow=67, ncol=18)
tmp[,17] <- names.67
tmp[,18] <- part.67
for (j in 1:67){
    for (t in 1:8){
        tmp[j,t] <- jotas[t,2,j]
        tmp[j,t+8] <- jotas[t,5,j]
        }}
tmp<-data.matrix(tmp)
for (n in 1:16){tmp[,n] <- as.numeric(tmp[,n])}
write.table(tmp, file="aldfIdPts.xls", sep=",")
### Exporta
tmp <- data.frame(cbind(yr=RCs$yr, mo=RCs$mo, dy=RCs$dy ,a=amed, b=bmed))
write.table(tmp, file="aldfCutlines.xls", sep=",")

### TO RESET GRAPH PARAMETERS SAY par(oldpar) ###
oldpar <- par(no.readonly=TRUE)

#par(mfrow=c(3,3))
#par("pin" = c(.63,.58)) #width and height of plot region in inches

## FUNCTION TO DRAW ELLIPSES OVOIDS
ellipsePoints <- function(a,b, alpha = 0, loc = c(0,0), n = 201)
{
    ## Purpose: ellipse points,radially equispaced, given geometric par.s
    ## -------------------------------------------------------------------------
    ## Arguments: a, b : length of half axes in (x,y) direction
    ##            alpha: angle (in degrees) for rotation
    ##            loc  : center of ellipse
    ##            n    : number of points
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 19 Mar 2002, 16:26
    B <- min(a,b)
    A <- max(a,b)
    ## B <= A
    d2 <- (A-B)*(A+B)                   #= A^2 - B^2
    phi <- 2*pi*seq(0,1, len = n)
    sp <- sin(phi)
    cp <- cos(phi)
    r <- a*b / sqrt(B^2 + d2 * sp^2)
    xy <- r * cbind(cp, sp)
    ## xy are the ellipse points for alpha = 0 and loc = (0,0)
    al <- alpha * pi/180
    ca <- cos(al)
    sa <- sin(al)
    xy %*% rbind(c(ca, sa), c(-sa, ca)) + cbind(rep(loc[1],n),
                                                rep(loc[2],n))
}


tmp <- c(jotas[,1,1],jotas[,4,1],jotas[,1,2],jotas[,4,2],jotas[,1,3],jotas[,4,3],jotas[,1,4],jotas[,4,4],
        jotas[,1,5],jotas[,4,5],jotas[,1,6],jotas[,4,6],jotas[,1,7],jotas[,4,7],jotas[,1,8],jotas[,4,8],
        jotas[,1,9],jotas[,4,9],jotas[,1,10],jotas[,4,10],jotas[,1,11],jotas[,4,11],jotas[,1,12],jotas[,4,12],
        jotas[,1,13],jotas[,4,13],jotas[,1,14],jotas[,4,14],jotas[,1,15],jotas[,4,15],jotas[,1,16],jotas[,4,16],
        jotas[,1,17],jotas[,4,17],jotas[,1,18],jotas[,4,18],jotas[,1,19],jotas[,4,19],jotas[,1,20],jotas[,4,20],
        jotas[,1,21],jotas[,4,21],jotas[,1,22],jotas[,4,22],jotas[,1,23],jotas[,4,23],jotas[,1,24],jotas[,4,24],
        jotas[,1,25],jotas[,4,25],jotas[,1,26],jotas[,4,26],jotas[,1,27],jotas[,4,27],jotas[,1,28],jotas[,4,28],
        jotas[,1,29],jotas[,4,29],jotas[,1,30],jotas[,4,30],jotas[,1,31],jotas[,4,31],jotas[,1,32],jotas[,4,32],
        jotas[,1,33],jotas[,4,33],jotas[,1,34],jotas[,4,34],jotas[,1,35],jotas[,4,35],jotas[,1,36],jotas[,4,36],
        jotas[,1,37],jotas[,4,37],jotas[,1,38],jotas[,4,38],jotas[,1,39],jotas[,4,39],jotas[,1,40],jotas[,4,40],
        jotas[,1,41],jotas[,4,41],jotas[,1,42],jotas[,4,42],jotas[,1,43],jotas[,4,43],jotas[,1,44],jotas[,4,44],
        jotas[,1,45],jotas[,4,45],jotas[,1,46],jotas[,4,46],jotas[,1,47],jotas[,4,47],jotas[,1,48],jotas[,4,48],
        jotas[,1,49],jotas[,4,49],jotas[,1,50],jotas[,4,50],jotas[,1,51],jotas[,4,51],jotas[,1,52],jotas[,4,52],
        jotas[,1,53],jotas[,4,53],jotas[,1,54],jotas[,4,54],jotas[,1,55],jotas[,4,55],jotas[,1,56],jotas[,4,56],
        jotas[,1,57],jotas[,4,57],jotas[,1,58],jotas[,4,58],jotas[,1,59],jotas[,4,59],jotas[,1,60],jotas[,4,60],
        jotas[,1,61],jotas[,4,61],jotas[,1,62],jotas[,4,62],jotas[,1,63],jotas[,4,63],jotas[,1,64],jotas[,4,64],
        jotas[,1,65],jotas[,4,65],jotas[,1,66],jotas[,4,66],jotas[,1,67],jotas[,4,67])
for (i in 1:length(tmp)) { tmp[i] <- ifelse(is.na(tmp[i])==TRUE,0,tmp[i]) }
min <- min( tmp )
tmp <- c(jotas[,3,1],jotas[,6,1],jotas[,3,2],jotas[,6,2],jotas[,3,3],jotas[,6,3],jotas[,3,4],jotas[,6,4],
        jotas[,3,5],jotas[,6,5],jotas[,3,6],jotas[,6,6],jotas[,3,7],jotas[,6,7],jotas[,3,8],jotas[,6,8],
        jotas[,3,9],jotas[,6,9],jotas[,3,10],jotas[,6,10],jotas[,3,11],jotas[,6,11],jotas[,3,12],jotas[,6,12],
        jotas[,3,13],jotas[,6,13],jotas[,3,14],jotas[,6,14],jotas[,3,15],jotas[,6,15],jotas[,3,16],jotas[,6,16],
        jotas[,3,17],jotas[,6,17],jotas[,3,18],jotas[,6,18],jotas[,3,19],jotas[,6,19],jotas[,3,20],jotas[,6,20],
        jotas[,3,21],jotas[,6,21],jotas[,3,22],jotas[,6,22],jotas[,3,23],jotas[,6,23],jotas[,3,24],jotas[,6,24],
        jotas[,3,25],jotas[,6,25],jotas[,3,26],jotas[,6,26],jotas[,3,27],jotas[,6,27],jotas[,3,28],jotas[,6,28],
        jotas[,3,29],jotas[,6,29],jotas[,3,30],jotas[,6,30],jotas[,3,31],jotas[,6,31],jotas[,3,32],jotas[,6,32],
        jotas[,3,33],jotas[,6,33],jotas[,3,34],jotas[,6,34],jotas[,3,35],jotas[,6,35],jotas[,3,36],jotas[,6,36],
        jotas[,3,37],jotas[,6,37],jotas[,3,38],jotas[,6,38],jotas[,3,39],jotas[,6,39],jotas[,3,40],jotas[,6,40],
        jotas[,3,41],jotas[,6,41],jotas[,3,42],jotas[,6,42],jotas[,3,43],jotas[,6,43],jotas[,3,44],jotas[,6,44],
        jotas[,3,45],jotas[,6,45],jotas[,3,46],jotas[,6,46],jotas[,3,47],jotas[,6,47],jotas[,3,48],jotas[,6,48],
        jotas[,3,49],jotas[,6,49],jotas[,3,50],jotas[,6,50],jotas[,3,51],jotas[,6,51],jotas[,3,52],jotas[,6,52],
        jotas[,3,53],jotas[,6,53],jotas[,3,54],jotas[,6,54],jotas[,3,55],jotas[,6,55],jotas[,3,56],jotas[,6,56],
        jotas[,3,57],jotas[,6,57],jotas[,3,58],jotas[,6,58],jotas[,3,59],jotas[,6,59],jotas[,3,60],jotas[,6,60],
        jotas[,3,61],jotas[,6,61],jotas[,3,62],jotas[,6,62],jotas[,3,63],jotas[,6,63],jotas[,3,64],jotas[,6,64],
        jotas[,3,65],jotas[,6,65],jotas[,3,66],jotas[,6,66],jotas[,3,67],jotas[,6,67])
for (i in 1:length(tmp)) { tmp[i] <- ifelse(is.na(tmp[i])==TRUE,0,tmp[i]) }
max <- max( tmp )
lims <- c(NA,NA)
lims[1] <- ifelse(abs(min)>max, min, -max) 
lims[2] <- ifelse(abs(min)>max, abs(min), max) 

#### FOR USE IF ELLIPSES WILL BE GRAPHED
##eps <- array(NA, dim=c(201,2,67))
##eps[,,1] <- ellipsePoints(a=jotas[t,2,1]-jotas[t,1,1],b=jotas[t,5,1]-jotas[t,4,1],alpha=0,loc=c(jotas[t,2,1],jotas[t,5,1]),n=201)
##eps[,,2] <- ellipsePoints(a=jotas[t,2,2]-jotas[t,1,2],b=jotas[t,5,2]-jotas[t,4,2],alpha=0,loc=c(jotas[t,2,2],jotas[t,5,2]),n=201)
##eps[,,3] <- ellipsePoints(a=jotas[t,2,3]-jotas[t,1,3],b=jotas[t,5,3]-jotas[t,4,3],alpha=0,loc=c(jotas[t,2,3],jotas[t,5,3]),n=201)
##eps[,,4] <- ellipsePoints(a=jotas[t,2,4]-jotas[t,1,4],b=jotas[t,5,4]-jotas[t,4,4],alpha=0,loc=c(jotas[t,2,4],jotas[t,5,4]),n=201)
##eps[,,5] <- ellipsePoints(a=jotas[t,2,5]-jotas[t,1,5],b=jotas[t,5,5]-jotas[t,4,5],alpha=0,loc=c(jotas[t,2,5],jotas[t,5,5]),n=201)
##eps[,,6] <- ellipsePoints(a=jotas[t,2,6]-jotas[t,1,6],b=jotas[t,5,6]-jotas[t,4,6],alpha=0,loc=c(jotas[t,2,6],jotas[t,5,6]),n=201)
##eps[,,7] <- ellipsePoints(a=jotas[t,2,7]-jotas[t,1,7],b=jotas[t,5,7]-jotas[t,4,7],alpha=0,loc=c(jotas[t,2,7],jotas[t,5,7]),n=201)
##eps[,,8] <- ellipsePoints(a=jotas[t,2,8]-jotas[t,1,8],b=jotas[t,5,8]-jotas[t,4,8],alpha=0,loc=c(jotas[t,2,8],jotas[t,5,8]),n=201)
##eps[,,9] <- ellipsePoints(a=jotas[t,2,9]-jotas[t,1,9],b=jotas[t,5,9]-jotas[t,4,9],alpha=0,loc=c(jotas[t,2,9],jotas[t,5,9]),n=201)
##eps[,,10] <- ellipsePoints(a=jotas[t,2,10]-jotas[t,1,10],b=jotas[t,5,10]-jotas[t,4,10],alpha=0,loc=c(jotas[t,2,10],jotas[t,5,10]),n=201)
##eps[,,11] <- ellipsePoints(a=jotas[t,2,11]-jotas[t,1,11],b=jotas[t,5,11]-jotas[t,4,11],alpha=0,loc=c(jotas[t,2,11],jotas[t,5,11]),n=201)
##eps[,,12] <- ellipsePoints(a=jotas[t,2,12]-jotas[t,1,12],b=jotas[t,5,12]-jotas[t,4,12],alpha=0,loc=c(jotas[t,2,12],jotas[t,5,12]),n=201)
##eps[,,13] <- ellipsePoints(a=jotas[t,2,13]-jotas[t,1,13],b=jotas[t,5,13]-jotas[t,4,13],alpha=0,loc=c(jotas[t,2,13],jotas[t,5,13]),n=201)
##eps[,,14] <- ellipsePoints(a=jotas[t,2,14]-jotas[t,1,14],b=jotas[t,5,14]-jotas[t,4,14],alpha=0,loc=c(jotas[t,2,14],jotas[t,5,14]),n=201)
##eps[,,15] <- ellipsePoints(a=jotas[t,2,15]-jotas[t,1,15],b=jotas[t,5,15]-jotas[t,4,15],alpha=0,loc=c(jotas[t,2,15],jotas[t,5,15]),n=201)
##eps[,,16] <- ellipsePoints(a=jotas[t,2,16]-jotas[t,1,16],b=jotas[t,5,16]-jotas[t,4,16],alpha=0,loc=c(jotas[t,2,16],jotas[t,5,16]),n=201)
##eps[,,17] <- ellipsePoints(a=jotas[t,2,17]-jotas[t,1,17],b=jotas[t,5,17]-jotas[t,4,17],alpha=0,loc=c(jotas[t,2,17],jotas[t,5,17]),n=201)
##eps[,,18] <- ellipsePoints(a=jotas[t,2,18]-jotas[t,1,18],b=jotas[t,5,18]-jotas[t,4,18],alpha=0,loc=c(jotas[t,2,18],jotas[t,5,18]),n=201)
##eps[,,19] <- ellipsePoints(a=jotas[t,2,19]-jotas[t,1,19],b=jotas[t,5,19]-jotas[t,4,19],alpha=0,loc=c(jotas[t,2,19],jotas[t,5,19]),n=201)
##eps[,,20] <- ellipsePoints(a=jotas[t,2,20]-jotas[t,1,20],b=jotas[t,5,20]-jotas[t,4,20],alpha=0,loc=c(jotas[t,2,20],jotas[t,5,20]),n=201)
##eps[,,21] <- ellipsePoints(a=jotas[t,2,21]-jotas[t,1,21],b=jotas[t,5,21]-jotas[t,4,21],alpha=0,loc=c(jotas[t,2,21],jotas[t,5,21]),n=201)
##eps[,,22] <- ellipsePoints(a=jotas[t,2,22]-jotas[t,1,22],b=jotas[t,5,22]-jotas[t,4,22],alpha=0,loc=c(jotas[t,2,22],jotas[t,5,22]),n=201)
##eps[,,23] <- ellipsePoints(a=jotas[t,2,23]-jotas[t,1,23],b=jotas[t,5,23]-jotas[t,4,23],alpha=0,loc=c(jotas[t,2,23],jotas[t,5,23]),n=201)
##eps[,,24] <- ellipsePoints(a=jotas[t,2,24]-jotas[t,1,24],b=jotas[t,5,24]-jotas[t,4,24],alpha=0,loc=c(jotas[t,2,24],jotas[t,5,24]),n=201)
##eps[,,25] <- ellipsePoints(a=jotas[t,2,25]-jotas[t,1,25],b=jotas[t,5,25]-jotas[t,4,25],alpha=0,loc=c(jotas[t,2,25],jotas[t,5,25]),n=201)
##eps[,,26] <- ellipsePoints(a=jotas[t,2,26]-jotas[t,1,26],b=jotas[t,5,26]-jotas[t,4,26],alpha=0,loc=c(jotas[t,2,26],jotas[t,5,26]),n=201)
##eps[,,27] <- ellipsePoints(a=jotas[t,2,27]-jotas[t,1,27],b=jotas[t,5,27]-jotas[t,4,27],alpha=0,loc=c(jotas[t,2,27],jotas[t,5,27]),n=201)
##eps[,,28] <- ellipsePoints(a=jotas[t,2,28]-jotas[t,1,28],b=jotas[t,5,28]-jotas[t,4,28],alpha=0,loc=c(jotas[t,2,28],jotas[t,5,28]),n=201)
##eps[,,29] <- ellipsePoints(a=jotas[t,2,29]-jotas[t,1,29],b=jotas[t,5,29]-jotas[t,4,29],alpha=0,loc=c(jotas[t,2,29],jotas[t,5,29]),n=201)
##eps[,,30] <- ellipsePoints(a=jotas[t,2,30]-jotas[t,1,30],b=jotas[t,5,30]-jotas[t,4,30],alpha=0,loc=c(jotas[t,2,30],jotas[t,5,30]),n=201)
##eps[,,31] <- ellipsePoints(a=jotas[t,2,31]-jotas[t,1,31],b=jotas[t,5,31]-jotas[t,4,31],alpha=0,loc=c(jotas[t,2,31],jotas[t,5,31]),n=201)
##eps[,,32] <- ellipsePoints(a=jotas[t,2,32]-jotas[t,1,32],b=jotas[t,5,32]-jotas[t,4,32],alpha=0,loc=c(jotas[t,2,32],jotas[t,5,32]),n=201)
##eps[,,33] <- ellipsePoints(a=jotas[t,2,33]-jotas[t,1,33],b=jotas[t,5,33]-jotas[t,4,33],alpha=0,loc=c(jotas[t,2,33],jotas[t,5,33]),n=201)
##eps[,,34] <- ellipsePoints(a=jotas[t,2,34]-jotas[t,1,34],b=jotas[t,5,34]-jotas[t,4,34],alpha=0,loc=c(jotas[t,2,34],jotas[t,5,34]),n=201)
##eps[,,35] <- ellipsePoints(a=jotas[t,2,35]-jotas[t,1,35],b=jotas[t,5,35]-jotas[t,4,35],alpha=0,loc=c(jotas[t,2,35],jotas[t,5,35]),n=201)
##eps[,,36] <- ellipsePoints(a=jotas[t,2,36]-jotas[t,1,36],b=jotas[t,5,36]-jotas[t,4,36],alpha=0,loc=c(jotas[t,2,36],jotas[t,5,36]),n=201)
##eps[,,37] <- ellipsePoints(a=jotas[t,2,37]-jotas[t,1,37],b=jotas[t,5,37]-jotas[t,4,37],alpha=0,loc=c(jotas[t,2,37],jotas[t,5,37]),n=201)
##eps[,,38] <- ellipsePoints(a=jotas[t,2,38]-jotas[t,1,38],b=jotas[t,5,38]-jotas[t,4,38],alpha=0,loc=c(jotas[t,2,38],jotas[t,5,38]),n=201)
##eps[,,39] <- ellipsePoints(a=jotas[t,2,39]-jotas[t,1,39],b=jotas[t,5,39]-jotas[t,4,39],alpha=0,loc=c(jotas[t,2,39],jotas[t,5,39]),n=201)
##eps[,,40] <- ellipsePoints(a=jotas[t,2,40]-jotas[t,1,40],b=jotas[t,5,40]-jotas[t,4,40],alpha=0,loc=c(jotas[t,2,40],jotas[t,5,40]),n=201)
##eps[,,41] <- ellipsePoints(a=jotas[t,2,41]-jotas[t,1,41],b=jotas[t,5,41]-jotas[t,4,41],alpha=0,loc=c(jotas[t,2,41],jotas[t,5,41]),n=201)
##eps[,,42] <- ellipsePoints(a=jotas[t,2,42]-jotas[t,1,42],b=jotas[t,5,42]-jotas[t,4,42],alpha=0,loc=c(jotas[t,2,42],jotas[t,5,42]),n=201)
##eps[,,43] <- ellipsePoints(a=jotas[t,2,43]-jotas[t,1,43],b=jotas[t,5,43]-jotas[t,4,43],alpha=0,loc=c(jotas[t,2,43],jotas[t,5,43]),n=201)
##eps[,,44] <- ellipsePoints(a=jotas[t,2,44]-jotas[t,1,44],b=jotas[t,5,44]-jotas[t,4,44],alpha=0,loc=c(jotas[t,2,44],jotas[t,5,44]),n=201)
##eps[,,45] <- ellipsePoints(a=jotas[t,2,45]-jotas[t,1,45],b=jotas[t,5,45]-jotas[t,4,45],alpha=0,loc=c(jotas[t,2,45],jotas[t,5,45]),n=201)
##eps[,,46] <- ellipsePoints(a=jotas[t,2,46]-jotas[t,1,46],b=jotas[t,5,46]-jotas[t,4,46],alpha=0,loc=c(jotas[t,2,46],jotas[t,5,46]),n=201)
##eps[,,47] <- ellipsePoints(a=jotas[t,2,47]-jotas[t,1,47],b=jotas[t,5,47]-jotas[t,4,47],alpha=0,loc=c(jotas[t,2,47],jotas[t,5,47]),n=201)
##eps[,,48] <- ellipsePoints(a=jotas[t,2,48]-jotas[t,1,48],b=jotas[t,5,48]-jotas[t,4,48],alpha=0,loc=c(jotas[t,2,48],jotas[t,5,48]),n=201)
##eps[,,49] <- ellipsePoints(a=jotas[t,2,49]-jotas[t,1,49],b=jotas[t,5,49]-jotas[t,4,49],alpha=0,loc=c(jotas[t,2,49],jotas[t,5,49]),n=201)
##eps[,,50] <- ellipsePoints(a=jotas[t,2,50]-jotas[t,1,50],b=jotas[t,5,50]-jotas[t,4,50],alpha=0,loc=c(jotas[t,2,50],jotas[t,5,50]),n=201)
##eps[,,51] <- ellipsePoints(a=jotas[t,2,51]-jotas[t,1,51],b=jotas[t,5,51]-jotas[t,4,51],alpha=0,loc=c(jotas[t,2,51],jotas[t,5,51]),n=201)
##eps[,,52] <- ellipsePoints(a=jotas[t,2,52]-jotas[t,1,52],b=jotas[t,5,52]-jotas[t,4,52],alpha=0,loc=c(jotas[t,2,52],jotas[t,5,52]),n=201)
##eps[,,53] <- ellipsePoints(a=jotas[t,2,53]-jotas[t,1,53],b=jotas[t,5,53]-jotas[t,4,53],alpha=0,loc=c(jotas[t,2,53],jotas[t,5,53]),n=201)
##eps[,,54] <- ellipsePoints(a=jotas[t,2,54]-jotas[t,1,54],b=jotas[t,5,54]-jotas[t,4,54],alpha=0,loc=c(jotas[t,2,54],jotas[t,5,54]),n=201)
##eps[,,55] <- ellipsePoints(a=jotas[t,2,55]-jotas[t,1,55],b=jotas[t,5,55]-jotas[t,4,55],alpha=0,loc=c(jotas[t,2,55],jotas[t,5,55]),n=201)
##eps[,,56] <- ellipsePoints(a=jotas[t,2,56]-jotas[t,1,56],b=jotas[t,5,56]-jotas[t,4,56],alpha=0,loc=c(jotas[t,2,56],jotas[t,5,56]),n=201)
##eps[,,57] <- ellipsePoints(a=jotas[t,2,57]-jotas[t,1,57],b=jotas[t,5,57]-jotas[t,4,57],alpha=0,loc=c(jotas[t,2,57],jotas[t,5,57]),n=201)
##eps[,,58] <- ellipsePoints(a=jotas[t,2,58]-jotas[t,1,58],b=jotas[t,5,58]-jotas[t,4,58],alpha=0,loc=c(jotas[t,2,58],jotas[t,5,58]),n=201)
##eps[,,59] <- ellipsePoints(a=jotas[t,2,59]-jotas[t,1,59],b=jotas[t,5,59]-jotas[t,4,59],alpha=0,loc=c(jotas[t,2,59],jotas[t,5,59]),n=201)
##eps[,,60] <- ellipsePoints(a=jotas[t,2,60]-jotas[t,1,60],b=jotas[t,5,60]-jotas[t,4,60],alpha=0,loc=c(jotas[t,2,60],jotas[t,5,60]),n=201)
##eps[,,61] <- ellipsePoints(a=jotas[t,2,61]-jotas[t,1,61],b=jotas[t,5,61]-jotas[t,4,61],alpha=0,loc=c(jotas[t,2,61],jotas[t,5,61]),n=201)
##eps[,,62] <- ellipsePoints(a=jotas[t,2,62]-jotas[t,1,62],b=jotas[t,5,62]-jotas[t,4,62],alpha=0,loc=c(jotas[t,2,62],jotas[t,5,62]),n=201)
##eps[,,63] <- ellipsePoints(a=jotas[t,2,63]-jotas[t,1,63],b=jotas[t,5,63]-jotas[t,4,63],alpha=0,loc=c(jotas[t,2,63],jotas[t,5,63]),n=201)
##eps[,,64] <- ellipsePoints(a=jotas[t,2,64]-jotas[t,1,64],b=jotas[t,5,64]-jotas[t,4,64],alpha=0,loc=c(jotas[t,2,64],jotas[t,5,64]),n=201)
##eps[,,65] <- ellipsePoints(a=jotas[t,2,65]-jotas[t,1,65],b=jotas[t,5,65]-jotas[t,4,65],alpha=0,loc=c(jotas[t,2,65],jotas[t,5,65]),n=201)
##eps[,,66] <- ellipsePoints(a=jotas[t,2,66]-jotas[t,1,66],b=jotas[t,5,66]-jotas[t,4,66],alpha=0,loc=c(jotas[t,2,66],jotas[t,5,66]),n=201)
##eps[,,67] <- ellipsePoints(a=jotas[t,2,67]-jotas[t,1,67],b=jotas[t,5,67]-jotas[t,4,67],alpha=0,loc=c(jotas[t,2,67],jotas[t,5,67]),n=201)

### SET t
t <- 5
for (t in 1:T){
par(mar = c(3.1, 3.1, 2.1, 2.1) )
##plot(c(-1.5,1.5),c(-1.5,1.5),xlim = lims,ylim = lims,type="n",
##       xlab=c(""), ##xlab=c("pro-SQ                                         pro-change"),
##       ylab=c(""), ##ylab=c("interpretivist                                            literalist"),
##       main=c("")) ##main=paste("Acc+Con (ancla j) time=",t,I,"obs"))
plot(c(-1.5,1.5),c(-1.5,1.5),type="n",
       xlab=c(""), ##xlab=c("pro-SQ                                         pro-change"),
       ylab=c(""), ##ylab=c("interpretivist                                            literalist"),
       main=c("")) ##main=paste("Acc+Con (ancla j) time=",t,I,"obs"))
abline(-1.5,0,col="grey",lty=3); abline(-1,0,col="grey",lty=3); abline(-.5,0,col="grey",lty=3); 
       abline(0,0,col="grey",lty=3); abline(.5,0,col="grey",lty=3); abline(1,0,col="grey",lty=3); 
       abline(1.5,0,col="grey",lty=3); 
abline(v=-1.5,col="grey",lty=3); abline(v=-1,col="grey",lty=3); abline(v=-.5,col="grey",lty=3); 
       abline(v=0,col="grey",lty=3); abline(v=.5,col="grey",lty=3); abline(v=1,col="grey",lty=3); 
       abline(v=1.5,col="grey",lty=3);
legend(1.1,-.65, legend=part.list, cex=.75, pch=20, pt.cex=1.25, col=color.list, bg="white")
##for (j in 1:J){
##    segments(jotas[t,1,j],jotas[t,5,j],jotas[t,3,j],jotas[t,5,j],col="gray")
##    segments(jotas[t,2,j],jotas[t,4,j],jotas[t,2,j],jotas[t,6,j],col="gray")
##    }
##for (j in 1:J){
##    lines(eps[,,j],col=color.67[j])
##    }
for (j in 1:J){
    points(jotas[t,2,j],jotas[t,5,j],pch=20,col=color.67[j])
    }
for (j in 1:J){ 
    points(jotas[t,2,j],jotas[t,5,j], col=dCoord[j]); ## pone coordinadores
    }
##for (j in 1:J){
##    text(jotas[t,2,j],jotas[t,5,j],labels=coords[j])
##    }
tmp <- c("2006-3","2007-1","2007-2","2007-3","2008-1","2008-2","2008-3","2009-1")
setwd("d:/01/data/rollcall/aldf/graphs")
savePlot(filename = paste(tmp[t], sep=""), type = "pdf")
#setwd("c:/data")
setwd("d:/01/data/rollcall/aldf")
}

## cutlines
cuad <- 1*d1+2*d2+3*d3+4*d4+5*d5+6*d6+7*d7+8*d8
t <- 5
for (t in 1:8){
    plot(c(-1.5,1.5),c(-1.5,1.5),type="n",
           xlab=c(""), ##xlab=c("pro-SQ                                         pro-change"),
           ylab=c(""), ##ylab=c("interpretivist                                            literalist"),
           main=c("")) ##main=paste("Acc+Con (ancla j) time=",t,I,"obs"))
    atmp <- amed[cuad==t]; btmp <- bmed[cuad==t]; 
    N <- length(atmp)
    for (n in 1:N){
        abline(a=btmp[n],b=atmp[n]) } ## OJO: a en mi modelo es slope, en R es constant
    for (j in 1:J){
        points(jotas[t,2,j],jotas[t,5,j],pch=20,col=color.67[j])
        }
    tmp <- c("2006-3","2007-1","2007-2","2007-3","2008-1","2008-2","2008-3","2009-1")
    setwd("d:/01/data/rollcall/aldf/graphs")
    savePlot(filename = paste(tmp[t], "cutlines", sep=""), type = "pdf")
    setwd("d:/01/data/rollcall/aldf")
              }





# Prob of being median
attach.bugs(trife.edos.1)
is.median <- matrix(NA, nrow=3000, ncol=7)
med <- rep(NA, times=3000)
for (i in 1:3000){
    med[i]<-median(x[i,1:7])}
for (i in 1:3000){
    for (j in 1:7){
        is.median[i,j] <- ifelse(x[i,j]==med[i],1,0)}}
pr.median <- rep(NA, times=7, names=names.1)
for (j in 1:7){
    pr.median[j] <- sum(is.median[,j]/3000)}
names.1
pr.median


#HISTOGRAM OF POSTERIOR m
oldpar <- par(no.readonly=TRUE)

attach.bugs(trife.all.1)

#par(mfrow=c(2,1))
#par(oldpar)

eme <- c(m[,1],m[,2],m[,3],m[,4],m[,5],m[,6],m[,7])
#equis <- c(-400:400)/100
hist(eme[abs(eme)<=3], col="gray", xlim=c(-3,3), ylim=c(-.04,.55),freq=FALSE, main="m_i's prior and posterior densities", xlab=NULL)
points(all.1.80[,2],rep(0,times=7), pch=20)
curve(dnorm, from=-3, to=3, add=TRUE)
text(all.1.80[1,2],-.03,label="Reyes")
text(all.1.80[7,2],-.03,label="Fuentes")
#densityplot(eme[abs(eme)<=4])

#######################################################
### Static 66 members 2Dimensions extremist anchors ###
#######################################################

### ALLOWS TO DROP CASES FROM ANALYSIS
#year1 <- ifelse( (cuad==1 | cuad==2 | cuad==3), 1, 0 ) ## FALTA QUITAR A UN DIPUTADO (M?S?) QUE NO ENTR? HASTA DESPU?S
#drop <- ifelse(year1==0,1,0)
#tmp<-RCs[drop==0,]
#RCs<-tmp
#sem<-sem[drop==0]; cuad<-cuad[drop==0]; trim<-trim[drop==0]

votes <- RCs[,9:ncol(RCs)]
#votes[votes==-1] <- 0  # los -1s se vuelven 0s # DEJA ABSTENCION COMO VOTO NAY  
votes <- t(votes)
J <- nrow(votes); I <- ncol(votes)
v <- votes
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
aldf.data <- list ("J", "I", "lo.v", "hi.v", "N", "W", "E", "S")
aldf.inits <- function (){
    list (
    v.star=vstar,
    delta=rnorm(I),
    angle=runif(I),
    b=rnorm(I)
#    x=rnorm(J),
#    y=rnorm(J)
    )
    }
aldf.parameters <- c("delta", "x", "y", "a", "b", "angle")

#test ride to see program works
aldf.66 <- bugs (aldf.data, aldf.inits, aldf.parameters, 
                "model66Sta2Dj.txt", n.chains=3, 
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

plot(aldf.66)
print(aldf.66)

#longer run
aldf.66 <- bugs (aldf.data, aldf.inits, aldf.parameters, 
                "model66Sta2Dj.txt", n.chains=3, 
                n.iter=10000, n.thin=25, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

#to continue running
tmp1<-list (
    v.star=vstar,
    delta=aldf.66$last.values[[1]]$delta,
    angle=aldf.66$last.values[[1]]$angle,
    b=aldf.66$last.values[[1]]$b,
    x=aldf.66$last.values[[1]]$x,
    y=aldf.66$last.values[[1]]$y
    )
tmp2<-list (
    v.star=vstar,
    delta=aldf.66$last.values[[2]]$delta,
    angle=aldf.66$last.values[[2]]$angle,
    b=aldf.66$last.values[[2]]$b,
    x=aldf.66$last.values[[2]]$x,
    y=aldf.66$last.values[[2]]$y
    )
tmp3<-list (
    v.star=vstar,
    delta=aldf.66$last.values[[3]]$delta,
    angle=aldf.66$last.values[[3]]$angle,
    b=aldf.66$last.values[[3]]$b,
    x=aldf.66$last.values[[3]]$x,
    y=aldf.66$last.values[[3]]$y
    )
### for (chain in 1:3){aldf.66$last.values[[chain]]$v.star <- vstar}
aldf.66.2 <- bugs (aldf.data, 
                inits=list(tmp1,tmp2,tmp3), 
                aldf.parameters, 
                "model66Sta2Dj.txt", n.chains=3, 
                n.iter=25000, n.thin=60, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

aldf.66 <- aldf.66.2
rm(aldf.66.2)

