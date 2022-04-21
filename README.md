- [Description of *Roll call votes data for the Mexican Chamber of Deputies* repository](#org9f45733)
- [License](#org2ca3be9)
- [Citation in BibTex format](#org773b9a1)
- [Files included](#orgef5c329)
- [Codebook](#org9bb152a)
- [Acknowledgements](#org5d60a1e)

Last revision: 2021-06-29


<a id="org9f45733"></a>

# Description of *Roll call votes data for the Mexican Chamber of Deputies* repository

-   Author: Eric Magar
-   Email: emagar at gmail dot com
-   GitHub: <https://github.com/emagar/dipMex>

The repository contains roll call data for recent Cámaras de Diputados of the Mexican Congress. It also includes data on the deputies present in each. Data has been compiled primarily from the Cámara's web page at <http://www.diputados.gob.mx/>, with information from other sources and the press.

Repository contains code, raw data, and clean roll call databases. Code included replicates data downloading from the primary source (`code/getweb/`); databases preparation from raw data (`code/rcPrep`); and descriptive analysis and ideal point estimation (`code/rcAnalysis/`). Raw data is in `data/fromWeb/` directory. ****If interested in clean roll call votes only, simply copy the contents of the `data/votesForWeb/` subdirectory****. Data includes roll call votes of the 60th (1sep2006-31aug2009), 61st (1sep2009-31aug2012), and most of the 62nd (1sep2012-7oct2014) Legislatures. Data is in R (<http://cran.r-project.org/>) and csv formats.


<a id="org2ca3be9"></a>

# License

This repository in under the MIT License, see <http://opensource.org/licenses/MIT>. The sole condition to use the data is to cite it as follows: Francisco Cantu, Scott Desposato, and Eric Magar. 2014. "Consideraciones metodologicas para estudiantes de politica legislativa mexicana: sesgo por seleccion en votaciones nominales". Politica y Gobierno vol. 21, num. 1, pp. 25-54.


<a id="org773b9a1"></a>

# Citation in BibTex format

```<TeX>
@article{cantuDesposatoMagar2014,
        title = {Consideraciones metodol\'ogicas para estudiantes de pol\'itica legislativa mexicana: sesgo por selecci\'on en votaciones nominales},
        author = {Cant\'u, Francisco and Desposato, Scott and Magar, Eric},
        year = {2014},
        volume = {21},
        number = {1},
        journal = {Pol\'itica y Gobierno},
        pages = {25--54}
        url = {http://www.politicaygobierno.cide.edu/index.php/pyg/article/view/18/564}
}
```


<a id="orgef5c329"></a>

# Files included

-   `data/diputados/dip*.csv` = comma-separated vote for all diputados elected to one term of office (*Legislatura*). The numeral identifies the term in question (eg. `dip58.csv` are 58th Legislatura members). **Columns include the following data**:
    -   `pila` = member's first and middle names.
    -   `patmat` = member's last names (includes patronym and matronym, as used in Mexico).
    -   `id` = district identifier. For members elected in single-member districts (SMDs), the `id` concatenates the state abbreviation, the district number, and whether the member is *propietario* (p) or *suplente* (s). For members elected by proportional representation (PR), the `id` includes information on the second-tier district number and the member's *propietario/suplente* status.
    -   `edo` = SMD's state. For PR members, the state that they declared representing (often none).
    -   `birth` = member's birthyear.
    -   `gen` = member's gender (\*M\*ale or \*F\*emale).
    -   `part` = member's party.
    -   `postulo` = party that nominated the member, if different from `part`.
    -   `dsmd` = dummy equal 1 for SMD members, 0 for members elected by PR.
    -   `dsup` = dummy equal 1 for *suplentes*, 0 for *propietarios*. Suplentes may replace propietarios taking leaves o absence (*licencia*).
    -   `cabecera` = administrative seat of SMDs.
    -   `yrin` `moin` `dyin` = date when member took oath.
    -   `yrout` `moout` `dyout` = date when member took a leave of absence.
    -   `lider` = indicates party leaders or deputy leaders.
    -   `prescom` = member chaired a permanent committee (*comisión ordinaria*).
    -   `repite` = members present in other terms: eg. 58-62 would indicate member present in 58th and 62nd Legislaturas; but 580-62 would indicate that member was elected to the same but never took the oath of office to the 58th Legislatura.
    -   `doath` = dummy equal 1 for members who took the oath of office, 0 otherwise.
-   (Under construction)


<a id="org9bb152a"></a>

# Codebook

The objects in each R file (zipped together in text-only files) are the following:

<<<<<<< HEAD -dipdat: individual federal deputy information (nom=first name, patmat=last names (patronym and matronym), fm=gender, id=district, part=party, `=====` -dipdat: individual federal deputy information (nom=name, id=district, part=party, >>>>>>> cd7a7409e0be3d3c5513fbd13fbc598a2cb34d86 edo=state, edon=state number, dsmd=indicates single-member district deputies, dis=district number, tipo=propietario or suplente, dcarta=dummy equal 1 for members filing a letter of intent with the chamber's Junta to run for office again (inapplicable before 2018 see [this](http://eleccionconsecutiva.diputados.gob.mx/contendientes)), dreran=dummy equal 1 for members renominated, dreelected=dummy equal 1 for members who reelected.

-votdat: vote information (favor=ayes, contra=nays, absten=abstained, quorum=present but not voting, ausen=no show, title=motion considered, leg=legislature, yr-mo-dy=vote's date).

-rc: roll call vote information (0=was not chamber member, 1=aye, 2=nay, 3=abstained, 4=present but did not vote, 5=no show).

-dgaceta: indicates whether (1) or not (0) the vote was also reported by the **Gaceta Parlamentaria**, as discussed by Cantu, Desposato, and Magar 2014.


<a id="org5d60a1e"></a>

# Acknowledgements

Eric Magar Meurs acknowledges financial support from the Asociación Mexicana de Cultura A.C. Many thanks to Ana Lucía Enríquez Araiza, Sonia Kuri Kosegarten, Vidal Mendoza Tinoco, and Eugenio Solís Flores Tinoco for research assistance. The author is responsible for mistakes and shortcomings in the data. Please report any error to emagar at gmail dot com.
