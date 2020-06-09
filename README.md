
# Table of Contents

1.  [Description of *Roll call votes data for the Mexican Chamber of Deputies* repository](#org8112397)
2.  [License](#orga7e1892)
3.  [Citation in BibTex format](#org369b9b2)
4.  [Codebook](#org52e66c9)
5.  [Acknowledgements](#orga4bf0f7)

Last revision: 2020-06-09


<a id="org8112397"></a>

# Description of *Roll call votes data for the Mexican Chamber of Deputies* repository

-   Author: Eric Magar
-   Email: emagar at gmail dot com
-   GitHub: <https://github.com/emagar/dipMex>

The repository contains roll call data for recent Cámaras de Diputados of the Mexican Congress. It also includes data on the deputies present in each. Data has been compiled primarily from the Cámara's web page at <http://www.diputados.gob.mx/>, with information from other sources and the press.  

Repository contains code, raw data, and clean roll call databases. Code included replicates data downloading from the primary source (`code/getweb/`); databases preparation from raw data (`code/rcPrep`); and descriptive analysis and ideal point estimation (`code/rcAnalysis/`). Raw data is in `data/fromWeb/` directory. ****If interested in clean roll call votes only, simply copy the contents of the `data/votesForWeb/` subdirectory****. Data includes roll call votes of the 60th (1sep2006-31aug2009), 61st (1sep2009-31aug2012), and most of the 62nd (1sep2012-7oct2014) Legislatures. Data is in R (<http://cran.r-project.org/>) and csv formats. 


<a id="orga7e1892"></a>

# License

This repository in under the MIT License, see <http://opensource.org/licenses/MIT>. The sole condition to use the data is to cite it as follows: Francisco Cantu, Scott Desposato, and Eric Magar. 2014. "Consideraciones metodologicas para estudiantes de politica legislativa mexicana: sesgo por seleccion en votaciones nominales". Politica y Gobierno vol. 21, num. 1, pp. 25-54.


<a id="org369b9b2"></a>

# Citation in BibTex format

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


<a id="org52e66c9"></a>

# Codebook

The objects in each R file (zipped together in text-only files) are the following:

-dipdat: individual federal deputy information (nom=name, id=district, part=party, 
 edo=state, edon=state number, dsmd=indicates single-member district deputies, 
 dis=district number, tipo=propietario or suplente)

-votdat: vote information (favor=ayes, contra=nays, absten=abstained,
 quorum=present but not voting, ausen=no show, title=motion considered, 
 leg=legislature, yr-mo-dy=vote's date)

-rc: roll call vote information (0=was not chamber member, 1=aye,
 2=nay, 3=abstained, 4=present but did not vote, 5=no show)

-dgaceta: indicates whether (1) or not (0) the vote was also reported by the
 **Gaceta Parlamentaria**, as discussed by Cantu, Desposato, and Magar 2014.


<a id="orga4bf0f7"></a>

# Acknowledgements

Eric Magar acknowledges financial support from the Asociación Mexicana de Cultura A.C. and CONACYT's Sistema Nacional de Investigadores. He is responsible for mistakes and shortcomings in the data. I am grateful to Ana Lucía Enríquez Araiza and Vidal Mendoza Tinoco for research assistance. 

