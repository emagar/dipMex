**Roll call votes data for the Mexican Chamber of Deputies**

Github: https://github.com/emagar/dipMex

Author: Eric Magar

email: emagar at gmail dot com

Revised: September 11, 2018

This repository in under the MIT License, see http://opensource.org/licenses/MIT. The sole condition to use the data is to cite it as follows: Francisco Cantu, Scott Desposato, and Eric Magar. 2014. "Consideraciones metodologicas para estudiantes de politica legislativa mexicana: sesgo por seleccion en votaciones nominales". Politica y Gobierno vol. 21, num. 1, pp. 25-54.

*Citation in BibTex format*
``` TeX
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

*Description*

The main primary source is the Chamber of Deputies' web page at http://www.diputados.gob.mx/. Repository contains code, raw data, and clean roll call databases. Code included replicates data downloading from the primary source (code/getweb/); databases preparation from raw data (code/rcPrep); and descriptive analysis and ideal point estimation (code/rcAnalysis/). Raw data is in data/fromWeb/ directory. **If interested in clean roll call votes only, simply copy the contents of the data/votesForWeb/ subdirectory**. Data includes roll call votes of the 60th (1sep2006-31aug2009), 61st (1sep2009-31aug2012), and most of the 62nd (1sep2012-7oct2014) Legislatures. Data is in R (http://cran.r-project.org/) and csv formats. 

*Codebook*

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
 *Gaceta Parlamentaria*, as discussed by Cantu, Desposato, and Magar 2014.

