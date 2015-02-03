TUW-IMP at the NTCIR-11 Math2
=============================

This repository contains the Information Retrieval (IR) system developed by the TUW-IMP group for the NTCIR-11 Math2 challenge. 
Details about its implementation are available in the following 
[Technical Report, Presentation and Poster](https://www.researchgate.net/publication/269988341).

How to use the system
---------------------

To run the commands explained below it is needed the jar file downloadable from the folder 
[out/artifacts/tuw_imp_at_the_ntcir-11_math2](https://github.com/aldolipani/TUW-IMP_at_the_NTCIR-11_Math2/tree/master/out/artifacts/tuw_imp_at_the_ntcir-11_math2).
Each command, described below, needs to be preceded by the following command: `java -jar tuw-imp_at_the_ntcir-11_math2.jar`

Help
----

The command `--help` shows the following instructions:

```
Search:    --cmd search --name-run <name> --topics <path> --expansions <path> --formula-index <path> --text-index <path>
Index
 Text:     --cmd index-text     --test-collection <path> --index-folder <path>
 Formulae: --cmd index-formulae --test-collection <path> --index-folder <path>
```

Search
------
After having created the indices (as explained below) the search command search over them. The parameter `--topics` needs the 
topics in the XML file, with the format as given by the organizers of the Math challenge. Each topic is composed of a set
of formulae and a set of keywords that are filed respectively to the two indices passed through `--formula-index` for the
index of formulae and `--text-index` for the index of text.
If the text index is omitted the system will search only over the formulae index. However if it is not omitted and there are 
keywords in the topics to be searched, it is possible to apply a topic expansion to each of them through an XML file passed 
through the parameter `--expansions`.
The expansions used for the challenge NTCIR-11 Math2 are provided in
[input/NTCIR11-Math2-Query_Expansions](https://github.com/aldolipani/TUW-IMP_at_the_NTCIR-11_Math2/tree/master/out/input/NTCIR11-Math2-Query_Expansions.xml).

At the moment, the only settings about the parameters of the system are the ones used in the challenge that can be set up 
throng the name of the run. The characteristic of each run are described in the 
[technical report](https://www.researchgate.net/publication/269988341), 
which are: FLA, FLASM, FLAN and FLASL.

Index Text and Formulae
-----------------------
As we have seen the Search command needs two indices, one for formulae and one for text that can be respectively created using the 
two commands `--index-formulae` and `--index-text`. These two commands need as input the compressed test collection (tar.gz) 
as provided by the organizers of the Math challenge, which is passed through the parameter `--test-collection`.
And generate as output a Lucene index in the folder specified with the parameter `--index-folder`.

