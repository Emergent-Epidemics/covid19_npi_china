## Code and data for: The effect of human mobility and control measures on the COVID-19 epidemic in China

### Citation
The effect of human mobility and control measures on the COVID-19 epidemic in China

Moritz U.G. Kraemer, Chia-Hung Yang, Bernardo Gutierrez, Chieh-Hsi Wu , Brennan Klein, David Pigott, open COVID-19 data working group*, Louis du Plessis, Nuno R. Faria, Ruoran Li, Bill Hanage, John S. Brownstein, Maylis Layan, Alessandro Vespignani, Huaiyu Tian, Christopher Dye, Oliver G. Pybus, and Samuel V. Scarpino.

doi: https://doi.org/10.1101/2020.03.02.20026708

### Acknowledgements
We want to thank all the individuals and organizations across the world who have been willing and able to report data in as open and timely manner as possible. To see individuals involved in the often painstaking data curation process, please see [beoutbreakprepared/nCoV2019](https://github.com/beoutbreakprepared/nCoV2019) and our correspondence in The Lancet Infectious Diseases, ["Open access epidemiological data from the COVID-19 outbreak"](https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30119-5/fulltext).

*For a complete list of the COVID-19 data working group see [Specific Contributors](https://github.com/beoutbreakprepared/nCoV2019).

### Abstract
The ongoing COVID-19 outbreak has expanded rapidly throughout China. Major behavioral, clinical, and state interventions are underway currently to mitigate the epidemic and prevent the persistence of the virus in human populations in China and worldwide. It remains unclear how these unprecedented interventions, including travel restrictions, have affected COVID-19 spread in China. We use real-time mobility data from Wuhan and detailed case data including travel history to elucidate the role of case importation on transmission in cities across China and ascertain the impact of control measures. Early on, the spatial distribution of COVID-19 cases in China was well explained by human mobility data. Following the implementation of control measures, this correlation dropped and growth rates became negative in most locations, although shifts in the demographics of reported cases are still indicative of local chains of transmission outside Wuhan. This study shows that the drastic control measures implemented in China have substantially mitigated the spread of COVID-19.

### Notes on the code
To run, you need to create a folder called "secrets" in the directory "code" and add to that folder:
1.  A file called service_google_api_key.json that has the json object from your Google service API key (see https://gargle.r-lib.org/articles/non-interactive-auth.html for more information).
2. A file called google_sheet_name.txt that has the id for the google sheet on the first line of an otherwise plain text file.  In this case, you'll almost certainly want to use: 1itaohdPiAeniCXNlntNztZ_oRvjh0HsGuJXUJWET008

### Data
1. Mobility data were made public by Baidu and manually transcribed from their [web site](qiangxi.baidu.com) on February 10th, 2020 and are stored [here](https://docs.google.com/spreadsheets/d/1ov7Z2IjEPRB41rmRe4oTF1nI6L3EB9_Bn64AJIkOX6g/edit#gid=0). For more information please see: [Baidu service](https://qianxi.baidu.com/), this gnews [post](https://gnews.org/91700/), and this [blog post](https://zhuanlan.zhihu.com/p/104119625, it is in Chinese though...) where the author brute-force copied and pasted all the migration indices and proportion into her own spreadsheet, and she did some regression analysis. We are publishing these data for research purposes under Article 22 of the Copyright Law of the People's Republic of China (https://wipolex.wipo.int/en/text/466268) and US 9th Circuit Court of Appeals [No. 17-16783 D.C. No. 3:17-cv-03301-EMC](http://cdn.ca9.uscourts.gov/datastore/opinions/2019/09/09/17-16783.pdf).  Baidu's Intellectual Property statement may be found [here](https://www.baidu.com/duty/copyright.html). A version translated using Google translate is below.  Please see the Warranty section below as we make no representation as to the suitability of these data for any purpose.  We thank Baidu for making these data publicly and encourage them to continue to do so. 

Intellectual Property Statement (translated by Google translate via the [University of Virginia Biocomplexity Institute](https://dataverse.lib.virginia.edu/dataset.xhtml?persistentId=doi:10.18130/V3/YQLJ5W)) 

Baidu owns the copyright of all the materials in this website. If there are special provisions in the statement of rights of each sub-channel, those provisions shall prevail. Any authorized viewing, copying, printing and dissemination of materials belonging to this website must meet the following conditions:

* All materials and images are for informational purposes ;
* All materials and images must not be used for commercial purposes ;
* All materials, images and any part thereof must include this copyright notice ;

All products, technologies, and all programs on this website (www.baidu.com) belong to Baidu's intellectual property and are not authorized here. "Baidu", "Baidu" and related graphics are registered trademarks of Baidu.

Without Baidu's permission, no one may use (including but not limited to: illegally copying, disseminating, displaying, mirroring, uploading, downloading) or using unconventional means (such as maliciously interfering with Baidu data) to affect Baidu ’s normal Service, no one may obtain Baidu data automatically by software program without authorization. Otherwise, Baidu will pursue legal responsibility according to law.

2. Johns Hopkins maintains a cumulative case count data set [here](https://docs.google.com/spreadsheets/d/1wQVypefm946ch4XDp37uZ-wartW4V7ILdg-qYiDXUHM/edit#gid=1021921085) that we also used in addition to the line list.
3. Our team works collaboratively to maintain a public line list of COVID-19 cases.  You can find [Hubei cases here](https://docs.google.com/spreadsheets/d/1itaohdPiAeniCXNlntNztZ_oRvjh0HsGuJXUJWET008/edit#gid=429276722) and [cases Outside Hubei here](https://docs.google.com/spreadsheets/d/1itaohdPiAeniCXNlntNztZ_oRvjh0HsGuJXUJWET008/edit#gid=0). The below is copied from a post in [Virological](http://virological.org/t/epidemiological-data-from-the-ncov-2019-outbreak-early-descriptions-from-publicly-available-data/337) by [Moritz Kraemer](http://evolve.zoo.ox.ac.uk/Evolve/Moritz_Kraemer.html) that describes the data.

Epidemiological data from the 2019 nCoV outbreak: early descriptions

We have collected publicly available information on cases confirmed during the ongoing nCoV-2019 outbreak. Data were entered in a spreadsheet with each line representing a unique case, including age, sex, geographic information, history and time of travel where available. Sources were included as a reference for each entry. Data is openly available here: https://tinyurl.com/s6gsq5y 107 and was last updated at 5pm, GMT, January 23, 2020. We encourage the use of these data, which is intended as a resource for the community.

### License
(see LICENSE)

## Additional license, warranty, and copyright information
We provide a license for our code (see LICENSE) and do not claim ownership, nor the right to license, the data we have obtained nor any third-party software tools/code used in our analyses.  Please cite the appropriate agency, paper, and/or individual in publications and/or derivatives using these data, contact them regarding the legal use of these data, and remember to pass-forward any existing license/warranty/copyright information.  THE DATA AND SOFTWARE ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE DATA AND/OR SOFTWARE OR THE USE OR OTHER DEALINGS IN THE DATA AND/OR SOFTWARE.