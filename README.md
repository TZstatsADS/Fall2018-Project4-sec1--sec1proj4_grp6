# Project: OCR (Optical Character Recognition) 

![image](figs/intro.png)

### [Full Project Description](doc/project4_desc.md)

Term: Fall 2018

+ Team # 6
+ Team members
	+ team member 1: Yi Lin
	+ team member 2: Yang Xing
	+ team member 3: Chuqiao Rong
	+ team member 4: Hongru Liu
	+ team member 5: Liu Han

+ Project summary: In this project, we created an OCR post-processing procedure to enhance Tesseract OCR output. The detection method is n-gram and correction method is probability scoring. 
Error detection is performed by comparing all possible bigrams of all words in Tesseract with positional binary matrix constructed from Ground Truth. If any bigrams of a word do not appear in the corresponding positional binary matrix then the word is classified as error. 
Error correction is performed by using Baysian probability to find the correction type with highest prior probability multiplied by probability of it being a typo given the correction. Different methods of estimations such as MLE and ELE are performed. 
Performance evaluation is to compare the correction accuracy of different methods of estimations by calculating the proportion of errors before and after correction.
	
[**Contribution statement**](doc/a_note_on_contributions.md)): All team members approve our work presented in this GitHub repository including this contributions statement. The contribution for each team member is following:

	+ Hongru Liu: Error detection using paper D2 and performence evaluation
	+ Chuqiao Rong: Error correction using papaer C4
	+ Yang Xing: Error correction using papaer C4
	+ Yi Lin: README files and presentation slides

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
