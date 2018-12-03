### A Note on Contributions


Project 5: OCR POST PROCESSING: ERROR DETECTION AND CORRECTION

Team members: 
  + Hongru Liu
  + Yi Lin
  + Yang Xing
  + Chuqiao Rong
  + Liu Han

Summary: In this project, we created an OCR post-processing procedure to enhance Tesseract OCR output. The detection method is n-gram and correction method is probability scoring. Error detection is performed by comparing all possible bigrams of all words in Tesseract with positional binary matrix constructed from Ground Truth. If any bigrams of a word do not appear in the corresponding positional binary matrix then the word is classified as error. Error correction is performed by using Baysian probability to find the correction type with highest prior probability multiplied by probability of it being a typo given the correction. Different methods of estimations such as MLE and ELE are performed. Performance evaluation is to compare the correction accuracy of different methods of estimations by calculating the proportion of errors before and after correction.

[Contribution Statement] All team members approve our work presented in this GitHub repository including this contributions statement. The contribution for each team member is following:
  + Hongru Liu: Error detection using paper D2 and performence evaluation
  + Chuqiao Rong: Error correction using papaer C4
  + Yang Xing: Error correction using papaer C4
  + Yi Lin: README files and presentation slides 
  + Han Liu: Helping prepare the presentation slides and debugging
