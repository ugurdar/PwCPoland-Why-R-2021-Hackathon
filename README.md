# whyR-PwC Hackathon 2021

This repo consists the script, data and readme file of the submission for hackathon implemented by Ugur Dar and Mustafa Cavus.

Our motivation is to propose new classifier method to maximize accuracy based on optimal cut-off value approach. The optimal 
cut-value is obtained by a search algorithm is "optimal_cut". It needs a initial value and it is obtained from the summary 
statistics of the similarity values of true matched texts in train set. After obtained the optimal cut value, the proposed classifier proposed_matcher() function returns the labels of the texts considered to be similar. The accuracy and F1 score  for train set 
of our proposed method are 0.9912 and 0.9946, respectively. 

Please see our submission in [WhyR - PwC Hackathon.R](https://github.com/ugurdar/PwCPoland/blob/main/WhyR%20-%20PwC%20Hackathon.R)
and the our predictions in [valid-submission.csv](https://github.com/ugurdar/PwCPoland/blob/main/valid-submission.csv).


