# whyR-PwC Hackathon 2021

This repo consists the script, data and readme file of the submission for hackathon implemented by Ugur Dar and Mustafa Cavus.

Our motivation is to propose new classifier method to maximize accuracy based on optimal cut-off value approach. The optimal 
cut-value is obtained by a search algorithm is "optimal_cut". It needs a initial value and it is obtained from the summary 
statistics of the similarity values of true matched texts in train set. After obtained the optimal cut value, the proposed classifier proposed_matcher() function returns the labels of the texts considered to be similar. The accuracy of our proposed method is 
0.9912 for train set. 
