#Santander Customer Satisfaction

#### Introduction:

The task was to analyze anonymized(by variables) data and predict which customers of a popular European bank are likely to be 
dissatisified. Click [here](https://www.kaggle.com/c/santander-customer-satisfaction) to know more about the competition.

This competition is by far the most popular mainstream competition on kaggle and the reason I was so much intrested in it because **anyone can win** as there are so many features to mess around.

#### Challenges:

- The dataset is unbalanced(96:4)
- There are a lot of variables :grin: but they are anonymized :disappointed:, which means we can not apply any industry knowledge
- The data set has duplicate records. Sometimes these records don't have the same value in the dependent variable :anguished:
- The dataset comprises of continuous vaiables(mostly) and mode of almost all the variables is 0
- There are many variables which are an outcome of two or more variables, the challenge here is should I keep the outcome or the raw variables or both :confused:, some are as follows:
  1. saldo_var26  = saldo_var32 + saldo_var25
  2. saldo_var1   = saldo_var40 + saldo_var18 + saldo_var34
  3. saldo_var12  = saldo_var14 + saldo_var20 + saldo_var24
- Most importantly, the validation stratergy is unrelaiable. I have tried many, built some throughout the competition, some which I had never thought of or tried earlier
- The forums and public scripts are useful and misleading at the same time(which is good).

#### Words of wisdom:

> I've always(most of the times) thought believing in one's CV is the key to win a competition, but this competition proved me wrong. [I could have secured 80th place(not bragging) had I left just after my first submission(not bragging at all)](https://www.kaggle.com/c/santander-customer-satisfaction/forums/t/20895/its-nobody-s-fault). Well, its just an excuse to keep myself motivated :wink:.


Finally, I still don't know whether Santander wants to minimize customer attrition by this competition because there a lot of reasons for a customer to leave(apart from dissatisifaction) and at times it has nothing to do with the customer(hope the guys at Santander know that).




