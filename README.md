# RStudio Analysis on Employee Churn (Explanatory and Machine learning Analysis)

Project Aim：
1. The aim of the project is to explore what factors affect employee churn.
2. The data will use churn (yes/no) as dependent variable, and various factors such as wage, position, distance to travel, age, job satisfaction, overtime, employee/manager relationship etc to explore against dependent variable.
3. 3 machine learning models (Logistic Regression, Decision Tree, Random Forest) will also be used to explore the relationship between dependent and independent variables.

Project Result:
explanatory data analysis: 
1. wage was not the most important factor for employee churn for only certain job position (e.g research role in this dateset), indicate that employee prefer what they do and contribute over wage factor. 
2. age, overtime job satisfaction and employee/manager relationship are all related to employee churn.
3. others like education, salary hike, martial status doesn't show strong relationship.

Machine learning analysis:
1. all three machine learning models showing three factors that occured in all model analysis: overtime, wage and age.
2. even removed multicollinearity columns, these factors remained in the models showing these are the most important factos affecting employee churn.
3. employee with overtime (positive correlated) and low wage (negative correlated) are still more likely to leave
4. employee with higher age are less likely to leave (negative correlated)


