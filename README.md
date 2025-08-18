# 🎓 Student Retention Prediction  
Predicting at-risk students with machine learning to support early interventions and improve student success.  

## 🌟 Quick Highlights  
- ✅ **Built end-to-end pipeline**: SQL database design → feature engineering → ML modeling → interpretability  
- 📊 **Compared 3 models** (LASSO, Random Forest, Neural Additive Models) for performance + explainability  
- 🧩 **Handled 4 datasets** efficiently with Python dicts & custom preprocessing  
- 🔍 **Used MICE imputation, SMOTE, feature importance & shape functions** for robust modeling  
- 🎯 **Challenged administrator assumptions**: showed demographic/academic/aid data alone was insufficient for retention prediction  

## 🚀 Overview  
Student retention is one of the biggest challenges in higher education. In this project, I developed machine learning models to identify students most at risk of dropping out, enabling institutions to intervene early and improve retention.  

I designed **custom relational database tables**, queried them with SQL, and joined with institutional data to create a unified dataset for analysis. The modeling pipeline balanced predictive performance with interpretability, comparing **LASSO**, **Random Forest**, and **Neural Additive Models (NAMs)**.  

📌 *Why it matters*: This project revealed that **administrative data alone (demographic, financial aid, academic) was not enough** to explain retention at this institution—challenging administrator assumptions and paving the way for deeper data collection (e.g., surveys of exiting students).  

## 🔑 Key Features  
- 🗄️ **Database design & SQL**: created custom tables, performed joins with institutional data  
- 🧹 **Data preprocessing** with MICE imputation for missing values  
- 📊 **Exploratory Data Analysis (EDA)** with impactful visualizations  
- 🛠️ **Feature engineering** across four datasets, streamlined with Python dictionaries  
- ⚡ **Model training & hyperparameter tuning** with cross-validation  
- 📈 **Evaluation metrics**: accuracy, precision, recall, F1-score, ROC-AUC  
- 🔍 **Interpretability**: feature importance and shape functions to understand model behavior  
- ⚖️ **Class imbalance handling** with SMOTE  

## 🧰 Tech Stack  
- **Python**: pandas, scikit-learn, matplotlib, seaborn, dnamite  
- **SQL**: database design, joins, and queries for student data  
- **Modeling**: LASSO, Random Forest, Neural Additive Models  
- **Workflow**: Google Colab for development & experimentation  

## 📌 Results & Takeaways  
- NAMs offered **interpretability** while maintaining strong performance.  
- Feature importance and shape functions showed the limits of administrative-only data.  
- SMOTE improved recall for identifying at-risk students.  
- SQL design enabled clean integration of multiple data sources.  
- Python dict structures reduced redundancy when handling multiple datasets.  


## 🎯 Next Steps  
- Collect **survey data** from students who exit to enrich the model  
- Analyze **misclassified students** to understand hidden risk factors  
- Deploy as an **interactive dashboard** for advisors and administrators  
- Extend with **time-series modeling** to track retention semester by semester  

---

✨ This project bridges **data engineering, machine learning, and educational impact**—challenging assumptions and helping institutions focus resources where they matter most.  

---

## 👩‍💻 About Me  
Hi, I’m **Ruth Clayton** — a Data Scientist in training with a passion for applying machine learning to real-world problems in education, analytics, and beyond.  

- 🎓 Pursuing my **M.S. in Data Science & Analytics** at the University of Oklahoma  
- 🛠️ Experienced in **Python, SQL, and machine learning** for applied problem-solving  
- 📊 Strong interest in **interpretable AI** and **data-driven decision-making**  
- 🌎 Excited to contribute to projects where **data science creates measurable impact**  

💡 If you’re hiring or collaborating on impactful data projects, feel free to connect!  

---

