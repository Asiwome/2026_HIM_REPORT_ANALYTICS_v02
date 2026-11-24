from bs4 import BeautifulSoup
import pandas as pd

# Load the saved HTML file
with open("InPatientMorbidityRegister_vo1_files/saved_resource.html", "r", encoding="utf-8") as f:
    html_content = f.read()

# Parse with BeautifulSoup
soup = BeautifulSoup(html_content, "html.parser")

# Extract the first table
table = soup.find("table")

# Convert to pandas DataFrame
df = pd.read_html(str(table))[0]

print(df.head())
