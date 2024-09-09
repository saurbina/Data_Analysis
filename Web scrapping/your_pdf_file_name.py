import PyPDF2
import os

os.chdir("/Users/saurbina/Documents/pdf-txt") #changing to the directory qith the reports
os.getcwd()

# Get the list of PDF files in the directory
pdf_files = os.listdir()

# Loop through each PDF file
for pdf_file in pdf_files:
    try:
        with open(pdf_file, 'rb') as file:
            pdf_reader = PyPDF2.PdfReader(file)
            
            # Initialize a variable to store text from all pages
            all_text = ""
            
            # Loop through each page of the PDF
            for page in pdf_reader.pages:
                text = page.extract_text() 
                all_text += text  
            
            # Write the extracted text to a .txt file
            output_file_path = os.path.join("/Users/saurbina/Documents/", pdf_file[:-4] + ".txt")
            with open(output_file_path, "a") as output_file:
                output_file.write(all_text)
    
    except Exception as e:
        # Handle any exceptions that occur during the PDF processing
        print("Error reading {}: {}".format(pdf_file, e))
        continue 



