from PyPDF3 import PdfFileReader, PdfFileWriter
import tkinter as tk
from tkinter import filedialog

root = tk.Tk()
root.withdraw()

path = filedialog.askopenfilename()
file = open(path, 'rb')
pdf = PdfFileReader(file)
writer = PdfFileWriter()

writer.appendPagesFromReader(pdf)
metadata = pdf.getDocumentInfo()
writer.addMetadata(metadata)

writer.addMetadata({'/Title': input("Enter title: ")})
writer.addMetadata({'/Author': input("Enter author: ")})
writer.addMetadata({'/Subject': input("Enter subject: ")})
writer.addMetadata({'/Keywords': input("Enter keywords: ")})

temp = path.split('.')
temp[-2] += '_tagged'
save_path = '.'.join(temp)

output = open(save_path, 'wb')
writer.write(output)
output.close()
file.close()
