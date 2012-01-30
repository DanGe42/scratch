from csv_reader import Parser
from html_out   import generate_html
from sys        import argv
import os

csv_file = argv[1]
parser = Parser(csv_file)

TITLE = 3
app_dir = "./applications/"

if not os.path.exists(app_dir):
    os.makedirs(app_dir)

for entry in parser.data:
    out_file = open(app_dir + entry[TITLE] + ".html", 'w')
    out_file.write(generate_html(parser.header, entry, TITLE))
    out_file.close()


