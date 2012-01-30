from csv_reader import Parser
from html_out   import generate_html

csv_file = "app-utf8.csv"
parser = Parser(csv_file)

TITLE = 3

for entry in parser.data:
    out_file = open(entry[TITLE] + ".html", 'w')
    out_file.write(generate_html(parser.header, entry, TITLE))
    out_file.close()


