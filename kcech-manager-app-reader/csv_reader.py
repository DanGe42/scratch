import csv

class Parser(object):
    def __init__(self, csv_filename):
        csv_file   = open(csv_filename, 'rb')
        csv_reader = csv.reader(csv_file, delimiter='\t', quotechar='"')

        self.header = csv_reader.next()
        self.data   = []
        for i in csv_reader:
            self.data.append(i)

    def print_contents(self):
        print "header="
        print self.header
        print "\n\ndata="
        print self.data

    def to_utf8():
        pass

