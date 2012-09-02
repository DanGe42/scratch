from flask import Flask, render_template, jsonify
from sheet_helper import SheetHelper
import config

app = Flask(__name__)

# configuration
SPREADSHEET_KEY = config.spreadsheet_key
EMAIL = config.email
PASSWORD = config.password

@app.route('/')
def index():
    """ Render the home page with the graph """

    return render_template('index.html')

@app.route('/update')
def update():
    """ Grab updates and return JSON data """
    sheet = SheetHelper(EMAIL, PASSWORD, SPREADSHEET_KEY)
    return jsonify(data=sheet.scores)

@app.route('/about')
def about():
    """ Render the about page """
    return render_template('about.html')


if __name__ == '__main__':
    app.run()
