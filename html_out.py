def generate_html(header, data_entry, title_index):
    name = data_entry[title_index]
    html = """<!DOCTYPE html>
    <html>
        <head>
            <title>""" + name + """</title>
            <link href="css/styles.css" rel="stylesheet" type="text/css" />
        </head>
        <body>
            <h1>""" + name + """'s Application</h1>
    """
    for i in range(len(header)):
        html += "<h2 class=\"question\">" + header[i] + "</h2>\n"
        html += htmlify(data_entry[i])

    html += """
        </body>
    </html>"""

    return html

def htmlify(data_str):
    output = "<p class=\"response\">"
    output += data_str.replace("\r\n", "</p>\n<p class=\"response\">")
    output += "</p>\n"
    return output
