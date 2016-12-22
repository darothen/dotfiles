#!/usr/bin/env python

"""

Generate stub files for quickly rendering a d3.js visualization.

- README.md
- [vis_name].js
- [vis_name].html
- [vis_name].css
- go

Version: June 17, 2015

"""

import os, stat

import click

README_CONTENTS = """
Basic startup for `{vis_name}` visualization.

Run the `go` script to launch a mini-server to see the visualization
in {vis_name}.js; it will also open up {vis_name}.html in Google Chrome
if available.

"""

HTML_CONTENTS = """
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <title>{vis_name} Visualization Template</title>
        <script type="text/javascript" src="http://d3js.org/d3.v3.min.js"></script>
        <link type="text/css" rel="stylesheet" href="{vis_name}.css"/>
    </head>
    
    <body>
        <div id="chart"></div>
        <script type="text/javascript" src="{vis_name}.js"></script>
    </body>
</html>
"""

GO_CONTENTS = """#/usr/bin/env bash
open -a "Google Chrome" http://localhost:8000/{vis_name}.html
echo "Reload browser..."
python -m SimpleHTTPServer 8000 
"""

@click.command()
@click.argument('vis_name')
def main(vis_name):

    vis_path = os.path.join(os.getcwd(), vis_name)
    if os.path.exists(vis_path):
        click.echo("Path %s exists; continuing will overwrite files" % vis_path)
        click.echo("continue? [y/N] ")
        c = click.getchar().lower()
        if c == 'n':
            click.echo("Not doing anything...")
            return
    else:
        os.makedirs(vis_path)

    click.echo("Creating %s" % vis_path)

    # README file
    readme_fn = os.path.join(vis_path, "README.md")
    with open(readme_fn, 'wb') as f:
        f.write(README_CONTENTS.format(vis_name=vis_name))

    # HTML file
    html_fn = os.path.join(vis_path, "%s.html" % vis_name)
    with open(html_fn, 'wb') as f:
        f.write(HTML_CONTENTS.format(vis_name=vis_name))

    # JS file
    js_fn = os.path.join(vis_path, "%s.js" % vis_name)
    with open(js_fn, 'wb') as f:
        f.write('// Enter the javascript vis routine here\n')
        f.write('d3.select("body")\n')
        f.write('  .append("p")\n')
        f.write('  .text("`%s` routine will go here!")\n' % vis_name)

    # CSS file
    css_fn = os.path.join(vis_path, "%s.css" % vis_name)
    with open(css_fn, 'wb') as f:
        f.write("// Enter any stylesheet information here\n")

    # `go` - create a simple webserver that serves content from the current
    #        directory
    go_fn = os.path.join(vis_path, 'go')
    with open(go_fn, 'wb') as f:
        f.write(GO_CONTENTS.format(vis_name=vis_name))
    # set execute permissions
    st = os.stat(go_fn)
    os.chmod(go_fn, st.st_mode | stat.S_IEXEC)

    click.echo("... done!")

if __name__ == "__main__":
    main()
