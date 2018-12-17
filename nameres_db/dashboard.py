from __future__ import absolute_import, division, print_function

from flask import Flask
from flask_mako import MakoTemplates, render_template
import funcy as F
import pony.orm as P
import schema as S
import json

app = Flask(__name__)
app.config['MAKO_TRANSLATE_EXCEPTIONS'] = False
mako = MakoTemplates(app)


@app.route("/")
@P.db_session
def index():
    projects = P.select(p for p in S.Project)
    print(list(projects))
    return render_template(
        "index.mako",
        projects=list(P.select(p for p in S.Project)),
        stats=S.stats(),
        S=S, F=F, json=json
    )

if __name__ == '__main__':
    S.init_db()
    app.run(host='0.0.0.0', port=8000, debug=True)
