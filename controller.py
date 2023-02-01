from flask import Flask, render_template, request
import scheme_interpreter.lis as lis
from scheme_parser.ocaml_bridge import OcamlBridge
import database.db as db
import os
import socket

app = Flask(__name__)
app.secret_key = "thisisasecret"
app.config["DEBUG"] = os.environ.get("DEBUG", False)


@app.route("/")
def index():
    html = render_template("index.html")
    return html


@app.route("/get_json", methods=["POST"])
def code_submitted():
    # return JSON to ajax call -- code input by user
    print(request.form.get("user_input"))
    user_input = request.form.get("user_input").strip()

    if not user_input:
        print('NO USER INPUT')
        return None

    json_object = lis.format_json(user_input)
    print(json_object)

    return json_object


@app.route("/get_db_code/<code_id>")
def get_db_code(code_id):
    # return JSON to ajax call -- code from database
    code_object = db.s.query(db.Code).filter_by(id=code_id).one()
    code = code_object.code
    return code


@app.route("/save_to_db", methods=["POST"])
def save_to_db():
    print(request.form.get("user_input"))
    user_input = request.form.get("user_input").strip()
    if not user_input:
        print('NO USER INPUT')

    else:
        success = db.new_code(user_input)
        if success:
            return "Share your code with http://visualispy.herokuapp.com/code/%r." % success
        else:
            return "Sorry, an error occurred."


@app.route("/code/<int:code_id>")
def display_db_code(code_id):
    # return JSON to ajax call -- code from database
    code_object = db.s.query(db.Code).filter_by(id=code_id).one()
    code = code_object.code
    html = render_template("index.html", code=code)
    return html


@app.route("/about")
def about():
    html = render_template("about.html")
    return html


if __name__ == "__main__":
    OcamlBridge()


    app.run(debug=True)
