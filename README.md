VisuaLisPy
=========
A browser-based abstract syntax tree visualizer.

![intro_image](https://raw.githubusercontent.com/lolilo/VisuaLisPy/master/images/Intro.png)

 * Built a Scheme-to-JavaScript compiler and a JavaScript parser using Python.
 * Constructed browser-based abstract syntax tree visualizer using JavaScript/jQuery/Ajax/JSON, 
D3.js, HTML, CSS. 
 * Set up PostgreSQL database using SQLAlchemy containing example input for visualizer.

Deployed on Heroku: [http://visualispy.herokuapp.com/](http://visualispy.herokuapp.com/) (Database may take some time to "wake up" on Heroku's server.)

This is a fork of [lolilo's reop](https://github.com/lolilo/VisuaLisPy).


Getting Started
------------------
Clone this repository to your local machine.

Make sure your using python >= 3.10, and ocaml using opam.

From the VisuaLisPy working directory, set up a [virtual environment](http://docs.python-guide.org/en/latest/dev/virtualenvs/) and install requirements with

     VisuaLisPy$ pip install -r requirements.txt

Install the ocaml-in-python library as described [here](https://github.com/thierry-martinez/ocaml-in-python).

If your using a venv, you may need to rerun
    `pip install --editable " 'opam var ocaml-in-python:lib'" `
on your venv.


Then, run

     VisuaLisPy$ python controller.py

This should hopefully get the web app running.

