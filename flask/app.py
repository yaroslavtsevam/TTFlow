from dash.dependencies import Output, Input, State
import dash_bootstrap_components as dbc
from dash import dcc
from dash import html
import plotly.express as px
from flask import Flask
import pandas as pd
import dash






server = Flask(__name__)
app = dash.Dash(server=server, external_stylesheets=[dbc.themes.FLATLY], use_pages = True)
app.title = 'Состояние деревьев в реальном времени'

app.layout = dbc.Container([ 

    dash.page_container
    
    
])


if __name__=='__main__':
    app.run_server()