import dash
from dash import html, dash_table, Input, Output
import psycopg2
from sqlalchemy import create_engine
import pandas as pd
import altair as alt
import plotly.graph_objs as go
import dash_bootstrap_components as dbc
from urllib import request
import json
#Loaing and Setting russian locale for altair
with open('/usr/src/app/ru-RU.json') as f:
  ru_format = json.load(f)
with open('/usr/src/app/ru-RU.time.json') as f:
  ru_time_format = json.load(f)

# Create an engine instance
alchemyEngine = create_engine('postgresql+psycopg2://hello_flask:hello_flask@db:5432/hello_flask_prod', pool_recycle=3600);
# Connect to PostgreSQL server
dbConnection = alchemyEngine.connect();
# Read data from PostgreSQL database table and load into a DataFrame instance

pd.set_option('display.expand_frame_repr', False);


dash.register_page(__name__, path_template="/cloud/<cloud_id>/tt/<tt_id>")

#data = pd.read_sql(query, dbConnection)
def layout(cloud_id=None, tt_id=None ):
    query  = f"select * from \"{cloud_id}\".mydata_f where \"TT_ID\" = \'{tt_id}\'"
    if (tt_id == None or tt_id == ""):
        query  = f"select * from \"{cloud_id}\".mydata_f"
    
    if(cloud_id != None):
        data = pd.read_sql(query, dbConnection)
        alt.renderers.set_embed_options(formatLocale=ru_format, timeFormatLocale=ru_time_format) 
        altair_graph =  alt.Chart(data).mark_line().encode(
            x= alt.X('Timestamp', title = "Дата"),
            y= alt.X('Tair', title = "Температура воздуха,С"),
            tooltip = 'Tair').properties(
            width=960,
            height=360
            ).configure_axis(
                labelFontSize=20,
                titleFontSize=20
            ).interactive().to_html(embed_options=dict(formatLocale=ru_format, timeFormatLocale=ru_time_format,  actions=False))
        
        return(
            dbc.Container([ 
                html.Div(id='DataDiv', style={'display': 'none'}, 
                        children=data.to_json(orient='split')),
                dbc.Row(
                    dbc.Col(
                        html.H2(f"Состояние дерева под индексом {tt_id}"), width={'size': 12, 'offset': 0, 'order': 0}
                        ),
                        style = {'textAlign': 'center', 'paddingBottom': '1%'}
                    ),
                dbc.Row(dbc.Col([
                                html.Iframe(
                                    id = 'scatterplots',
                                    srcDoc = altair_graph, # here is where we will put the graph we make
                                    style = {'border-width': '5', 'width': '100%', 'height': '500px','textAlign':'center'}),
                                ],
                                md = 12,
                                xl = {'offset':1, 'size':10 },
                                sm=12),
                                
                        ),
                dbc.Row(dbc.Col([
                                html.Div(
                                    [
                                        dbc.RadioItems(
                                            id="radios",
                                            className="btn-group",
                                            inputClassName="btn-check",
                                            labelClassName="btn btn-outline-primary",
                                            labelCheckedClassName="active",
                                            options=[
                                                {"label": "Температруа воздуха", "value": ['Tair',"Температура воздуха, С"]},
                                                {"label": "Влажность воздуха", "value": ['RH',"Относительная влажность воздуха, %"]},
                                                {"label": "Угол наклона", "value": ['Psi',"Угол дерева (относительно Земли), град"]},
                                                {"label": "Температура ствола", "value": ['Tref_0',"Температура ствола, С"]},
                                                {"label": "Состояние кроны", "value": ['NDVI',"Состояние кроны по вегетационному индексу NDVI"]},
                                                {"label": "Скорость сокотечения", "value": ['Flux',"Объем сокодвижения за последний час, л"]}
                                                
                                            ],
                                            value=1,
                                        ),
                                        html.Div(id="output"),
                                    ],
                                    className="radio-group",
                                )
                                ],
                                md = 12,
                                xl = {'offset':1, 'size':10 },
                                sm=12)
                        ),
                dbc.Row(dbc.Col([

                                ])
                        )                   
            ])
        )

#Callbacks for layout in pages
@dash.callback( Output("scatterplots", "srcDoc"), 
                [Input("radios", "value"),Input("DataDiv", "children")])
def display_graph(value,dataj):
    data = pd.read_json(dataj, orient='split')
    return(
     alt.Chart(data).mark_line().encode(
        x= alt.X('Timestamp', title = "Дата"),
        y= alt.X(value[0], title = value[1]),
     tooltip = value[0]).properties(
     width=960,
     height=360
     ).configure_axis(
                labelFontSize=20,
                titleFontSize=20
    ).interactive().to_html(embed_options=dict(formatLocale=ru_format, timeFormatLocale=ru_time_format, actions=False))
    )



''''    dbc.Container([
        html.Div(f"Cloud ID: {cloud_id}.TT ID: {tt_id}."),
        dash_table.DataTable(data.to_dict('records'), [{"name": i, "id": i} for i in data.columns])
    ])       '''

