import dash
from dash import html, dash_table
import psycopg2
from sqlalchemy import create_engine
import pandas as pd
import dash_bootstrap_components as dbc
 
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
    print(query)
    print(cloud_id)
    if(cloud_id != None):
        data = pd.read_sql(query, dbConnection)
        print(data)
        return  dash_table.DataTable(data.to_dict('records'), [{"name": i, "id": i} for i in data.columns])
    
''''    dbc.Container([
        html.Div(f"Cloud ID: {cloud_id}.TT ID: {tt_id}."),
        dash_table.DataTable(data.to_dict('records'), [{"name": i, "id": i} for i in data.columns])
    ])       '''

