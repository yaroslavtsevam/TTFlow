from airflow import DAG
from airflow.providers.http.operators.http import SimpleHttpOperator
from datetime import datetime, timedelta

#Default settings applied to all tasks
default_args = {
    'owner': 'airflow',
    'depends_on_past': False,
    'email_on_failure': False,
    'email_on_retry': False,
    'retries': 0,
}

#Instantiate DAG
with DAG('basic_cloud_scheduler_dag',
         start_date=datetime(2022, 1, 1),
         max_active_runs=1,
         schedule_interval='55 * * * *', # start task at the end of each hour
         default_args=default_args,
         catchup=False,
         template_searchpath='/usr/local/airflow/include' #include path to look for external files
         ) as dag:

        hourlycalc = SimpleHttpOperator(
            task_id='hourly_cloud_calc',
            http_conn_id='cloud_calc',
            endpoint='/C18AC097',
            method='GET',
            log_response=True
        )