import pika
import telebot
from retry import retry


bot = telebot.TeleBot("5543143275:AAFRr-Qf4LB2XpY9q422EH50Da7Zm8Z6eZs", parse_mode=None)

@retry(pika.exceptions.AMQPConnectionError, delay=5, jitter=(1, 3))
def consume():
    connection = pika.BlockingConnection()
    channel = connection.channel()
    channel.basic_consume('test', on_message_callback)

    try:
        channel.start_consuming()
    # Don't recover connections closed by server
    except pika.exceptions.ConnectionClosedByBroker:
        pass


consume()