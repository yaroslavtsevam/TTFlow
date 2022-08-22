FROM python:3.8-alpine

RUN pip install pika
RUN pip install python-telegram-bot --upgrade

# Without this setting, Python never prints anything out.
ENV PYTHONUNBUFFERED=1

# Actually install the application
WORKDIR /usr/src/app
# It's only a single file.  It has to be in the same directory as the
# Dockerfile, or a subdirectory, but not a parent or sibling.
#COPY consumer.py .

# When you just 'docker run publisher' with no command afterwards,
# default to this:
#CMD ["python", "/usr/src/app/consumer.py"]