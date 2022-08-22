FROM php:8.1.9-cli

RUN apt-get update && apt-get install -y curl zip git libpq-dev \
    && rm -rf /var/lib/apt/lists/*

RUN docker-php-ext-install sockets pdo pdo_pgsql pgsql

COPY --from=composer /usr/bin/composer /usr/bin/composer
RUN mkdir app
RUN cd /app
RUN composer require php-amqplib/php-amqplib:3.2.0