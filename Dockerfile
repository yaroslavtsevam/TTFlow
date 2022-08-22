FROM php:8.1.9-apache
SHELL ["/bin/bash", "-c"]
COPY ./php_listner/* /var/www/html/
RUN php /var/www/html/composer-setup.php --install-dir=bin --filename=composer
RUN php bin/composer