<?php
require_once __DIR__.'/vendor/autoload.php';
use PhpAmqpLib\Connection\AMQPStreamConnection;
use PhpAmqpLib\Message\AMQPMessage;

/*putenv('TEST_AMQP_DEBUG=1');

define('HOST', getenv('TEST_RABBITMQ_HOST') ? getenv('TEST_RABBITMQ_HOST') : 'localhost');
define('PORT', getenv('TEST_RABBITMQ_PORT') ? getenv('TEST_RABBITMQ_PORT') : 5671);
define('USER', getenv('TEST_RABBITMQ_USER') ? getenv('TEST_RABBITMQ_USER') : 'cloud_producer');
define('PASS', getenv('TEST_RABBITMQ_PASS') ? getenv('TEST_RABBITMQ_PASS') : 'k1o2j3z4g5s6f');
define('VHOST', '/');
define('AMQP_DEBUG', 
        getenv('TEST_AMQP_DEBUG') !== false ? (bool)getenv('TEST_AMQP_DEBUG') : false);
*/

/*
date_default_timezone_set('Europe/Rome');

$val = $_GET['data'];
$dirname = $_GET['sn'];
$t = date("d.m.y H:i:s").";";

$data = $t.$val."\r\n";

if (!file_exists($dirname)) {
    mkdir($dirname, 0777, true);
}

$fp = fopen($dirname."/ttcloud.txt", "a+");
fwrite ($fp,$data);

fclose($fp);
*/

$db_row_types = array('4B','4C','4D','45','46','49');
$bot_row_types = array('4B','4C');


$val = $_GET['data'];
$dirname = $_GET['sn'];
$t = date("d.m.y H:i:s").";";

$data = $t.$val."\r\n";

$data_array = explode(';',$val);
$row_type = $data_array[2];

$exchange = 'TT_router';
$queue_DB = 'TT_DB';
$queue_BOT = 'TT_BOT';
$consumerTag = 'consumer';

$connection = new AMQPStreamConnection('rabbit', 5672, 'guest', 'guest');
$channel = $connection->channel();

/*
    The following code is the same both in the consumer and the producer.
    In this way we are sure we always have a queue to consume from and an
        exchange where to publish messages.
*/

/*
    name: $queue
    passive: false
    durable: true // the queue will survive server restarts
    exclusive: false // the queue can be accessed in other channels
    auto_delete: false //the queue won't be deleted once the channel is closed.
*/

$channel->queue_declare($queue_DB, false, true, false, false);
$channel->queue_declare($queue_BOT, false, true, false, false);

/*
    name: $exchange
    type: direct
    passive: false
    durable: true // the exchange will survive server restarts
    auto_delete: false //the exchange won't be deleted once the channel is closed.
*/

$channel->exchange_declare($exchange, 'direct', false, true, false);

foreach ($db_row_types as $db_row_type) {
    $channel->queue_bind($queue_DB, $exchange, $db_row_type);
}
foreach ($bot_row_types as $bot_row_type) {
    $channel->queue_bind($queue_BOT, $exchange, $bot_row_type);
}
$messageBody = $data;
$message = new AMQPMessage($messageBody, array('content_type' => 'text/plain', 
'delivery_mode' => AMQPMessage::DELIVERY_MODE_PERSISTENT));
$channel->basic_publish($message, $exchange, $row_type);

$channel->close();
$connection->close();
?>