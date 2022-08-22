<?php return array(
    'root' => array(
        'name' => '__root__',
        'pretty_version' => '1.0.0+no-version-set',
        'version' => '1.0.0.0',
        'reference' => NULL,
        'type' => 'library',
        'install_path' => __DIR__ . '/../../',
        'aliases' => array(),
        'dev' => true,
    ),
    'versions' => array(
        '__root__' => array(
            'pretty_version' => '1.0.0+no-version-set',
            'version' => '1.0.0.0',
            'reference' => NULL,
            'type' => 'library',
            'install_path' => __DIR__ . '/../../',
            'aliases' => array(),
            'dev_requirement' => false,
        ),
        'php-amqplib/php-amqplib' => array(
            'pretty_version' => 'v3.2.0',
            'version' => '3.2.0.0',
            'reference' => '0bec5b392428e0ac3b3f34fbc4e02d706995833e',
            'type' => 'library',
            'install_path' => __DIR__ . '/../php-amqplib/php-amqplib',
            'aliases' => array(),
            'dev_requirement' => false,
        ),
        'phpseclib/phpseclib' => array(
            'pretty_version' => '2.0.37',
            'version' => '2.0.37.0',
            'reference' => 'c812fbb4d6b4d7f30235ab7298a12f09ba13b37c',
            'type' => 'library',
            'install_path' => __DIR__ . '/../phpseclib/phpseclib',
            'aliases' => array(),
            'dev_requirement' => false,
        ),
        'videlalvaro/php-amqplib' => array(
            'dev_requirement' => false,
            'replaced' => array(
                0 => 'v3.2.0',
            ),
        ),
    ),
);
