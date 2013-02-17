In order to generate a self-signed certificate for testing, try the following:

    openssl genrsa -out key.pem 2048
    openssl req -new -key key.pem -out certificate.csr
    openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
