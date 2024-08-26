type=$3

curl -k --tlsv1.2 -c ${type}_cookie.txt https://api-gateway.inpi.fr/services/uaa/api/authenticate
export TOKEN=`cat ${type}_cookie.txt | grep XSRF-TOKEN | awk '{print $7}'`
#echo "TOKEN = " $TOKEN

export ID1=`curl -k --tlsv1.2 -b ${type}_cookie.txt -c ${type}_cookie.txt 'https://api-gateway.inpi.fr/auth/login' \
-H 'Accept: application/json, text/plain, */*' -H "X-XSRF-TOKEN: $TOKEN" -H 'Content-Type: application/json' \
-H 'Connection: keep-alive' \
-H "Cookie: XSRF-TOKEN= $TOKEN"  --data '{"username":"'$1'","password":"'$2'","rememberMe":true}'`

export access_token=`cat ${type}_cookie.txt | grep access_token | awk '{print $7}'`;
#echo "access_token = " $access_token
export refresh_token=`cat ${type}_cookie.txt | grep refresh_token | awk '{print $7}'`;
#echo "refresh_token = " $refresh_token

QUERY=$4

if [ "$type" = "1_patent" ]; then
# Make the POST request with the adjusted Cookie header
curl -X POST -k --tlsv1.2 'https://api-gateway.inpi.fr/services/apidiffusion/api/brevets/search' \
-H 'accept: application/xml' \
-H 'Content-Type: application/json' \
-H "X-XSRF-TOKEN: $TOKEN" \
-H "Cookie: XSRF-TOKEN=$TOKEN; access_token=$access_token; session_token=$refresh_token" \
-d "{ \"collections\": [ \"FR\", \"WO\",\"EP\",\"CCP\"], \"query\": \"($QUERY)\", \"size\": 10000}" > ${type}_scraping_output.xml
else
curl -X POST -k --tlsv1.2 'https://api-gateway.inpi.fr/services/apidiffusion/api/marques/search' \
-H 'accept: application/xml' \
-H 'Content-Type: application/json' \
-H "X-XSRF-TOKEN: $TOKEN" \
-H "Cookie: XSRF-TOKEN=$TOKEN; access_token=$access_token; session_token=$refresh_token" \
-d "{ \"collections\": [ \"FR\", \"WO\",\"EU\"], \"query\": \"($QUERY)\", \"size\": 10000}" > ${type}_scraping_output.xml
fi