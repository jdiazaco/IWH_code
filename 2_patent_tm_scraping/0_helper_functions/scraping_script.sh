type=$3

node=$5
dstring="data/2_patent_tm_scraping/2_working/"
curl -k --tlsv1.2 -c ${dstring}cookie_${node}.txt https://api-gateway.inpi.fr/services/uaa/api/authenticate
export TOKEN=`cat ${dstring}cookie_${node}.txt | grep XSRF-TOKEN | awk '{print $7}'`

export ID1=`curl -k --tlsv1.2 -b ${dstring}cookie_${node}.txt -c ${dstring}cookie_${node}.txt 'https://api-gateway.inpi.fr/auth/login' \
-H 'Accept: application/json, text/plain, */*' -H "X-XSRF-TOKEN: $TOKEN" -H 'Content-Type: application/json' \
-H 'Connection: keep-alive' \
-H "Cookie: XSRF-TOKEN= $TOKEN"  --data '{"username":"'$1'","password":"'$2'","rememberMe":true}'`

export access_token=`cat ${dstring}cookie_${node}.txt | grep access_token | awk '{print $7}'`;
#echo "access_token = " $access_token
export refresh_token=`cat ${dstring}cookie_${node}.txt | grep refresh_token | awk '{print $7}'`;
#echo "refresh_token = " $refresh_token

QUERY=$4
if [ "$type" = "patent" ]; then
# Make the POST request with the adjusted Cookie header
curl -X POST -k --tlsv1.2 'https://api-gateway.inpi.fr/services/apidiffusion/api/brevets/search' \
-H 'accept: application/xml' \
-H 'Content-Type: application/json' \
-H "X-XSRF-TOKEN: $TOKEN" \
-H "Cookie: XSRF-TOKEN=$TOKEN; access_token=$access_token; session_token=$refresh_token" \
-d "{ \"collections\": [ \"FR\", \"WO\",\"EP\",\"CCP\"], \"query\": \"($QUERY)\", \"size\": 10000}" > ${dstring}scraping_output${node}.xml
else
  curl -X POST -k --tlsv1.2 'https://api-gateway.inpi.fr/services/apidiffusion/api/marques/search' \
-H 'accept: application/xml' \
-H 'Content-Type: application/json' \
-H "X-XSRF-TOKEN: $TOKEN" \
-H "Cookie: XSRF-TOKEN=$TOKEN; access_token=$access_token; session_token=$refresh_token" \
-d "{ \"collections\": [ \"FR\", \"WO\",\"EU\"], \"query\": \"($QUERY)\", \"size\": 10000}" > ${dstring}scraping_output${node}.xml
fi