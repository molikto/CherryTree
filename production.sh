sudo lsof -i tcp:443 | sudo awk 'NR!=1 {print $2}' | sudo xargs kill
sudo rm -rf server
unzip server.zip
mv server-0.1.0-SNAPSHOT server
cd server
chmod a+x bin/server
sudo bin/server -Dconfig.resource=application.prod.conf -Dhttp.port=disabled -Dhttps.port=443 -Dplay.server.https.keyStore.path="/home/ec2-user/cherrytree.keystore" -Dplay.server.https.keyStore.password=fksde2#aj_i3_lkj32jkfdsali34j2kl -Dplay.crypto.secret=jkf,d,djs!ifj#al2314hkdji
# when generating keystore
# sudo bin/server  -Dconfig.resource=application.prod.conf -Dhttp.port=80 -Dhttps.port=443 -Dplay.server.https.keyStore.path=/home/ec2-user/cherrytree.keystore -Dplay.user.keyStore.path=conf/cherrytree.pvt.keystore -Dplay.server.https.keyStore.password=fksde2#aj_i3_lkj32jkfdsali34j2kl -Djdk.tls.ephemeralDHKeySize=2048 -Dplay.evolutions.db.default.autoApply=true -Dplay.crypto.secret=jkf,d,djs!ifj#al2314hkdji
